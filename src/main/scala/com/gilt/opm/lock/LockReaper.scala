package com.gilt.opm.lock

import java.util.concurrent.{ThreadFactory, Executors, ScheduledFuture, ScheduledExecutorService}

import com.giltgroupe.util.RichLongForMeasurement._
import java.util.concurrent.TimeUnit.MILLISECONDS
import com.giltgroupe.util.Loggable

import com.mongodb.casbah.Imports._
import java.util.concurrent.atomic.AtomicInteger
import java.lang.Thread.UncaughtExceptionHandler

/**
 * When using OPM, it's important to setup a periodic job to reap stale locks in the database.
 *
 * Probably the best way to do this is to plug this into a leader election, so that only one instance
 * in a cluster is reaping stale locks.
 *
 * @author Eric Bowman
 * @since 1/9/13 8:47 AM
 */
trait LockReaper extends Loggable {

  /** Extenders must supply the locks collection. */
  def locks: MongoCollection

  /** Periodic Mongo queries will be run against this executor. */
  def executor: ScheduledExecutorService = defaultExecutor

  /** Run Mongo queries every periodMs milliseconds. */
  def periodMs: Long = (5 minutes) in milliseconds

  /** Delay this many milliseconds before running the first mongo query. */
  def initialDelayMs: Long = (30 seconds) in milliseconds

  /** How many milliseconds since the last access time must have passed before we reap a lock. */
  def maxLifespanMs: Long = periodMs

  /** Starts a periodic reap task to query mongo and delete stale locks against the executor. */
  def startReaper() {
    synchronized {
      require(future == None, "LockReaper already started")
      future = Some(
        executor.scheduleAtFixedRate(new Runnable {
          def run() {
            reap()
          }
        }, initialDelayMs, periodMs, MILLISECONDS)
      )
    }
  }

  def stopReaper() {
    synchronized {
      require(future.isDefined, "LockReaper not yet started")
      future.foreach(_.cancel(false))
      future = None
    }
  }

  private[this] var future: Option[ScheduledFuture[_]] = None

  /** If no executor is supplied, use this one. */
  private lazy val defaultExecutor = Executors.newScheduledThreadPool(1, new ThreadFactory {
    val count = new AtomicInteger(1)

    def newThread(r: Runnable) = {
      val thread = new Thread(r)
      thread.setDaemon(true)
      thread.setName("LockReaper-" + count.incrementAndGet())
      thread.setUncaughtExceptionHandler(new UncaughtExceptionHandler {
        def uncaughtException(t: Thread, e: Throwable) {
          error("Unhandled exception", e)
        }
      })
      thread
    }
  })

  /** Finds & deletes stale locks. */
  protected def reap() {
    import scala.util.control.Exception._
    import LockManager.{IdKey, TimestampKey}
    val now = System.currentTimeMillis()
    val query = TimestampKey $lt (now - maxLifespanMs)
    allCatch either locks.find(query).foreach {
      (dbObj: DBObject) =>
        val obj = wrapDBObj(dbObj)
        warn("Deleting expired lock %s last modified %s with lifetime %s (exceeded by %s ms)".format(
          obj.as[String](IdKey), obj.as[Long](TimestampKey), maxLifespanMs, (now - obj.as[Long](TimestampKey) - maxLifespanMs)))
        allCatch either locks.remove(obj) match {
          case Left(e) => error("Exception trying to remove lock %s".format(obj))
          case Right(_) => () // success
        }
    } match {
      case Left(e) => error("Unknown exception running periodic reaper task", e)
      case Right(_) => ()   // success
    }
  }
}


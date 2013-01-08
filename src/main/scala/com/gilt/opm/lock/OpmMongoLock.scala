package com.gilt.opm.lock

import com.mongodb.casbah._
import annotation.tailrec
import com.giltgroupe.util.Loggable
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._
import com.mongodb.WriteConcern
import com.mongodb.casbah.{WriteConcern => CWriteConcern}

/**
 * We need to take a short mutex when writing an OPM object to the database.
 * This class is used to implement that.
 *
 * @author Eric Bowman
 * @since 1/7/13 5:31 PM
 */

sealed trait Lock {
  def unlock()
}

private[opm] case class LockImpl(key: String, col: MongoCollection)(implicit writeConcern: WriteConcern) extends Lock {

  def unlock() {
    LockManager.unlock(this)
  }
}

private object LockManager extends Loggable {
  val pendingLocks = new ConcurrentHashMap[Lock, Lock].asScala
  sys.addShutdownHook {
    pendingLocks.values.foreach(_.unlock())
  }

  def unlock(lock: LockImpl)(implicit writeConcern: WriteConcern) {
    debug("Releasing lock for %s".format(lock))
    import scala.util.control.Exception._
    allCatch either lock.col.remove(MongoDBObject("_id" -> lock.key)) match {
      case Left(e) => error("Exception removing %s during cleanup".format(lock), e)
      case Right(_) => ()  // success
    }
    pendingLocks.remove(lock)
  }
}

trait LockManager extends Loggable {

  def locks: MongoCollection
  def waitMs: Long = 100l
  def sleepMs: Long = 50l
  def writeConcern = CWriteConcern.valueOf("SAFE")

  implicit lazy val _writeConcern = writeConcern

  import LockManager._

  final def lock(key: String): Lock = {
    val now = System.currentTimeMillis()
    debug("Trying to acquire lock %s from %s with timestamp %s".format(key, locks, now))
    @tailrec
    def recurse(): Lock = {
      import scala.util.control.Exception._
      allCatch either locks.insert(MongoDBObject("_id" -> key, "ts" -> now)) match {
        case Right(_) =>
          val lock = LockImpl(key, locks)
          pendingLocks += (lock -> lock)
          lock
        case Left(e) =>
          if (System.currentTimeMillis() - now > waitMs) {
            warn("Timed out trying to acquire lock \"%s\" (waitMs = %s)".format(key, waitMs))
            sys.error("Timed out waiting for lock on object %s".format(key))
          } else {
            Thread.sleep(sleepMs)
            recurse()
          }
      }
    }
    recurse()
  }
}


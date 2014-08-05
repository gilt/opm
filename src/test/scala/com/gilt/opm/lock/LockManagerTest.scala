package com.gilt.opm.lock

import org.scalatest.FunSuite
import org.scalatest.Matchers
import java.util.concurrent.{CountDownLatch, Executors}
import java.util.concurrent.atomic.AtomicInteger
import com.gilt.opm.CollectionHelper

/**
 * Tests the lock contract.
 *
 * @author Eric Bowman
 * @since 1/8/13 7:44 AM
 */
class LockManagerTest extends FunSuite with Matchers with LockManager with CollectionHelper {

  override val collectionName = "opm_LockManagerTest"

  // If the lock is taken, wait up to this long to retrieve it
  override val waitMs = 100L

  // If the lock is taken, sleep this long before trying again
  override val sleepMs = 50L

  test("taking a lock, then releasing it then taking it again should succeed") {
    val lock1 = lock("test1")
    lock1.unlock()
    val lock2 = this.lock("test1")
    lock2.unlock()
  }

  test("taking a lock, then trying to take the same lock again, should fail within 200ms") {
    val aLock = this.lock("test2")
    try {
      val start = System.currentTimeMillis()
      a [RuntimeException] should be thrownBy lock("test2")
      val done = System.currentTimeMillis()
      (done - start) should be >= 100L
    } finally {
      aLock.unlock()
    }
  }

  test("under concurrency, no more than one thread should ever have the lock") {
    val threads = 8 // contending threads
    val successCount = 5 // # times each thread must obtain the lock
    val lockCount = new AtomicInteger() // used to confirm only 1 thread ever has the lock
    @volatile var fail = false // set to true if more than one thread takes the lock

    val executor = Executors.newFixedThreadPool(threads)
    val latch = new CountDownLatch(threads)
    import scala.util.control.Exception._
    for (t <- 1 to threads) {
      val runnable = new Runnable {
        override def run() {
          var remaining = successCount
          while (remaining > 0) {
            allCatch opt lock("concurrency test") match {
              case None => ()
              case Some(lock) =>
                allCatch either {
                  remaining -= 1
                  val locked = lockCount.incrementAndGet()
                  fail = locked != 1
                  Thread.sleep(10)
                  lockCount.decrementAndGet()
                  lock.unlock()
                } match {
                  case Left(e) => e.printStackTrace()
                  case Right(_) => ()
                }
            }
          }
          latch.countDown()
        }
      }
      executor.submit(runnable)
    }
    latch.await()
    executor.shutdown()
    assert(fail === false)
  }

}

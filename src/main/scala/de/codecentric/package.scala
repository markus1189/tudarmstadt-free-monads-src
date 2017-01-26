package de

import com.google.common.util.concurrent.{FutureCallback, Futures, ListenableFuture}

import scala.concurrent.{Future, Promise}

package object codecentric {
  implicit class ListenableFutureConverter[A](val future: ListenableFuture[A]) extends AnyVal {
    def toFuture: Future[A] = {
      val p = Promise[A]()

      Futures.addCallback(future,
        new FutureCallback[A] {
          def onSuccess(r: A): Unit = {
            val _ = p success r
          }

          def onFailure(t: Throwable): Unit = {
            val _ = p failure t
          }
        })

      p.future
    }
  }
}

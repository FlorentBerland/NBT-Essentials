package util

import java.util.Date

import scala.reflect.ClassTag

object Time {

  def apply[A](p: => A)(callback: => Long => Unit)(implicit ct: ClassTag[A]): A = {
    val start = new Date().getTime
    val result = p
    callback(new Date().getTime - start)
    result
  }

}

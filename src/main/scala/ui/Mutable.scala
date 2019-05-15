package ui

import scala.collection.mutable.ListBuffer

class Mutable[T](initialValue: T){

  private var value: T = initialValue
  private val callbacks = new ListBuffer[T => Unit]

  def apply(): T = value
  def update(newValue: T): Unit = {
    value = newValue
    callbacks.foreach(_(newValue))
  }
  def refresh(): Unit = callbacks.foreach(_(value))

  def addListener(callback: T => Unit): Unit = callbacks += callback
  def removeListener(callback: T => Unit): Unit = callbacks -= callback

}
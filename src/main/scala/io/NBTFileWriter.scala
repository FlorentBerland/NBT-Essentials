package io

import java.io.FileOutputStream
import java.nio.ByteBuffer
import java.util.zip.GZIPOutputStream

import model.nbt._

import scala.util.{Failure, Success, Try}

object NBTFileWriter {

  def write(fileName: String, nbtTree: NBT): Try[Unit] = {
    var gzip: GZIPOutputStream = null
    try {
      gzip = new GZIPOutputStream(new FileOutputStream(fileName))
      saveNBTTree(gzip, nbtTree)
      Success()
    } catch {
      case e: Exception => Failure(e)
    } finally {
      if(gzip != null) gzip.close()
    }
  }

  private def saveNBTTree(output: GZIPOutputStream, nbtTree: NBT): Unit = {
    def writeByte(value: Byte): Unit = output.write(Array[Byte](value))
    def writeShort(value: Short): Unit = output.write(ByteBuffer.allocate(2).putShort(value).array())
    def writeInt(value: Int): Unit = output.write(ByteBuffer.allocate(4).putInt(value).array())
    def writeLong(value: Long): Unit = output.write(ByteBuffer.allocate(8).putLong(value).array())
    def writeFloat(value: Float): Unit = output.write(ByteBuffer.allocate(4).putFloat(value).array())
    def writeDouble(value: Double): Unit = output.write(ByteBuffer.allocate(8).putDouble(value).array())
    def writeByteArray(value: Array[Byte]): Unit = output.write(value)
    def writeString(value: String): Unit = {
      writeShort(value.length.toShort)
      output.write(value.getBytes)
    }
    def tagId(tag: TAG): Byte = {
      tag match {
        case _: TAG_End => 0
        case _: TAG_Byte => 1
        case _: TAG_Short => 2
        case _: TAG_Int => 3
        case _: TAG_Long => 4
        case _: TAG_Float => 5
        case _: TAG_Double => 6
        case _: TAG_Byte_Array => 7
        case _: TAG_String => 8
        case _: TAG_List => 9
        case _: TAG_Compound => 10
        case _: TAG_Int_Array => 11
        case _: TAG_Long_Array => 12
      }
    }
    def writeTagContent(tag: TAG): Unit = {
      tag match {
        case _: TAG_End =>
        case t: TAG_Byte =>
          writeByte(t.value)
        case t: TAG_Short =>
          writeShort(t.value)
        case t: TAG_Int =>
          writeInt(t.value)
        case t: TAG_Long =>
          writeLong(t.value)
        case t: TAG_Float =>
          writeFloat(t.value)
        case t: TAG_Double =>
          writeDouble(t.value)
        case t: TAG_Byte_Array =>
          writeInt(t.bytes.length)
          writeByteArray(t.bytes)
        case t: TAG_String =>
          writeString(t.value)
        case t: TAG_List =>
          writeByte(t.tagId)
          writeInt(t.elems.size)
          t.elems.foreach(writeTagContent)
        case t: TAG_Compound =>
          t.elems.foreach(kv => {
            writeByte(tagId(kv._2))
            writeString(kv._1)
            writeTagContent(kv._2)
          })
          writeByte(0)
        case t: TAG_Int_Array =>
          writeInt(t.ints.length)
          t.ints.foreach(writeInt)
        case t: TAG_Long_Array =>
          writeInt(t.longs.length)
          t.longs.foreach(writeLong)
      }
    }
    writeByte(tagId(nbtTree.tree))
    writeString(nbtTree.name)
    writeTagContent(nbtTree.tree)
  }
}

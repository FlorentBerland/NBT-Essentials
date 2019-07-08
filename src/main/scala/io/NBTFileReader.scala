package io

import java.io.FileInputStream
import java.nio.ByteBuffer
import java.util.zip.GZIPInputStream

import model.nbt._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object NBTFileReader {

  def read(fileName: String): Try[NBT] = {
    var gzip: GZIPInputStream = null
    try {
      gzip = new GZIPInputStream(new FileInputStream(fileName))
      val nbtTree = createNBTTree(gzip)
      Success(nbtTree)
    } catch {
      case e: Exception => Failure(e)
    } finally {
      if(gzip != null) gzip.close()
    }
  }

  private def createNBTTree(input: GZIPInputStream): NBT = {
    def readByte(): Byte = input.readNBytes(1)(0)
    def readShort(): Short = ByteBuffer.wrap(input.readNBytes(2)).getShort()
    def readInt(): Int = ByteBuffer.wrap(input.readNBytes(4)).getInt()
    def readLong(): Long = ByteBuffer.wrap(input.readNBytes(8)).getLong()
    def readFloat(): Float = ByteBuffer.wrap(input.readNBytes(4)).getFloat()
    def readDouble(): Double = ByteBuffer.wrap(input.readNBytes(8)).getDouble()
    def readByteArray(size: Int): Array[Byte] = input.readNBytes(size)
    def readString(): String = {
      val stringSize = readShort()
      new String(input.readNBytes(stringSize))
    }
    def readTagContent(tagType: Byte): TAG = {
      tagType match {
        case 0 => // TAG_End
          TAG_End()
        case 1 => // TAG_Byte
          TAG_Byte(readByte())
        case 2 => // TAG_Short
          TAG_Short(readShort())
        case 3 => // TAG_Int
          TAG_Int(readInt())
        case 4 => // TAG_Long
          TAG_Long(readLong())
        case 5 => // TAG_Float
          TAG_Float(readFloat())
        case 6 => // TAG_Double
          TAG_Double(readDouble())
        case 7 => // TAG_Byte_Array
          val arraySize = readInt()
          TAG_Byte_Array(readByteArray(arraySize))
        case 8 => // TAG_String
          TAG_String(readString())
        case 9 => // TAG_List
          val tagType = readByte()
          val size = readInt()
          val elems = new ListBuffer[TAG]()
          for(_ <- 0 until size) elems += readTagContent(tagType)
          TAG_List(tagType, elems.toList)
        case 10 => // TAG_Compound
          val elems = new mutable.HashMap[String, TAG]()
          var elemType = readByte()
          while(elemType != 0){
            val tagName = readString()
            elems += tagName -> readTagContent(elemType)
            elemType = readByte()
          }
          TAG_Compound(elems.toMap)
        case 11 => // TAG_Int_Array
          val size = readInt()
          val elems = Array.ofDim[Int](size)
          for(i <- 0 until size) elems(i) = readInt()
          TAG_Int_Array(elems)
        case 12 => // TAG_Long_Array
          val size = readInt()
          val elems = Array.ofDim[Long](size)
          for(i <- 0 until size) elems(i) = readLong()
          TAG_Long_Array(elems)
      }
    }
    val tagType = readByte()
    val tagName = readString()
    new NBT(tagName, readTagContent(tagType))
  }

  def printNBT(nbtTree: NBT): Unit = {
    def printNBTImpl(NBTTree: TAG, name: String, indentation: String): Unit = {
      NBTTree match {
        case _: TAG_End => println(indentation + name + ": " + "EMPTY")
        case t: TAG_Byte => println(indentation + name + ": "  + t.value)
        case t: TAG_Short => println(indentation + name + ": "  + t.value)
        case t: TAG_Int => println(indentation + name + ": "  + t.value)
        case t: TAG_Long => println(indentation + name + ": "  + t.value)
        case t: TAG_Float => println(indentation + name + ": "  + t.value)
        case t: TAG_Double => println(indentation + name + ": "  + t.value)
        case t: TAG_Byte_Array => println(indentation + name + ": "  + "Array[Byte](" + t.bytes.length + " bytes)")
        case t: TAG_String => println(indentation + name + ": "  + t.value)
        case t: TAG_List =>
          if(t.elems.isEmpty) println(indentation + name + ": (type: " + t.tagId + ")[]")
          else {
            println(indentation + name + ": (type: " + t.tagId + ")[")
            t.elems.foreach(e => {
              printNBTImpl(e, "", indentation + "  ")
            })
            println(indentation + "]")
          }
        case t: TAG_Compound =>
          if(t.elems.isEmpty) println(indentation + name + ": {}")
          else {
            println(indentation + name + ": {")
            t.elems.foreach(kv => {
              printNBTImpl(kv._2, kv._1, indentation + "  ")
            })
            println(indentation + "}")
          }
        case t: TAG_Int_Array => println(indentation + name + ": Array[Int](" + t.ints.length + " ints)")
        case t: TAG_Long_Array => println(indentation + name + ": Array[Long](" + t.longs.length + " longs)")
      }
    }
    printNBTImpl(nbtTree.tree, nbtTree.name, "")
  }

}

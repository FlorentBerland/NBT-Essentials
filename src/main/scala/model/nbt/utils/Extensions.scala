package model.nbt.utils

import model.nbt._
import scala.language.postfixOps
import scala.reflect.ClassTag

object Extensions {

  implicit class TAGFind(tag: TAG){
    def $(query: String): TAGLookup = {
      if(query.isEmpty) Some(tag)
      else {
        val (token, nextQuery) = nextToken(query)
        token match {
          case Left(field) => tag match {
            case tagCompound: TAG_Compound =>
              if(tagCompound.elems.contains(field)) tagCompound(field) $ nextQuery
              else None
            case _ => None
          }
          case Right(index) => tag match {
            case tagList: TAG_List =>
              if(tagList.elems.isDefinedAt(index)) tagList(index) $ nextQuery
              else None
            case _ => None
          }
        }
      }
    }
    def find(query: String): Option[TAG] = this $ query
    def findByte(query: String): Option[TAG_Byte] = findGen[TAG_Byte](query)
    def findShort(query: String): Option[TAG_Short] = findGen[TAG_Short](query)
    def findInt(query: String): Option[TAG_Int] = findGen[TAG_Int](query)
    def findLong(query: String): Option[TAG_Long] = findGen[TAG_Long](query)
    def findFloat(query: String): Option[TAG_Float] = findGen[TAG_Float](query)
    def findDouble(query: String): Option[TAG_Double] = findGen[TAG_Double](query)
    def findByteArray(query: String): Option[TAG_Byte_Array] = findGen[TAG_Byte_Array](query)
    def findString(query: String): Option[TAG_String] = findGen[TAG_String](query)
    def findList(query: String): Option[TAG_List] = findGen[TAG_List](query)
    def findCompound(query: String): Option[TAG_Compound] = findGen[TAG_Compound](query)
    def findIntArray(query: String): Option[TAG_Int_Array] = findGen[TAG_Int_Array](query)
    def findLongArray(query: String): Option[TAG_Long_Array] = findGen[TAG_Long_Array](query)

    private def findGen[T <: TAG](query: String)(implicit tg: ClassTag[T]): Option[T] = this $ query match {
      case Some(t: T) => Some(t)
      case _ => None
    }
  }

  type TAGLookup = Option[TAG]
  implicit class Lookups(lookup: TAGLookup){
    def isDefined: Boolean = lookup.nonEmpty
    def is[T <: TAG](implicit tg: ClassTag[T]): Boolean = lookup match {
      case Some(_: T) => true
      case _ => false
    }
    def as[T <: TAG](implicit  tg: ClassTag[T]): Option[T] = lookup match {
      case Some(t: T) => Some(t)
      case _ => None
    }
  }

  private def nextToken(query: String): (Either[String, Int], String) = {
    val indexRegex = """^(\[\d+\]).*""".r
    val fieldRegex = """^([^\[\]\.]+).*""".r
    query match {
      case indexRegex(index, _*) => // e.g. "[54655]"
        Right(index.tail.init.toInt) -> query.replaceFirst("""\[\d+\]""", "")
      case fieldRegex(field, _*) => // e.g. "myfield"
        Left(field) -> query.replaceFirst("""[^\[\]\.]+""", "").replaceFirst("""^\.""", "")
      case _ => throw new IllegalArgumentException("Invalid query string: '" + query + "'")
    }
  }
}

package model

import model.Block.Block

case class Material(block: Block, data: Byte)

object Material {

  val default = Material(Block.AIR, 0)

  def parseMaterial(rawName: String): Option[Material] = {
    val (blockName, data): (String, Byte) = {
      val materialRegex = "(.*)_([0-9]|(?:1[0-5]))".r
      rawName match {
        case materialRegex(name, stringData) => (name, stringData.toByte)
        case name => (name, 0)
      }
    }
    try {
      Some(Material(Block.withName(blockName), data))
    } catch {
      case _: Exception => None
    }
  }

}
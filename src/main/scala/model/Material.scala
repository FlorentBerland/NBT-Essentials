package model

import model.Block.Block

case class Material(name: String, block: Block, data: Byte)

object Material {

  val default = Material("Air", Block.AIR, 0)

  def parseMaterial(rawName: String): Option[Material] = {
    val (blockName, data): (String, Byte) = {
      val materialRegex = "(.*)_([0-9]|(?:1[0-5]))".r
      rawName match {
        case materialRegex(name, stringData) => (name, stringData.toByte)
        case name => (name, 0)
      }
    }

    val matName = blockName.head + blockName.tail.replace("_", " ").toLowerCase

    try {
      Some(Material(matName, Block.withName(blockName), data))
    } catch {
      case _: Exception => None
    }
  }

}
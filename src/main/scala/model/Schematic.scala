package model

import model.nbt._
import model.nbt.utils.Extensions._

import scala.util.{Failure, Success, Try}

case class Schematic(tree: TAG_Compound) {

  private val blocks = tree("Blocks").asInstanceOf[TAG_Byte_Array].bytes
  private val data = tree("Data").asInstanceOf[TAG_Byte_Array].bytes

  val width: Int = tree("Width").asInstanceOf[TAG_Short].value
  val height: Int = tree("Height").asInstanceOf[TAG_Short].value
  val length: Int = tree("Length").asInstanceOf[TAG_Short].value

  def apply(x: Int, y: Int, z: Int): Block.Value = getBlock(x, y, z)

  def update(x: Int, y: Int, z: Int, block: Block.Value): Unit = setBlock(x, y, z, block.id.toByte)

  def getBlock(x: Int, y: Int, z: Int): Block.Value = {
    Block(blocks((y * length + z) * width + x))
  }

  def getData(x: Int, y: Int, z: Int): Byte = {
    data((y * length + z) * width + x)
  }

  def setBlock(x: Int, y: Int, z: Int, value: Byte): Unit = {
    blocks((y * length + z) * width + x) = value
  }

  def setData(x: Int, y: Int, z: Int, blockData: Byte): Unit = {
    data((y * length + z) * width + x) = blockData
  }

  def toNBT: NBT = NBT("Schematic", tree)

}

object Schematic {

  def apply(nbtTree: NBT): Try[Schematic] = {
    try {
      assert(nbtTree.tree.isInstanceOf[TAG_Compound])
      val compound = nbtTree.tree.asInstanceOf[TAG_Compound]
      assert(compound.findShort("Width").nonEmpty)
      assert(compound.findShort("Height").nonEmpty)
      assert(compound.findShort("Length").nonEmpty)
      assert(compound.findString("Materials").exists(_.value == "Alpha"))
      assert(compound.findList("TileEntities").nonEmpty)
      val width = compound("Width").asInstanceOf[TAG_Short].value
      val height = compound("Height").asInstanceOf[TAG_Short].value
      val length = compound("Length").asInstanceOf[TAG_Short].value
      assert(compound.findByteArray("Blocks").exists(_.bytes.length == width * height * length))
      assert(compound.findByteArray("Data").exists(_.bytes.length == width * height * length))
      Success(Schematic(nbtTree.tree.asInstanceOf[TAG_Compound]))
    } catch {
      case e: AssertionError => Failure(e)
    }
  }

  def apply(width: Int, height: Int, length: Int): Schematic = {
    Schematic(TAG_Compound(Map(
      "Width" -> TAG_Short(width.toShort),
      "Height" -> TAG_Short(height.toShort),
      "Length" -> TAG_Short(length.toShort),
      "Materials" -> TAG_String("Alpha"),
      "Blocks" -> TAG_Byte_Array(Array.ofDim(width * height * length)),
      "Data" -> TAG_Byte_Array(Array.ofDim(width * height * length)),
      "TileEntities" -> TAG_List()
    )))
  }

  def unapply(arg: Schematic): Option[NBT] = Some(NBT("Schematic", arg.tree))

}
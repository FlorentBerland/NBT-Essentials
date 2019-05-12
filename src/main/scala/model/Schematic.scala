package model

import model.nbt._
import model.nbt.utils.Extensions._

import scala.util.{Failure, Success, Try}

/**
  * Represents the Schematic data structure and contains basic operations such as block editing.
  *
  * @param tree The root of a schematic file. It has to contain all the required fields.
  */
case class Schematic private(tree: TAG_Compound) {

  private val blocks = tree("Blocks").asInstanceOf[TAG_Byte_Array].bytes
  private val data = tree("Data").asInstanceOf[TAG_Byte_Array].bytes

  /**
    * The size of the schematic along the X axis
    */
  val width: Int = tree("Width").asInstanceOf[TAG_Short].value

  /**
    * The size of the schematic along the Y axis
    */
  val height: Int = tree("Height").asInstanceOf[TAG_Short].value

  /**
    * The size of the schematic along the Z axis
    */
  val length: Int = tree("Length").asInstanceOf[TAG_Short].value

  /**
    * Returns the block at the given coordinates.
    *
    * @param x The x coordinate, has to be less than the width
    * @param y The y coordinate, has to be less than the height
    * @param z The z coordinate, has to be less than the length
    * @return The block at the given value
    * @throws IndexOutOfBoundsException An exception is thrown if one of the coordinates is out of bounds
    */
  @throws[IndexOutOfBoundsException]
  def apply(x: Int, y: Int, z: Int): Block.Value = getBlock(x, y, z)

  /**
    * Change the block at the given coordinates.
    *
    * @param x The x coordinate, has to be less than the width
    * @param y The y coordinate, has to be less than the height
    * @param z The z coordinate, has to be less than the length
    * @param block The block to set
    * @throws IndexOutOfBoundsException An exception is thrown if one of the coordinates is out of bounds
    */
  @throws[IndexOutOfBoundsException]
  def update(x: Int, y: Int, z: Int, block: Block.Value): Unit = setBlock(x, y, z, block)

  /**
    * Returns the block at the given coordinates.
    *
    * @param x The x coordinate, has to be less than the width
    * @param y The y coordinate, has to be less than the height
    * @param z The z coordinate, has to be less than the length
    * @return The block at the given value
    * @throws IndexOutOfBoundsException An exception is thrown if one of the coordinates is out of bounds
    */
  @throws[IndexOutOfBoundsException]
  def getBlock(x: Int, y: Int, z: Int): Block.Value = {
    assertCoordinatesAreGood(x, y, z)
    Block(blocks((y * length + z) * width + x))
  }

  /**
    * Change the block at the given coordinates.
    *
    * @param x The x coordinate, has to be less than the width
    * @param y The y coordinate, has to be less than the height
    * @param z The z coordinate, has to be less than the length
    * @param block The block to set
    * @throws IndexOutOfBoundsException An exception is thrown if one of the coordinates is out of bounds
    */
  @throws[IndexOutOfBoundsException]
  def setBlock(x: Int, y: Int, z: Int, block: Block.Value): Unit = {
    assertCoordinatesAreGood(x, y, z)
    blocks((y * length + z) * width + x) = block.id.toByte
  }

  /**
    * Returns the data at the given coordinates. The data is the additional info about the block,
    * e.g. the axis for a log, the stage of a growing crop or the color of a wool block.
    *
    * @param x The x coordinate, has to be less than the width
    * @param y The y coordinate, has to be less than the height
    * @param z The z coordinate, has to be less than the length
    * @return The data to set to this block
    * @throws IndexOutOfBoundsException An exception is thrown if one of the coordinates is out of bounds
    */
  @throws[IndexOutOfBoundsException]
  def getData(x: Int, y: Int, z: Int): Byte = {
    assertCoordinatesAreGood(x, y, z)
    data((y * length + z) * width + x)
  }

  /**
    * Change the data at the given coordinates. The data is the additional info about the block,
    * e.g. the axis for a log, the stage of a growing crop or the color of a wool block.
    *
    * @param x The x coordinate, has to be less than the width
    * @param y The y coordinate, has to be less than the height
    * @param z The z coordinate, has to be less than the length
    * @param blockData The data to set
    * @throws IndexOutOfBoundsException An exception is thrown if one of the coordinates is out of bounds
    */
  @throws[IndexOutOfBoundsException]
  def setData(x: Int, y: Int, z: Int, blockData: Byte): Unit = {
    assertCoordinatesAreGood(x, y, z)
    data((y * length + z) * width + x) = blockData
  }

  /**
    * @return A NBT data structure that represents the schematic
    */
  def toNBT: NBT = NBT("Schematic", tree)

  /**
    * Tests whether the given coordinates can be safely used to access a block in the blocks array.
    *
    * @param x The x coordinate to check
    * @param y The x coordinate to check
    * @param z The x coordinate to check
    * @throws IllegalArgumentException If one of the coordinates is out of bounds.
    */
  @throws[IllegalArgumentException]
  private def assertCoordinatesAreGood(x: Int, y: Int, z: Int): Unit = {
    if(x >= width) throw new IndexOutOfBoundsException("x must be less than width (" + x + ")")
    else if(y >= height) throw new IndexOutOfBoundsException("y must be less than height (" + y + ")")
    else if(z >= length) throw new IndexOutOfBoundsException("z must be less than length (" + z + ")")
    else if(x < 0) throw new IndexOutOfBoundsException("x must be greater or equal 0 (" + x + ")")
    else if(y < 0) throw new IndexOutOfBoundsException("y must be greater or equal 0 (" + y + ")")
    else if(z < 0) throw new IndexOutOfBoundsException("z must be greater or equal 0 (" + z + ")")
  }

}

object Schematic {

  /**
    * Creates a new schematic using the given NBT tree. All the required fields have to be well defined
    * according to the Schematic specifications.
    *
    * @param nbtTree The NBT tree to try to parse
    * @return Either the schematic if all the required fields were well defined or an exception if the
    *         process failed.
    */
  def apply(nbtTree: NBT): Try[Schematic] = {
    try {
      assert(nbtTree.name == "Schematic")
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
package model

import model.nbt._
import model.nbt.utils.Extensions._

import scala.util._

/**
  * Represents the Schematic data structure and contains basic operations such as block editing.
  */
class Schematic private(val width: Int, val height: Int, val length: Int, blocks: Array[Material]) {

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
  def apply(x: Int, y: Int, z: Int): Material = getBlock(x, y, z)

  /**
    * Change the block at the given coordinates.
    *
    * @param x The x coordinate, has to be less than the width
    * @param y The y coordinate, has to be less than the height
    * @param z The z coordinate, has to be less than the length
    * @param material The material to set
    * @throws IndexOutOfBoundsException An exception is thrown if one of the coordinates is out of bounds
    */
  @throws[IndexOutOfBoundsException]
  def update(x: Int, y: Int, z: Int, material: Material): Unit = setBlock(x, y, z, material)

  /**
    * Returns the block at the given coordinates.
    *
    * @param x The x coordinate, has to be less than the width
    * @param y The y coordinate, has to be less than the height
    * @param z The z coordinate, has to be less than the length
    * @return The block material at the given value
    * @throws IndexOutOfBoundsException An exception is thrown if one of the coordinates is out of bounds
    */
  @throws[IndexOutOfBoundsException]
  def getBlock(x: Int, y: Int, z: Int): Material = {
    assertCoordinatesAreInBounds(x, y, z)
    blocks((y * length + z) * width + x)
  }

  /**
    * Change the block at the given coordinates.
    *
    * @param x The x coordinate, has to be less than the width
    * @param y The y coordinate, has to be less than the height
    * @param z The z coordinate, has to be less than the length
    * @param material The material to set
    * @throws IndexOutOfBoundsException An exception is thrown if one of the coordinates is out of bounds
    */
  @throws[IndexOutOfBoundsException]
  def setBlock(x: Int, y: Int, z: Int, material: Material): Unit = {
    assertCoordinatesAreInBounds(x, y, z)
    blocks((y * length + z) * width + x) = material
  }

  /**
    * @return A NBT data structure that represents the schematic
    */
  def toNBT: NBT = new NBT(
    "Schematic",
    TAG_Compound(
      "Width" -> TAG_Short(width.toShort),
      "Height" -> TAG_Short(height.toShort),
      "Length" -> TAG_Short(length.toShort),
      "Materials" -> TAG_String("Alpha"),
      "TileEntities" -> TAG_List(),
      "Blocks" -> TAG_Byte_Array(blocks.map(_.id.toByte)),
      "Data" -> TAG_Byte_Array(blocks.map(_.variant.toByte))
    )
  )

  /**
    * Tests whether the given coordinates can be safely used to access a block in the blocks array.
    *
    * @param x The x coordinate to check
    * @param y The x coordinate to check
    * @param z The x coordinate to check
    * @throws IllegalArgumentException If one of the coordinates is out of bounds.
    */
  @throws[IllegalArgumentException]
  private def assertCoordinatesAreInBounds(x: Int, y: Int, z: Int): Unit = {
    if(x >= width) throw new IndexOutOfBoundsException("x is greater than width (" + x + " >= " + width + ")")
    else if(y >= height) throw new IndexOutOfBoundsException("y is greater than height (" + y + " >= " + height + ")")
    else if(z >= length) throw new IndexOutOfBoundsException("z is greater than length (" + z + " >= " + length + ")")
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
  def fromNbt(nbtTree: NBT): Try[Schematic] = Try {
    if(nbtTree.name != "Schematic")
      throw new IllegalArgumentException("Incorrect structure name. Expecting: \"Schematic\", found: " + nbtTree.name)
    if(!nbtTree.tree.isInstanceOf[TAG_Compound])
      throw new IllegalArgumentException("Incorrect main tag type. Expecting TAG_Compound, found: " + nbtTree.tree.getClass.getSimpleName)
    val compound = nbtTree.tree.asInstanceOf[TAG_Compound]
    if(compound.findShort("Width").isEmpty)
      throw new IllegalArgumentException("Attribute \"Width\" is missing or is not a TAG_Short")
    if(compound.findShort("Height").isEmpty)
      throw new IllegalArgumentException("Attribute \"Height\" is missing or is not a TAG_Short")
    if(compound.findShort("Length").isEmpty)
      throw new IllegalArgumentException("Attribute \"Length\" is missing or is not a TAG_Short")
    if(!compound.findString("Materials").exists(_.value == "Alpha"))
      throw new IllegalArgumentException("Attribute \"Materials\" is not a TAG_String or does not equal \"Alpha\"")
    if(compound.findList("TileEntities").isEmpty)
      throw new IllegalArgumentException("Attribute \"TileEntities\" is missing or is not a TAG_LIST")
    val width = compound("Width").asInstanceOf[TAG_Short].value
    val height = compound("Height").asInstanceOf[TAG_Short].value
    val length = compound("Length").asInstanceOf[TAG_Short].value
    if(!compound.findByteArray("Blocks").exists(_.bytes.length == width * height * length))
      throw new IllegalArgumentException("Attribute \"Blocks\" is missing, is not a TAG_Byte_Array or has an incorrect size")
    if(!compound.findByteArray("Data").exists(_.bytes.length == width * height * length))
      throw new IllegalArgumentException("Attribute \"Data\" is missing, is not a TAG_Byte_Array or has an incorrect size")

    val blockIds = nbtTree.tree.findByteArray("Blocks").get.bytes
    val blocksData = nbtTree.tree.findByteArray("Data").get.bytes

    val materials = (blockIds zip blocksData).map(idAndData =>
      Material.all
        .find(mat => mat._2.id == idAndData._1 && mat._2.variant == idAndData._2)
        .fold(Material.Air)(_._2)
    )

    new Schematic(width, height, length, materials)
  }

  def create(width: Int, height: Int, length: Int): Schematic = {
    new Schematic(width, height, length, Array.fill(width * height * length)(Material.Air))
  }

}
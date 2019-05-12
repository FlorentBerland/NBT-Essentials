package model

/**
  * This enumeration contains all the block that can be
  * converted in a schematic. The value is the id of the
  * block in MC.
  */
object Block extends Enumeration {
  val AIR = Value(0)
  val STONE = Value(1)
  val GRASS = Value(2)
  val DIRT = Value(3)
  val COBBLESTONE = Value(4)
  val PLANKS = Value(5)
  val SAPLING = Value(6)
  val BEDROCK = Value(7)
  val SAND = Value(12)
  val GRAVEL = Value(13)
  val GOLD_ORE = Value(14)
  val IRON_ORE = Value(15)
  val COAL_ORE = Value(16)
  val LOG = Value(17)
  val LEAVES = Value(18)
  val SPONGE = Value(19)
  val GLASS = Value(20)
  val LAPIS_ORE = Value(21)
  val LAPIS_BLOCK = Value(22)
  val SANDSTONE = Value(24)
  val WOOL = Value(35)
  val GOLD_BLOCK = Value(41)
  val IRON_BLOCK = Value(42)
  val STONE_SLAB = Value(44)
  val BRICK_BLOCK = Value(45)
  val BOOKSHELF = Value(47)
  val MOSSY_COBBLESTONE = Value(48)
  val OBSIDIAN = Value(49)
  val OAK_STAIRS = Value(53)
  val DIAMOND_ORE = Value(56)
  val DIAMOND_BLOCK = Value(57)
  val SNOW = Value(80)
}

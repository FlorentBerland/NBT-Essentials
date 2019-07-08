package model


case class Material(id: Int, variant: Int){
  def name: String = Material.all.find(_._2 == this).get._1
}

object Material {

  private def _namesToMaterials = {
    this.getClass.getDeclaredFields.filter(_.getType == classOf[Material])
      .map(field => field.getName -> field.get(this).asInstanceOf[Material])
      .toMap
  }

  def all: Map[String, Material] = _namesToMaterials

  val Air =                 Material(0, 0)
  val Stone =               Material(1, 0)
  val Granite =             Material(1, 1)
  val Polished_Granite =    Material(1, 2)
  val Grass =               Material(2, 0)
  val Dirt =                Material(3, 0)
  val Cobblestone =         Material(4, 0)
  val Oak_Planks =          Material(5, 0)
  val Oak_Sapling =         Material(6, 0)
  val Bedrock =             Material(7, 0)
  val Sand =                Material(12, 0)
  val Oak_Wood =            Material(17, 0)
  val Oak_Leaves =          Material(18, 0)
  val Spruce_Leaves =       Material(18, 1)
  val Birch_Leaves =        Material(18, 2)
  val Lapis_Lazuli_Block =  Material(22, 0)
  val Block_Of_Gold =       Material(41, 0)
  val Block_Of_Diamond =    Material(57, 0)
  val Block_Of_Emerald =    Material(133, 0)
  val Block_Of_Redstone =   Material(152, 0)
  val Dark_Oak_Wood =       Material(162, 1)

}
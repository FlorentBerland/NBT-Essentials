package model

case class MaterialGroup(name: String, materialsToWeights: Map[Material, Double]){

  def updated(material: Material, weight: Double): MaterialGroup =
    MaterialGroup(name, materialsToWeights.updated(material, weight))

}

object MaterialGroup {

  val default = MaterialGroup("empty", Map.empty)

  def apply(name: String): MaterialGroup = new MaterialGroup(name, Map.empty)

}
package model.nbt

case class NBT(name: String, tree: TAG){

  def treeUpdated(newTree: TAG): NBT = NBT(name, newTree)

}
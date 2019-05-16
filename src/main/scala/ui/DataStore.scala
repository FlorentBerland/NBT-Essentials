package ui

import model.{Material, MaterialGroup}
import model.mesh.Object

class DataStore {

  val objectsToSelection = new Mutable(Map.empty[Object, Mutable[Boolean]])
  val recentFiles = new Mutable(List.empty[String])
  val materialsRemap = new Mutable(Map.empty[MaterialGroup, Mutable[List[Material]]])

  objectsToSelection.addListener(map => materialsRemap() = map.foldLeft(Set.empty[MaterialGroup])((set, kv) => {
    set ++ kv._1.materialsToLines.keySet ++ kv._1.materialsToFaces.keySet
  }).map(_ -> new Mutable(List.empty[Material])).toMap)

}

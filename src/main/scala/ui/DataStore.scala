package ui

import model.mesh.Object

class DataStore {

  val objectsToSelection = new Mutable(Map.empty[Object, Mutable[Boolean]])
  val recentFiles = new Mutable(List.empty[String])

}

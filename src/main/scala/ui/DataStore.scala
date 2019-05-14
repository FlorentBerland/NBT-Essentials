package ui

import model.mesh.Object

class DataStore {

  val objects = new Mutable(List.empty[Object])
  val recentFiles = new Mutable(List.empty[String])
  val selectedObjects = new Mutable(Set.empty[Object])

}

package model.mesh

import model.MaterialGroup

case class Object(name: String,
                  materialsToFaces: Map[MaterialGroup, Array[Face]],
                  materialsToLines: Map[MaterialGroup, Array[Line]]){

  lazy val objectInfo =
    (materialsToFaces.values.flatten.map(_.objectInfo) ++ materialsToLines.values.flatten.map(_.objectInfo))
      .reduce(_ merge _)

}
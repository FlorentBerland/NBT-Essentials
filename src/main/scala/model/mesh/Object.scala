package model.mesh

import model.MaterialGroup

case class Object(name: String,
                  materialsToFaces: Map[MaterialGroup, Array[Face]],
                  materialsToLines: Map[MaterialGroup, Array[Line]])
package io

import model.mesh._
import model.MaterialGroup

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Try

object ObjFileReader2 {

  def read(fileName: String): Try[List[model.mesh.Object]] = {
    Try(readImpl(fileName))
  }

  private def readImpl(fileName: String): List[Object] = {
    val source = Source.fromFile(fileName)
    var vertices = Array.ofDim[Vertex](16)
    val objects = new ListBuffer[Object]
    var currentObjectName: String = null
    var currentMaterial: MaterialGroup = MaterialGroup.default
    var currentObjectLines: mutable.HashMap[MaterialGroup, ListBuffer[Line]] = null
    var currentObjectFaces: mutable.HashMap[MaterialGroup, ListBuffer[Face]] = null
    var vertexIndex = 0

    def createObject(): Unit = {
      objects += Object(
        currentObjectName,
        currentObjectFaces.map(kv => kv._1 -> kv._2.toArray).toMap,
        currentObjectLines.map(kv => kv._1 -> kv._2.toArray).toMap
      )
    }

    source.getLines().foreach {
      case x if x.startsWith("o ") =>
        // Save the previous object values except the first time
        if(currentObjectName != null){
          createObject()
        }
        currentObjectName = x.split(" ")(1)
        currentObjectLines = new mutable.HashMap[MaterialGroup, ListBuffer[Line]]()
        currentObjectFaces = new mutable.HashMap[MaterialGroup, ListBuffer[Face]]()
      case x if x.startsWith("v ") =>
        val doubleArray = x.split(" ").tail.map(_.trim.toDouble)
        // Z axis is the height in a regular coordinate system but in MC it is the Y axis
        vertices(vertexIndex) = (doubleArray(0), doubleArray(2), -doubleArray(1))
        vertexIndex = vertexIndex + 1
        if(vertexIndex == vertices.length){
          // Extend the array
          val newArray = Array.ofDim[(Double, Double, Double)](vertices.length * 4)
          for(i <- vertices.indices) newArray(i) = vertices(i)
          vertices = newArray
        }
      case x if x.startsWith("l ") =>
        val vertexIndices = x.split(" ").tail.map(_.toInt)
        if(!currentObjectLines.contains(currentMaterial))
          currentObjectLines.put(currentMaterial, new ListBuffer[Line])
        currentObjectLines(currentMaterial) += Line(vertexIndices.map(i => vertices(i - 1)):_*)
      case x if x.startsWith("f ") =>
        val vertexIndices = x.split(" ").tail.map(_.split("/")(0).toInt)
        if(!currentObjectFaces.contains(currentMaterial))
          currentObjectFaces.put(currentMaterial, new ListBuffer[Face])
        currentObjectFaces(currentMaterial) += Face(vertexIndices.map(i => vertices(i - 1)).toList)
      case x if x.startsWith("usemtl ") =>
        val matName = x.split(" ")(1)
        currentMaterial = MaterialGroup(matName)
      case _ =>
    }
    source.close()
    createObject()

    objects.toList
  }

}
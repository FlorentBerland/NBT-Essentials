package io

import model.Block

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}

object ObjFileReader {

  def read(fileName: String): Try[Array[(Int, Int, Int, Block.Value)]] = {
    try {
      Success(readImpl(fileName))
    } catch {
      case e: Exception => Failure(e)
    }
  }

  private def readImpl(fileName: String): Array[(Int, Int, Int, Block.Value)] = {
    val sourceLines = Source.fromFile(fileName)
    val vertexCount = sourceLines.getLines().count(_.startsWith("v "))
    sourceLines.close

    val source = Source.fromFile(fileName)
    val coordsArray = Array.ofDim[((Double, Double, Double), ListBuffer[Block.Value])](vertexCount)
    val additionalBlocks = new ListBuffer[((Int, Int, Int), Block.Value)]()
    var currentMaterial = Block.AIR
    var vertexIndex = 0

    source.getLines().foreach {
      case x if x.startsWith("v ") =>
        val doubleArray = x.split(" ").tail.map(_.trim.toDouble)
        coordsArray(vertexIndex) = ((-doubleArray(0), doubleArray(2), doubleArray(1)), new ListBuffer[Block.Value])
        vertexIndex = vertexIndex + 1
      case x if x.startsWith("usemtl ") =>
        currentMaterial = try Block.withName(x.split(" ")(1).toUpperCase) catch { case _: Exception => Block.AIR}
      case x if x.startsWith("f ") =>
        val vertexIndices = x.split(" ").tail.map(_.split("/")(0).toInt)
        fillFace(additionalBlocks, currentMaterial, vertexIndices.map(i => coordsArray(i - 1)._1):_*)
        vertexIndices.foreach(i => coordsArray(i - 1)._2 += currentMaterial)
      case _ =>
    }
    source.close()
    val coordsToMaterials = coordsArray.view(0, vertexIndex).foldLeft(Map.empty[(Int, Int, Int), ListBuffer[Block.Value]])((map, tuple) => {
      val key = tuple._1.round()
      map.get(key) match {
        case Some(materials) =>
          materials ++= tuple._2
          map
        case None => map + (key -> tuple._2)
      }
    })
    val filledCoords = additionalBlocks.foldLeft(coordsToMaterials)((map, additionalBlock) => {
      map.get(additionalBlock._1) match {
        case Some(materials) =>
          materials += additionalBlock._2
          map
        case None =>
          val newList = new ListBuffer[Block.Value]()
          newList += additionalBlock._2
          map.updated(additionalBlock._1, newList)
      }
    })
    filledCoords.map(kv => (
      kv._1._1,
      kv._1._2,
      kv._1._3,
      kv._2.toList.groupBy(m => m).maxBy(_._2.size)._1
    )).toArray
  }


  ///// Private part /////

  private def fillFace(blocksList: ListBuffer[((Int, Int, Int), Block.Value)],
               material: Block.Value, vertices: (Double, Double, Double)*): Unit = {
    vertices.length match {
      case 1 =>
      case 2 => fillLine(blocksList, material, vertices(0), vertices(1))
      case 3 => fillTriangle(blocksList, material, vertices(0), vertices(1), vertices(2))
      case _ =>
    }
  }

  private def fillTriangle(blocksList: ListBuffer[((Int, Int, Int), Block.Value)],
                   material: Block.Value,
                   v1: (Double, Double, Double),
                   v2: (Double, Double, Double),
                   v3: (Double, Double, Double)): Unit = {
    val center = (v1 + v2 + v3) / 3
    val coords1 = v1.round()
    val coords2 = v2.round()
    val coords3 = v3.round()
    val coordsCenter = center.round()
    blocksList += ((coordsCenter, material))
    if(coords1 == coordsCenter) {
      fillLine(blocksList, material, v2, v1)
      fillLine(blocksList, material, v1, v3)
    } else if(coords2 == coordsCenter){
      fillLine(blocksList, material, v1, v2)
      fillLine(blocksList, material, v2, v3)
    } else if(coords3 == coordsCenter){
      fillLine(blocksList, material, v1, v3)
      fillLine(blocksList, material, v3, v2)
    } else {
      fillTriangle(blocksList, material, v1, v2, center)
      fillTriangle(blocksList, material, v1, v3, center)
      fillTriangle(blocksList, material, v2, v3, center)
    }
  }

  private def fillLine(blocksList: ListBuffer[((Int, Int, Int), Block.Value)],
               material: Block.Value,
               v1: (Double, Double, Double),
               v2: (Double, Double, Double)): Unit = {
    if(v1.round().isNeighbor(v2.round())) return
    if(v1.distanceTo(v2) < .1) return
    val center = (v1 + v2) / 2
    blocksList += ((center.round(), material))
    fillLine(blocksList, material, v1, center)
    fillLine(blocksList, material, center, v2)
  }


  ///// Implicit helpers /////

  implicit class VectorOps(vec: (Double, Double, Double)){
    def +(arg: (Double, Double, Double)): (Double, Double, Double) = (vec._1 + arg._1, vec._2 + arg._2, vec._3 + arg._3)
    def -(arg: (Double, Double, Double)): (Double, Double, Double) = vec + -arg
    def *(arg: Double): (Double, Double, Double) = (vec._1 * arg, vec._2 * arg, vec._3 * arg)
    def *(arg: Int): (Double, Double, Double) = vec * arg.toDouble
    def /(arg: Double): (Double, Double, Double) = (vec._1 / arg, vec._2 / arg, vec._3 / arg)
    def /(arg: Int): (Double, Double, Double) = vec / arg.toDouble
    def magnitude(): Double = Math.sqrt(vec._1 * vec._1 + vec._2 * vec._2 + vec._3 * vec._3)
    def distanceTo(arg: (Double, Double, Double)): Double = (vec - arg).magnitude()
    def unary_-(): (Double, Double, Double) = (-vec._1, -vec._2, -vec._3)
    def round(): (Int, Int, Int) = (Math.round(vec._1).toInt, Math.round(vec._2).toInt, Math.round(vec._3).toInt)
  }

  implicit class BlockOps(coords: (Int, Int, Int)) {
    def isNeighbor(arg: (Int, Int, Int)): Boolean = {
      Math.abs(coords._1 - arg._1) +
      Math.abs(coords._2 - arg._2) +
      Math.abs(coords._3 - arg._3) <= 1
    }
    def toVector: (Double, Double, Double) = (coords._1.toDouble, coords._2.toDouble, coords._3.toDouble)
  }

  implicit class IntOps(i: Int){
    def *(arg: (Double, Double, Double)): (Double, Double, Double) = arg * i
    def /(arg: (Double, Double, Double)): (Double, Double, Double) = arg / i
  }

  implicit class DoubleOps(d: Double){
    def *(arg: (Double, Double, Double)): (Double, Double, Double) = arg * d
    def /(arg: (Double, Double, Double)): (Double, Double, Double) = arg / d
  }

}
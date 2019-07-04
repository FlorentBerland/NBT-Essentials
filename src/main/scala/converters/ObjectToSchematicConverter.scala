package converters

import model.{Material, MaterialGroup, Schematic}
import model.mesh.{Object, ObjectInfo}

import scala.collection.mutable.ListBuffer

object ObjectToSchematicConverter {

  def createSchematic(objects: List[Object], mergeStrategy: MergeStrategy): Schematic = {

    def fillFace(blocksList: ListBuffer[((Int, Int, Int), MaterialGroup)],
                 materialGroup: MaterialGroup,
                 vertices: (Double, Double, Double)*): Unit = {
      vertices.length match {
        case 1 =>
        case 2 => fillLine(blocksList, materialGroup, vertices(0), vertices(1))
        case 3 => fillTriangle(blocksList, materialGroup, vertices(0), vertices(1), vertices(2))
        case _ =>
      }
    }

    def fillTriangle(blocksList: ListBuffer[((Int, Int, Int), MaterialGroup)],
      materialGroup: MaterialGroup,
      v1: (Double, Double, Double),
      v2: (Double, Double, Double),
      v3: (Double, Double, Double),
      lastBlock: Option[(Int, Int, Int)] = None): Unit = {
      val center = (v1 + v2 + v3) / 3
      val coords1 = v1.round
      val coords2 = v2.round
      val coords3 = v3.round
      val coordsCenter = center.round
      if(!lastBlock.contains(coordsCenter))
        blocksList += ((coordsCenter, materialGroup))
      if(coords1 == coordsCenter) {
        fillLine(blocksList, materialGroup, v2, v1)
        fillLine(blocksList, materialGroup, v1, v3)
      } else if(coords2 == coordsCenter){
        fillLine(blocksList, materialGroup, v1, v2)
        fillLine(blocksList, materialGroup, v2, v3)
      } else if(coords3 == coordsCenter){
        fillLine(blocksList, materialGroup, v1, v3)
        fillLine(blocksList, materialGroup, v3, v2)
      } else {
        fillTriangle(blocksList, materialGroup, v1, v2, center)
        fillTriangle(blocksList, materialGroup, v1, v3, center)
        fillTriangle(blocksList, materialGroup, v2, v3, center)
      }
    }

    def fillLine(blocksList: ListBuffer[((Int, Int, Int), MaterialGroup)],
                         materialGroup: MaterialGroup,
                         v1: (Double, Double, Double),
                         v2: (Double, Double, Double),
                         lastBlock: Option[(Int, Int, Int)] = None): Unit = {
      if(v1.round.isNeighbor(v2.round)) return
      if(v1.distanceTo(v2) < .1) return
      val center = (v1 + v2) / 2
      val coordsCenter = center.round
      if(!lastBlock.contains(coordsCenter))
        blocksList += ((coordsCenter, materialGroup))
      fillLine(blocksList, materialGroup, v1, center, Some(coordsCenter))
      fillLine(blocksList, materialGroup, center, v2, Some(coordsCenter))
    }

    val allMaterials = objects.flatMap(_.materialsToFaces.keys).distinct
    val mats = allMaterials.map(group => {
      val sum = group.materialsToWeights.values.sum
      val a = group.materialsToWeights.foldLeft((0d, List.empty[(Material, Double)]))((acc, value) => {
        (acc._1 + value._2) -> (acc._2 :+ (value._1 -> ((acc._1 + value._2) / sum)))
      })
      group -> a._2
    }).toMap
    val facesToFill = objects.flatMap(_.materialsToFaces.filter(_._1.materialsToWeights.nonEmpty).toList.flatMap(kv => kv._2.map(kv._1 -> _.vertices)))
    val allBlocks = new ListBuffer[((Int, Int, Int), MaterialGroup)]
    facesToFill.foreach(kv => fillFace(allBlocks, kv._1, kv._2:_*))

    val info = objects.foldLeft(ObjectInfo.default)((acc, value) => acc merge value.objectInfo)
    val xMin = Math.ceil(info.xMin).toInt
    val yMin = Math.ceil(info.yMin).toInt
    val zMin = Math.ceil(info.zMin).toInt
    val width = Math.ceil(info.xMax - info.xMin).toInt + 1
    val height = Math.ceil(info.yMax - info.yMin).toInt + 1
    val length = Math.ceil(info.zMax - info.zMin).toInt + 1
    val schematic = Schematic(width, height, length)

    val blocksToMaterials = allBlocks.groupBy(_._1).map(kv => kv._1 -> kv._2.groupBy(_._2).maxBy(_._2.size)._1)
    blocksToMaterials.foreach(kv => {
      val rand = math.random()
      val material = mats(kv._2).find(_._2 > rand).get._1
      schematic.setBlock(kv._1._1 - xMin, kv._1._2 - yMin, kv._1._3 - zMin, material.blockId)
      schematic.setData(kv._1._1 - xMin, kv._1._2 - yMin, kv._1._3 - zMin, material.data)
    })

    schematic
  }

  ///// Constants /////

  sealed trait MergeStrategy { val description: String }
  case object MostRepresentedGroup extends MergeStrategy {
    override val description =
      "If more than one group can be set to a single block, the group the most represented by the faces in the block will be chosen (slower)"
  }
  case object MixGroups extends MergeStrategy {
    override val description =
      "All the groups that can be set to a single block will be represented with their ratio (slowest)"
  }
  case object Default extends MergeStrategy {
    override val description = "A group will be set arbitrary (fastest)"
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
    def round: (Int, Int, Int) = (Math.ceil(vec._1).toInt, Math.ceil(vec._2).toInt, Math.ceil(vec._3).toInt)
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

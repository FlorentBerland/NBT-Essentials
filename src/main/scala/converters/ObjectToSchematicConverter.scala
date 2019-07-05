package converters

import model.mesh.Object
import model.{Material, MaterialGroup, Schematic}
import util.Time

import scala.collection.mutable

object ObjectToSchematicConverter {

  def createSchematic(objects: List[Object], mergeStrategy: MergeStrategy): Schematic = {

    def fillFace(blocksMap: mutable.HashMap[(Int, Int, Int), mutable.HashMap[MaterialGroup, Int]],
                 materialGroup: MaterialGroup,
                 vertices: (Double, Double, Double)*): Unit = {
      vertices.length match {
        case 1 =>
        case 2 => fillLine(blocksMap, materialGroup, vertices(0), vertices(1))
        case 3 => fillTriangle(blocksMap, materialGroup, vertices(0), vertices(1), vertices(2))
        case _ =>
      }
    }

    def fillTriangle(blocksMap: mutable.HashMap[(Int, Int, Int), mutable.HashMap[MaterialGroup, Int]],
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
        addToBlocks(blocksMap, coordsCenter, materialGroup)
      if(coords1 == coordsCenter || coords2 == coordsCenter || coords3 == coordsCenter) {
        fillLine(blocksMap, materialGroup, v1, v2)
        fillLine(blocksMap, materialGroup, v1, v3)
        fillLine(blocksMap, materialGroup, v2, v3)
      } else {
        val med12 = (v1 + v2) / 2
        val med13 = (v1 + v3) / 2
        val med23 = (v2 + v3) / 2
        fillTriangle(blocksMap, materialGroup, v1, med12, med13)
        fillTriangle(blocksMap, materialGroup, v2, med12, med23)
        fillTriangle(blocksMap, materialGroup, v3, med13, med23)
        fillTriangle(blocksMap, materialGroup, med12, med13, med23)
      }
    }

    def fillLine(blocksMap: mutable.HashMap[(Int, Int, Int), mutable.HashMap[MaterialGroup, Int]],
                         materialGroup: MaterialGroup,
                         v1: (Double, Double, Double),
                         v2: (Double, Double, Double),
                         lastBlock: Option[(Int, Int, Int)] = None): Unit = {
      if(v1.round.isNeighbor(v2.round)) return
      if(v1.distanceTo(v2) < .2) return
      val center = (v1 + v2) / 2
      val coordsCenter = center.round
      if(!lastBlock.contains(coordsCenter))
        addToBlocks(blocksMap, coordsCenter, materialGroup)
      fillLine(blocksMap, materialGroup, v1, center, Some(coordsCenter))
      fillLine(blocksMap, materialGroup, center, v2, Some(coordsCenter))
    }

    def addToBlocks(blocksMap: mutable.HashMap[(Int, Int, Int), mutable.HashMap[MaterialGroup, Int]],
                    coords: (Int, Int, Int),
                    group: MaterialGroup): Unit = {
      val materials = blocksMap.getOrElse(coords, new mutable.HashMap[MaterialGroup, Int]())
      val candidatesNumber = materials.getOrElse(group, 0) + 1
      materials.update(group, candidatesNumber)
      blocksMap.update(coords, materials)
    }

    val allMaterials = Time {
      print("Extracting materials... ")
      objects.flatMap(_.materialsToFaces.keys).distinct
    }(t => println("    done in " + t + " ms"))

    val mats = Time {
      print("Caching material weights... ")
      allMaterials.map(group => {
        val sum = group.materialsToWeights.values.sum
        val a = group.materialsToWeights.foldLeft((0d, List.empty[(Material, Double)]))((acc, value) => {
          (acc._1 + value._2) -> (acc._2 :+ (value._1 -> ((acc._1 + value._2) / sum)))
        })
        group -> a._2
      }).toMap
    }(t => println("done in " + t + " ms"))

    val facesToFill = Time {
      print("Caching faces... ")
      objects.flatMap(_.materialsToFaces.filter(_._1.materialsToWeights.nonEmpty).toList.flatMap(kv => kv._2.map(kv._1 -> _.vertices)))
    }(t => println("           done in " + t + " ms"))

    val allBlocks = new mutable.HashMap[(Int, Int, Int), mutable.HashMap[MaterialGroup, Int]]()
    Time {
      print("Converting structure... ")
      facesToFill.foreach(kv => fillFace(allBlocks, kv._1, kv._2:_*))
    }(t => println("    done in " + t + " ms"))

    val blocksToMaterials = Time {
      print("Scoring final materials... ")
      allBlocks.map(kv => kv._1 -> kv._2.maxBy(_._2)._1)
    }(t => println(" done in " + t + " ms"))

    val (xMin, xMax, yMin, yMax, zMin, zMax) = Time {
      print("Computing schematic info... ")
      var xm, xM, ym, yM, zm, zM = 0
      if(allBlocks.nonEmpty){
        xm = allBlocks.head._1._1
        xM = allBlocks.head._1._1
        ym = allBlocks.head._1._2
        yM = allBlocks.head._1._2
        zm = allBlocks.head._1._3
        zM = allBlocks.head._1._3
      }
      allBlocks.foreach(kv => {
        if(kv._1._1 < xm) xm = kv._1._1
        if(kv._1._1 > xM) xM = kv._1._1
        if(kv._1._2 < ym) ym = kv._1._2
        if(kv._1._2 > yM) yM = kv._1._2
        if(kv._1._3 < zm) zm = kv._1._3
        if(kv._1._3 > zM) zM = kv._1._3
      })
      (xm, xM, ym, yM, zm, zM)
    }(t => println("done in " + t + " ms"))

    val width = xMax - xMin + 1
    val height = yMax - yMin + 1
    val length = zMax - zMin + 1
    val schematic = Schematic(width, height, length)

    println("Schematic width:  " + width)
    println("Schematic height: " + height)
    println("Schematic length: " + length)
    println("Faces computed:   " + facesToFill.size)
    println("Blocks generated: " + allBlocks.size)

    Time {
      print("Filling schematic... ")
      blocksToMaterials.foreach(kv => {
        val rand = math.random()
        val material = mats(kv._2).find(_._2 > rand).get._1
        schematic.setBlock(kv._1._1 - xMin, kv._1._2 - yMin, kv._1._3 - zMin, material.blockId)
        schematic.setData(kv._1._1 - xMin, kv._1._2 - yMin, kv._1._3 - zMin, material.data)
      })
    }(t => println("       done in " + t + " ms"))

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

package converters

import model.mesh.Object
import model.{Material, MaterialGroup, Schematic}
import util.Time

import scala.collection.mutable

object ObjectToSchematicConverter {

  def createSchematic(objects: List[Object]): Schematic = {

    var blocksAdded = 0
    var fillTriangleCalls = 0
    var fillLineCalls = 0

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
      v3: (Double, Double, Double)): Unit = {
      fillTriangleCalls += 1
      val center = (v1 + v2 + v3) / 3
      val coords1 = v1.round
      val coords2 = v2.round
      val coords3 = v3.round
      val coordsCenter = center.round
      addToBlocks(blocksMap, coordsCenter, materialGroup)
      if(coords1 == coordsCenter || coords2 == coordsCenter || coords3 == coordsCenter || coords1 == coords2 || coords1 == coords3 || coords2 == coords3) {
        fillLine(blocksMap, materialGroup, v1, v2)
        fillLine(blocksMap, materialGroup, v1, v3)
        fillLine(blocksMap, materialGroup, v2, v3)
      } else {
        val d12 = v1.distanceTo(v2)
        val d13 = v1.distanceTo(v3)
        val d23 = v2.distanceTo(v3)
        if(d12 >= d13 && d12 >= d23){
          val med = (v1 + v2) / 2
          fillTriangle(blocksMap, materialGroup, v1, v3, med)
          fillTriangle(blocksMap, materialGroup, v2, v3, med)
        } else if(d13 >= d23){
          val med = (v1 + v3) / 2
          fillTriangle(blocksMap, materialGroup, v1, v2, med)
          fillTriangle(blocksMap, materialGroup, v3, v2, med)
        } else {
          val med = (v2 + v3) / 2
          fillTriangle(blocksMap, materialGroup, v2, v1, med)
          fillTriangle(blocksMap, materialGroup, v3, v1, med)
        }
      }
    }

    def fillLine(blocksMap: mutable.HashMap[(Int, Int, Int), mutable.HashMap[MaterialGroup, Int]],
                         materialGroup: MaterialGroup,
                         v1: (Double, Double, Double),
                         v2: (Double, Double, Double),
                         lastBlock: Option[(Int, Int, Int)] = None): Unit = {
      fillLineCalls += 1
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
      blocksAdded += 1
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

    val allBlocks = new mutable.HashMap[(Int, Int, Int), mutable.HashMap[MaterialGroup, Int]]()

    val nbFaces = Time {
      print("Converting structure... ")
      var nFace = 0
      objects.foreach(o => {
        o.materialsToFaces.foreach(groupToFaces => {
          if(groupToFaces._1.materialsToWeights.nonEmpty){
            groupToFaces._2.foreach(face => {
              fillFace(allBlocks, groupToFaces._1, face.vertices:_*)
              nFace += 1
            })
          }
        })
      })
      nFace
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
    val schematic = Schematic.create(width, height, length)

    Time {
      print("Filling schematic... ")
      blocksToMaterials.foreach(kv => {
        val rand = math.random()
        val material = mats(kv._2).find(_._2 > rand).get._1
        schematic(kv._1._1 - xMin, kv._1._2 - yMin, kv._1._3 - zMin) = material
      })
    }(t => println("       done in " + t + " ms"))

    println("Faces computed:      " + nbFaces)
    println("Triangle iterations: " + fillTriangleCalls)
    println("Line iterations:     " + fillLineCalls)
    println("Blocks payload:      " + blocksAdded)
    println("Blocks generated:    " + allBlocks.size)
    println("Schematic width:     " + width)
    println("Schematic height:    " + height)
    println("Schematic length:    " + length)

    schematic
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

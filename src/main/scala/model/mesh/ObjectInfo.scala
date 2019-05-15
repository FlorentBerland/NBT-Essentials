package model.mesh

case class ObjectInfo(xMin: Double, xMax: Double,
                      yMin: Double, yMax: Double,
                      zMin: Double, zMax: Double,
                      nbFaces: Int, nbVertices: Int){

  def merge(objectInfo: ObjectInfo): ObjectInfo = ObjectInfo(
    xMin min objectInfo.xMin,
    xMax max objectInfo.xMax,
    yMin min objectInfo.yMin,
    yMax max objectInfo.yMax,
    zMin min objectInfo.zMin,
    zMax max objectInfo.zMax,
    nbFaces + objectInfo.nbFaces,
    nbVertices + objectInfo.nbVertices
  )

}

object ObjectInfo {

  val default = ObjectInfo(0, 0, 0, 0, 0, 0, 0, 0)

}
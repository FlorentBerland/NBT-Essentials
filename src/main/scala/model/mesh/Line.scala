package model.mesh

case class Line(vertices: Vertex*){

  lazy val objectInfo = ObjectInfo(
    vertices.minBy(_._1)._1,
    vertices.maxBy(_._1)._1,
    vertices.minBy(_._2)._2,
    vertices.maxBy(_._2)._2,
    vertices.minBy(_._3)._3,
    vertices.maxBy(_._3)._3,
    0, vertices.length, 0
  )

}

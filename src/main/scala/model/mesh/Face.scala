package model.mesh

case class Face(vertices: List[Vertex]){

  lazy val objectInfo = ObjectInfo(
      vertices.minBy(_._1)._1,
      vertices.maxBy(_._1)._1,
      vertices.minBy(_._2)._2,
      vertices.maxBy(_._2)._2,
      vertices.minBy(_._3)._3,
      vertices.maxBy(_._3)._3,
      1, vertices.length
    )

}

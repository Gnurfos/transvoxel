package com.github.gnurfos.transvoxel


object Mesh {
  case class Vector(x: Float, y: Float, z: Float)

  val dummyVector: Vector = Vector(1, 2, 3)

  case class Vertex(position: Vector, normal: Vector)

  object Vertex {
    def apply(
               x1: Float, y1: Float, z1: Float): Vertex =
      Vertex(Vector(x1, y1, z1), dummyVector)
    def apply(
               x1: Float, y1: Float, z1: Float,
               x2: Float, y2: Float, z2: Float): Vertex =
        Vertex(Vector(x1, y1, z1), Vector(x2, y2, z2))
  }

  case class Triangle(v1: Vertex, v2: Vertex, v3: Vertex) {
    def this(
              x1: Float, y1: Float, z1: Float,
              x2: Float, y2: Float, z2: Float,
              x3: Float, y3: Float, z3: Float) =
      this(
        Vertex(Vector(x1, y1, z1), dummyVector),
        Vertex(Vector(x2, y2, z2), dummyVector),
        Vertex(Vector(x3, y3, z3), dummyVector))
  }

  object Triangle {
    def apply(
              x1: Float, y1: Float, z1: Float,
              x2: Float, y2: Float, z2: Float,
              x3: Float, y3: Float, z3: Float): Triangle =
      Triangle(
        Vertex(Vector(x1, y1, z1), dummyVector),
        Vertex(Vector(x2, y2, z2), dummyVector),
        Vertex(Vector(x3, y3, z3), dummyVector))
  }

}


/**
  * Class storing a 3D mesh (set of triangles)
  *
  * For efficiency, access directly the constructor parameters. Methods are just there for ease of testing/convenience
  *
  * @param vertices vertex data: 6 Floats per vertex (3 for position, 3 for normal)
  * @param triIndices triangulation data: 3 consecutive indices define a triangle. Indices refer to vertices,
  *                   not individual floats. In other words, any int found in triIndices will be < vertices.length / 6
  */
class Mesh(
            val positions: Array[Float],
            val normals: Array[Float],
            val triIndices: Array[Int]) {

  def numVertices: Int = positions.length / 3

  def getVertex(index: Int): Mesh.Vertex = {
    val vertexBase = 3 * index
    Mesh.Vertex(
      Mesh.Vector(
        positions(vertexBase),
        positions(vertexBase + 1),
        positions(vertexBase + 2)),
      Mesh.Vector(
        normals(vertexBase),
        normals(vertexBase + 1),
        normals(vertexBase + 2))
    )
  }

  def triangles: Seq[Mesh.Triangle] = for (i <- triIndices.indices by 3) yield {
    Mesh.Triangle(
      getVertex(triIndices(i)),
      getVertex(triIndices(i + 1)),
      getVertex(triIndices(i + 2))
    )
  }

}


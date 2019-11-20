package com.github.gnurfos.transvoxel

import com.github.gnurfos.transvoxel.Mesh.{Vector, Vertex}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class MeshTest extends FunSuite with Matchers {

  test("Mesh can list triangles") {
    val positions = Array[Float](
      0, 0, 0,
      1, 1, 1,
      2, 2, 2,
      3, 3, 3,
    )
    val normals = Array[Float](
      0, 0, 0,
      1, 1, 1,
      2, 2, 2,
      3, 3, 3,
    )
    val indices = Array(
      0, 1, 2,
      2, 1, 3
    )
    val m = new Mesh(positions, normals, indices)
    assert(m.numVertices == 4)
    assert(m.triangles.size == 2)
    assert(m.triangles(1).v1 == Vertex(Vector(2, 2, 2), Vector(2, 2, 2)))
  }

}
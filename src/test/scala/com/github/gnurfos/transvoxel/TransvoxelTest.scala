package com.github.gnurfos.transvoxel

import com.github.gnurfos.transvoxel.Mesh.{Triangle, Vector, Vertex}
import com.github.gnurfos.transvoxel.internal.Corner
import org.junit.runner.RunWith
import org.scalactic.Equality
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers, PrivateMethodTester}


@RunWith(classOf[JUnitRunner])
class TransvoxelTest extends FunSuite with Matchers with PrivateMethodTester {

  implicit val triangleEquality: Equality[Triangle] = new Equality[Triangle] {
    private def rotate(t: Triangle) : Triangle = Triangle(t.v2, t.v3, t.v1)
    def areEqual(a: Triangle, b: Any): Boolean = {
      b match {
        case t: Triangle => List(a, rotate(a), rotate(rotate(a))).exists(same(_, t))
        case _ => false
      }
    }
    private def same(a: Triangle, b: Triangle): Boolean = {
      same(a.v1, b.v1) && same(a.v2, b.v2) && same(a.v3, b.v3)
    }
    private def same(v1: Vertex, v2: Vertex): Boolean = {
      if ((v1.normal == Mesh.dummyVector) || (v2.normal == Mesh.dummyVector)) {
        // One has no normal, just compare positions
        same(v1.position, v2.position)
      } else {
        same(v1.position, v2.position) && same(v1.normal, v2.normal)
      }
    }
    private def same(v1: Vector, v2: Vector): Boolean = {
      (v1.x === v2.x +- 0.000001f) &&
        (v1.y === v2.y +- 0.000001f) &&
        (v1.z === v2.z +- 0.000001f)
    }
  }


  private val threshold = 0.5f
  private val noTransitionSide = Sides.ValueSet.empty

  test("Empty extraction") {
    val f = new DensityArray(10)
    val m = Transvoxel.extract(f, f.dimensions, threshold, 10, noTransitionSide)
    assert(m.triangles.isEmpty)
  }

  test("One cube corner gives one triangle") {
    val f = new DensityArray(1)
    f.set(0, 0, 0, 1.0f)
    val m = Transvoxel.extract(f, f.dimensions, threshold, 1, noTransitionSide)
    assert(m.triangles.length == 1)
    m.triangles should contain theSameElementsAs Array(Triangle(0.5f,0f,0f, 0f,0.5f,0f, 0f,0f,0.5f))
  }

  test("Another one cube corner") {
    val f = new DensityArray(1)
    f.set(0, 1, 0, 1.0f)
    val m = Transvoxel.extract(f, f.dimensions, threshold, 1, noTransitionSide)
    assert(m.triangles.length == 1)
    m.triangles should contain theSameElementsAs Array(Triangle(0.5f,1f,0f, 0f,1f,0.5f, 0f,0.5f,0f))
  }

  test("Two corners gives two triangles in one cube") {
    val f = new DensityArray(2)
    f.set(0, 0, 0, 1.0f)
    f.set(1, 0, 0, 1.0f)
    val oneCubeRegion = Block(0, 0, 0, 1)
    val m = Transvoxel.extract(f, oneCubeRegion, threshold, 1, noTransitionSide)
    assert(m.triangles.length == 2)
    m.triangles should contain theSameElementsAs Array(
      Triangle(1f,0f,0.5f, 0f,0.5f,0f, 0f,0f,0.5f),
      Triangle(1f,0.5f,0f, 0f,0.5f,0f, 1f,0f,0.5f))
  }

  test("Normals") {
    // Extract a flat (same y at 5.5) square
    // Set density=1 at y=5. We need to go wider than the extracted cube, because normals use gradient data that reaches farther than the cube
    val f = new DensityArray(10)
    for (x <- 4 to 7; z <- 4 to 7) {
      f.set(x, 5, z, 1.0f)
    }
    val oneCubeRegion = Block(5, 5, 5, 1)
    val m = Transvoxel.extract(f, oneCubeRegion, threshold, 1, noTransitionSide)
    assert(m.triangles.length == 2)
    // Check that all normals are unit +y
    m.triangles should contain theSameElementsAs Array(
      Triangle(
        Vertex(6f,5.5f,5f, 0f,1f,0f),
        Vertex(5f,5.5f,5f, 0f,1f,0f),
        Vertex(5f,5.5f,6f, 0f,1f,0f)),
      Triangle(
        Vertex(6f,5.5f,5f, 0f,1f,0f),
        Vertex(5f,5.5f,6f, 0f,1f,0f),
        Vertex(6f,5.5f,6f, 0f,1f,0f)))
  }

  test("Ambiguous case") {
    val f = new DensityArray(2)
    f.set(0, 0, 0, 1.0f)
    f.set(0, 0, 1, 1.0f)
    f.set(0, 1, 0, 1.0f)
    f.set(0, 1, 1, 1.0f)
    f.set(1, 1, 0, 1.0f)
    f.set(1, 0, 1, 1.0f)
    val firstCubeRegion = Block(0, 0, 0, 1)
    val m1 = Transvoxel.extract(f, firstCubeRegion, 0.5f, 1, noTransitionSide)
    assert(m1.triangles.length == 4)
    val secondCubeRegion = Block(1, 0, 0, 1)
    val m2 = Transvoxel.extract(f, secondCubeRegion, 0.5f, 1, noTransitionSide)
    assert(m2.triangles.length == 2)
    m2.triangles should contain theSameElementsAs Array(
      Triangle(1.5f,1f,0f, 1f,1f,0.5f, 1f,0.5f,0f),
      Triangle(1.5f,0f,1f, 1f,0f,0.5f, 1f,0.5f,1f))
  }

  test("Vertices are reused within a cell") {
    val f = new DensityArray(1)
    f.set(0, 0, 0, 1.0f)
    f.set(1, 0, 0, 1.0f)
    val oneCubeRegion = Block(0, 0, 0, 1)
    val m = Transvoxel.extract(f, oneCubeRegion, threshold, 1, noTransitionSide)
    assert(m.triangles.length == 2)
    m.triangles should contain theSameElementsAs Array(
      Triangle(1f,0f,0.5f, 0f,0.5f,0f, 0f,0f,0.5f),
      Triangle(1f,0.5f,0f, 0f,0.5f,0f, 1f,0f,0.5f))
    // 2 vertices should be reused to form a quad between the 2 triangles
    // => Total only 4 vertices instead of 6
    assert(m.numVertices == 4)
  }

  test("Vertices are reused between cells") {
    val f = new DensityArray(2)
    f.set(0, 2, 2, 1.0f)
    f.set(1, 2, 2, 1.0f)
    val twoByTwoByTwoCellsRegion = Block(0, 0, 0, 2)
    val m = Transvoxel.extract(f, twoByTwoByTwoCellsRegion, threshold, 2, noTransitionSide)
    assert(m.triangles.length == 3)
    m.triangles should contain theSameElementsAs Array(
      Triangle(0f,2f,1.5f, 1f,1.5f,2f, 0f,1.5f,2f),
      Triangle(1f,1.5f,2f, 0f,2f,1.5f, 1f,2f,1.5f),
      Triangle(1f,2f,1.5f, 1.5f,2f,2f, 1f,1.5f,2f))
    // 2 vertices should be reused to form a quad between the 2 triangles of the first cell
    // 2 vertices should be reused too between cells
    // => Total only 5 vertices instead of 9 = 3x3tris
    assert(m.numVertices == 5)
  }

  test("Trivial transition cell") {
    // Field left empty
    val f = new DensityArray(10)
    val tenByTenByTenCellsRegion = Block(0, 0, 0, 10)
    val gridSize = 1
    val m = Transvoxel.extract(f, tenByTenByTenCellsRegion, threshold, gridSize, Sides.ValueSet(Sides.NegativeZ))
    assert(m.triangles.isEmpty)
  }

  def restrict(triangles: Seq[Triangle], minX: Float, minY: Float, minZ: Float, size: Float): Seq[Triangle] = {
    def inCube(v: Vertex): Boolean = {
      (v.position.x >= minX) && (v.position.x <= minX + size) && (v.position.y >= minY) && (v.position.y <= minY + size) && (v.position.z >= minZ) && (v.position.z <= minZ + size)
    }
    triangles.filter(t => inCube(t.v1) && inCube(t.v2) && inCube(t.v3))
  }

  test("Simplest transition cell") {
    val f = new DensityArray(100)
    // We need cells in the middle of a transition block face, to ensure actual cell shrinking, so the "simplest" is not so simple
    // This produces 4 "quarters" of a pyramid (viewed from z-top)
    //  q2 q1
    //  q3 q4
    f.set(50, 50, 0, 1.0f)
    val region = Block(0, 0, 0, 100)
    val gridSize = 10
    val m = Transvoxel.extract(f, region, threshold, gridSize, Sides.ValueSet(Sides.NegativeZ))
    assert(m.triangles.length == 12)
    // shrink = 0.15, cell size=10
    // 1.5 = transition width
    // 5.75 = transition width + half the remaining
    val vTop = Vertex(50f,50f,5.75f) // Purely in the regular sub-cells

    val q1v2 = Vertex(55f,50f,1.5f)  // Between the transition and the regular sub-cells
    val q1v3 = Vertex(50f,55f,1.5f)  // Between the transition and the regular sub-cells
    val q1v4 = Vertex(50f,52.5f,0f)  // On the full-res face
    val q1v5 = Vertex(52.5f,50f,0f)  // On the full-res face

    val q2v2 = Vertex(50f,55f,1.5f)
    val q2v3 = Vertex(45f,50f,1.5f)
    val q2v4 = Vertex(47.5f,50f,0f)
    val q2v5 = Vertex(50f, 52.5f,0f)

    val q3v2 = Vertex(45f,50f,1.5f)
    val q3v3 = Vertex(50f,45f,1.5f)
    val q3v4 = Vertex(50f,47.5f,0f)
    val q3v5 = Vertex(47.5f,50f,0f)

    val q4v2 = Vertex(50f,45f,1.5f)
    val q4v3 = Vertex(55f,50f,1.5f)
    val q4v4 = Vertex(52.5f,50f,0f)
    val q4v5 = Vertex(50f,47.5f,0f)

    m.triangles should contain allOf(
      // For each quarter (Q1):
      // Regular sub-cell has 1 triangle
      Triangle(vTop, q1v2, q1v3),
      // Transition sub-cell triangles
      Triangle(q1v4, q1v3, q1v2),
      Triangle(q1v2, q1v5, q1v4)
    )

    m.triangles should contain theSameElementsAs Array(
      // For each quarter (Q1):
      // Regular sub-cell has 1 triangle
      Triangle(vTop, q1v2, q1v3),
      // Transition sub-cell triangles
      Triangle(q1v4, q1v3, q1v2),
      Triangle(q1v2, q1v5, q1v4),
      // Q2
      Triangle(vTop, q2v2, q2v3),
      Triangle(q2v4, q2v3, q2v2),
      Triangle(q2v2, q2v5, q2v4),
      // Q3
      Triangle(vTop, q3v2, q3v3),
      Triangle(q3v4, q3v3, q3v2),
      Triangle(q3v2, q3v5, q3v4),
      // Q4
      Triangle(vTop, q4v2, q4v3),
      Triangle(q4v4, q4v3, q4v2),
      Triangle(q4v2, q4v5, q4v4)

    )

    restrict(m.triangles, 50f, 50f, 0, 10f) should contain theSameElementsAs Array(
      // For each quarter (Q1):
      // Regular sub-cell has 1 triangle
      Triangle(vTop, q1v2, q1v3),
      // Transition sub-cell triangles
      Triangle(q1v4, q1v3, q1v2),
      Triangle(q1v2, q1v5, q1v4)
    )


  }

  test("Simple transition cell") {
    // We will extract a single 10x10x10 cell
    val f = new DensityArray(30)
    // Fill densities on the z=0 face
    f.set(10, 10, 0, 1.0f)
    f.set(10, 15, 0, 1.0f)
    // Case should be: 0x81
    // Class:
    val region = Block(0, 0, 0, 30)
    val gridSize = 3
    val m = Transvoxel.extract(f, region, threshold, gridSize, Sides.ValueSet(Sides.NegativeZ))
    val trianglesInCell = restrict(m.triangles, 10f, 10f, 0f, 10f)
    assert(trianglesInCell.length == 4) // 1 regular + 3 transition
    val v1 = Vertex(10f,10f,5.75f) // Purely in the regular sub-cell
    val v2 = Vertex(15f,10f,1.5f)  // Between the transition and the regular sub-cells
    val v3 = Vertex(10f,15f,1.5f)  // Between the transition and the regular sub-cells
    val v4 = Vertex(10f,17.5f,0f)  // On the full-res face
    val v5 = Vertex(12.5f,15f,0f)  // On the full-res face
    val v6 = Vertex(12.5f,10f,0f)  // On the full-res face
    trianglesInCell should contain theSameElementsAs Array(
      // Regular sub-cell has 1 triangle
      // shrink = 0.15, cell size=10
      // 1.5 = transition width
      // 5.75 = transition width + half the remaining
      //      Triangle(0,5,1.5, 0,0,5.75, 5,0,1.5),
      Triangle(v1, v2, v3),
      // Transition sub-cell triangles
      Triangle(v5, v4, v3),
      Triangle(v5, v3, v2),
      Triangle(v5, v2, v6)
    )
  }

  test("Transition face rotation computations") {
    // Region/block size 100, 10 cells. Cell size: 10
    val region = Block(0, 0, 0, 100)
    val gridSize = 10
    val emptyField = new DensitySource(gridSize) {
      override def getDensity(cellX: Int, cellY: Int, cellZ: Int): Float = 0
    }
    val t = new Transvoxel(emptyField, region, 0, gridSize, Sides.ValueSet(Sides.NegativeZ, Sides.PositiveZ, Sides.NegativeY, Sides.PositiveY, Sides.NegativeX, Sides.PositiveX), 0)
    //
    def getTransitionCornerPosition(cellX: Int, cellY: Int, cellZ: Int, fullSide: Side, cornerIndex: Int): (Float, Float, Float) = {
      // We need hacks and calling of private methods, because this cannot be extracted properly out of Transvoxel, as it needs to write into its attributes
      val setRotationFor = PrivateMethod[Unit]('setRotationFor)
      t invokePrivate setRotationFor(fullSide)
      val corner = new Corner
      val loadTransitionCornerPosition = PrivateMethod[Unit]('loadTransitionCornerPosition)
      t invokePrivate loadTransitionCornerPosition(cellX, cellY, cellZ, fullSide, cornerIndex, corner)
      (corner.x, corner.y, corner.z)
    }
    // Negative Z is the basis, without any rotation
    getTransitionCornerPosition(0, 0, 0, Sides.NegativeZ, 0) should equal (0, 0, 0)
    getTransitionCornerPosition(0, 0, 0, Sides.NegativeZ, 1) should equal (5, 0, 0)
    getTransitionCornerPosition(0, 0, 0, Sides.NegativeZ, 3) should equal (0, 5, 0)
    getTransitionCornerPosition(1, 2, 0, Sides.NegativeZ, 3) should equal (10, 25, 0)
    getTransitionCornerPosition(1, 2, 0, Sides.NegativeZ, 0xB) should equal (10, 30, 1.5)
    // Positive Z is rotated 180 degrees around Y
    getTransitionCornerPosition(0, 0, 9, Sides.PositiveZ, 0) should equal (10, 0, 100)
    getTransitionCornerPosition(0, 0, 9, Sides.PositiveZ, 1) should equal (5, 0, 100)
    getTransitionCornerPosition(0, 0, 9, Sides.PositiveZ, 3) should equal (10, 5, 100)
    getTransitionCornerPosition(1, 2, 9, Sides.PositiveZ, 3) should equal (20, 25, 100)
    getTransitionCornerPosition(1, 2, 9, Sides.PositiveZ, 0xB) should equal (20, 30, 98.5)
    // Negative Y is rotated -90 degrees around X
    getTransitionCornerPosition(0, 0, 0, Sides.NegativeY, 0) should equal (0, 0, 10)
    getTransitionCornerPosition(0, 0, 0, Sides.NegativeY, 1) should equal (5, 0, 10)
    getTransitionCornerPosition(0, 0, 0, Sides.NegativeY, 3) should equal (0, 0, 5)
    getTransitionCornerPosition(1, 0, 3, Sides.NegativeY, 3) should equal (10, 0, 35)
    getTransitionCornerPosition(1, 0, 3, Sides.NegativeY, 0xB) should equal (10, 1.5, 30)
    // Positive Y is rotated 90 degrees around X
    getTransitionCornerPosition(0, 9, 0, Sides.PositiveY, 0) should equal (0, 100, 0)
    getTransitionCornerPosition(0, 9, 0, Sides.PositiveY, 1) should equal (5, 100, 0)
    getTransitionCornerPosition(0, 9, 0, Sides.PositiveY, 3) should equal (0, 100, 5)
    getTransitionCornerPosition(1, 9, 3, Sides.PositiveY, 3) should equal (10, 100, 35)
    getTransitionCornerPosition(1, 9, 3, Sides.PositiveY, 0xB) should equal (10, 98.5, 40)
    // Negative X is rotated 90 degrees around Y
    getTransitionCornerPosition(0, 0, 0, Sides.NegativeX, 0) should equal (0, 0, 10)
    getTransitionCornerPosition(0, 0, 0, Sides.NegativeX, 1) should equal (0, 0, 5)
    getTransitionCornerPosition(0, 0, 0, Sides.NegativeX, 3) should equal (0, 5, 10)
    getTransitionCornerPosition(0, 2, 3, Sides.NegativeX, 3) should equal (0, 25, 40)
    getTransitionCornerPosition(0, 2, 3, Sides.NegativeX, 0xB) should equal (1.5, 30, 40)
    // Positive X is rotated -90 degrees around Y
    getTransitionCornerPosition(9, 0, 0, Sides.PositiveX, 0) should equal (100, 0, 0)
    getTransitionCornerPosition(9, 0, 0, Sides.PositiveX, 1) should equal (100, 0, 5)
    getTransitionCornerPosition(9, 0, 0, Sides.PositiveX, 3) should equal (100, 5, 0)
    getTransitionCornerPosition(9, 2, 3, Sides.PositiveX, 3) should equal (100, 25, 30)
    getTransitionCornerPosition(9, 2, 3, Sides.PositiveX, 0xB) should equal (98.5, 30, 30)
  }

  test("Simplest transition cell not negativeZ") {
    val f = new DensityArray(100)
    f.set(0, 20, 20, 1.0f)
    val region = Block(0, 0, 0, 100)
    val gridSize = 10
    val m = Transvoxel.extract(f, region, threshold, gridSize, Sides.ValueSet(Sides.NegativeX))
    val triangles = restrict(m.triangles, 0f, 20f, 20f, 10f)
    assert(triangles.length == 3)
    // shrink = 0.15, cell size=10
    // 1.5 = transition width
    // 5.75 = transition width + half the remaining
    val v1 = Vertex(5.75f,20f,20f) // Purely in the regular sub-cell
    val v2 = Vertex(1.5f,25f,20f)  // Between the transition and the regular sub-cells
    val v3 = Vertex(1.5f,20f,25f)  // Between the transition and the regular sub-cells
    val v4 = Vertex(0f,22.5f,20f)  // On the full-res face
    val v5 = Vertex(0f,20f,22.5f)  // On the full-res face
    triangles should contain theSameElementsAs Array(
      // Regular sub-cell has 1 triangle
      Triangle(v1, v2, v3),
      // Transition sub-cell triangles
      Triangle(v5, v3, v2),
      Triangle(v5, v2, v4)
    )
  }

  class CountingField extends ScalarField {
    var calls: List[(Float, Float, Float)] = List[(Float, Float, Float)]()
    var distinctCalls: Set[(Float, Float, Float)] = Set[(Float, Float, Float)]()
    override def getDensity(x: Float, y: Float, z: Float): Float = {
      calls :+= ((x, y, z))
      distinctCalls += ((x, y, z))
      x - y - z
    }
  }

  test("density calls without transitions") {
    val f = new CountingField
    val region = Block(0, 0, 0, 3)
    val gridSize = 3
    Transvoxel.extract(f, region, threshold, gridSize, Sides.ValueSet.empty)
    val cornersContributing = 4 * 4 * 4 // Values used for estimating cell cases
    val extendedCorners = 6 * 6 * 6 // Extended of 1 in each direction (useful only if there's a generated vertex around)
    f.calls.size should be >= cornersContributing
    f.calls.size should be <= extendedCorners
  }

  test("density calls with some transitions") {
    val f = new CountingField
    val region = Block(0, 0, 0, 3)
    val gridSize = 3
    val fullFaces = Sides.ValueSet(Sides.NegativeX, Sides.PositiveX)
    Transvoxel.extract(f, region, threshold, gridSize, fullFaces)
    val cornersContributing = 4 * 4 * 4 // Values used for estimating cell cases
    val extendedCorners = 6 * 6 * 6 // Extended of 1 in each direction (useful only if there's a generated vertex around)
    val fullFaceContributing = fullFaces.size * 7 * 7
    val extendedFullFace = fullFaces.size * 9 * 9 * 3
    f.calls.size should be >= cornersContributing + fullFaceContributing
    f.calls.size should be <= extendedCorners + extendedFullFace
    f.calls.size shouldEqual f.distinctCalls.size
    // Note: this last test doesn't work if the transition faces are touching
  }

}


class DensityArray(val size: Int) extends ScalarField {

  private val data = Array.ofDim[Float](size + 1, size + 1, size + 1)

  val dimensions = Block(0, 0, 0, size)

  def getDensity(x: Float, y: Float, z: Float) : Float = {
    if (x < 0 || y < 0 || z < 0 || x > size || y > size || z > size) 0
    else data(x.toInt)(y.toInt)(z.toInt)
  }

  def set(x: Int, y: Int, z: Int, v: Float): Unit = {
    data(x)(y)(z) = v
  }

}
package com.github.gnurfos.transvoxel

import com.github.gnurfos.transvoxel.internal.TransvoxelTables.{RegularCellCaseNumber, RegularVertexData, TransitionCellCaseNumber, TransitionVertexData}
import com.github.gnurfos.transvoxel.internal.{Corner, RotationState, SharedVertexIndices, TransvoxelTables}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Implementation of Eric Lengyel's Transvoxel Algorithm in scala/java
  *
  * Credits:
  * Eric Lengyel's Transvoxel Algorithm.
  *
  * [[https://transvoxel.org/]]
  *
  */
object Transvoxel {
  /**
    * Default/simplest extraction function.
    *
    * Customization can be attained by directly constructing and using a [[Transvoxel]] object.
    * The default uses a [[PreCachedDensitySource]]
    *
    * @param field provider of densities
    * @param region cubic space block for which to extract a mesh
    * @param threshHold density value defining the mesh surface
    * @param gridSize number of cells (in one dimension) in a block. Determines the default resolution
    * @param transitionSides which sides of the block are to be extracted at double-resolution
    * @return the [[Mesh]] of the "isosurface"
    */
  def extract(
               field: ScalarField,
               region: Block,
               threshHold: Float,
               gridSize: Int,
               transitionSides: Sides.ValueSet)
  : Mesh = {
    val densitySource = new PreCachedDensitySource(field, region, gridSize, transitionSides)
    val estimatedVertexCount = 2 * gridSize * gridSize * gridSize // Totally arbitrary: estimated 2 vertices per cell
    new Transvoxel(densitySource, region, threshHold, gridSize, transitionSides, estimatedVertexCount).run
  }

}

/**
  * Object dedicated to one mesh extraction of a block
  * @param densitySource provider of densities
  * @param region cubic space block for which to extract a mesh
  * @param threshHold density value defining the mesh surface
  * @param gridSize number of cells (in one dimension) in a block. Determines the default resolution
  * @param transitionSides which sides of the block are to be extracted at double-resolution
  * @param estimatedVertexCount the vertex buffer will be pre-allocated for this number of vertices (but still get
  *                             re-allocated if necessary)
  */
class Transvoxel(
                densitySource: DensitySource,
                region: Block,
                threshHold: Float,
                gridSize: Int,
                transitionSides: Sides.ValueSet,
                estimatedVertexCount: Int
                ) {

  /**
    * Internal constants
    */
  private val cellSize: Float = region.size / gridSize
  private val shrinkFactor = 0.15f
  private val shrink: Float = shrinkFactor * cellSize
  private val zeroToOne = 0 to 1 // Pre-created range that will get reused several times

  /**
    * Outputs
    */
  private val positionsBuffer = new mutable.ArrayBuilder.ofFloat
  private val normalsBuffer = new mutable.ArrayBuilder.ofFloat
  positionsBuffer.sizeHint(3 * estimatedVertexCount)
  normalsBuffer.sizeHint(3 * estimatedVertexCount)
  private var vertices = 0
  private val triIndices = new mutable.ArrayBuilder.ofInt
  triIndices.sizeHint(estimatedVertexCount)

  /**
    * Other mutated states and attributized outputs, to reduce allocations/boxing
    */
  private val rotationState = new RotationState
  private val sharedVertexIndices = new SharedVertexIndices(gridSize)
  // Methods named loadSomething() write into these
  private val cornerA = new Corner
  private val cornerB = new Corner

  /**
    * @return the [[Mesh]] of the "isosurface"
    */
  def run: Mesh = {
    extractAllCells()
    new Mesh(positionsBuffer.result(), normalsBuffer.result(), triIndices.result())
  }

  private def getDensity(cellX: Int, cellY: Int, cellZ: Int): Float = {
    densitySource.getDensity(cellX, cellY, cellZ)
  }

  private def getTransitionDensity(cellX: Int, cellY: Int, cellZ: Int, u: Int, v: Int, w: Int): Float = {
    densitySource.getTransitionDensity(cellX, cellY, cellZ, rotationState, u, v, w)
  }

  private def extractAllCells(): Unit = {
    // Regulars
    for (
      cellX <- 0 until gridSize;
      cellY <- 0 until gridSize;
      cellZ <- 0 until gridSize)
      extractRegularCell(cellX, cellY, cellZ)

    // Transitions
    val perFullSidePerCell = transitionSides.toSeq.map {
      fullSide => {
        setRotationFor(fullSide)
        for (
          cellU <- 0 until gridSize;
          cellV <- 0 until gridSize) yield {
          val cellX = rotationState.originX * (gridSize - 1) + cellU * rotationState.uAxisX + cellV * rotationState.vAxisX
          val cellY = rotationState.originY * (gridSize - 1) + cellU * rotationState.uAxisY + cellV * rotationState.vAxisY
          val cellZ = rotationState.originZ * (gridSize - 1) + cellU * rotationState.uAxisZ + cellV * rotationState.vAxisZ
          extractTransitionCell(cellX, cellY, cellZ, fullSide, cellU, cellV)
        }
      }
    }
  }

  private def setRotationFor(fullSide: Side): Unit = {
    rotationState.setFor(fullSide)
  }

  private def extractRegularCell(cellX: Int, cellY: Int, cellZ: Int): Seq[Nothing] = {
    val caseNumber = regularCellCase(cellX, cellY, cellZ)
    val cellClass = TransvoxelTables.GetRegularCellClass(caseNumber)
    val triangulationInfo = TransvoxelTables.GetRegularCellData(cellClass)
    val vertexData = TransvoxelTables.GetRegularVertexData(caseNumber)
    val cellVerticesIndices = vertexData.map { vd: RegularVertexData => getRegularVertex(cellX, cellY, cellZ, vd) }
    for (i <- 0 until triangulationInfo.triangleCount) {
      val i1 = triangulationInfo.triangleIndices(3 * i)
      val i2 = triangulationInfo.triangleIndices(3 * i + 1)
      val i3 = triangulationInfo.triangleIndices(3 * i + 2)
      val v1Index = cellVerticesIndices(i1)
      val v2Index = cellVerticesIndices(i2)
      val v3Index = cellVerticesIndices(i3)
      triIndices += v1Index
      triIndices += v2Index
      triIndices += v3Index
    }
    Nil
  }

  private def regularCellCase(cellX: Int, cellY: Int, cellZ: Int): RegularCellCaseNumber = {
    var cornerIndex = 0
    var caseNumber = 0
    for (dz <- zeroToOne; dy <- zeroToOne; dx <- zeroToOne) {
      val inside = getDensity(cellX + dx, cellY + dy, cellZ + dz) > threshHold
      caseNumber += (if (inside) {
        1 << cornerIndex
      } else 0)
      cornerIndex += 1
    }
    new RegularCellCaseNumber(caseNumber)
  }

  // Despite the name "regular", regular vertices and corners *can* be shifted, if in a cell at the border of a transition
  private def loadRegularCornerPosition(cellX: Int, cellY: Int, cellZ: Int, corner: Corner): Unit = {
    corner.x = region.baseX + cellX * cellSize
    corner.y = region.baseY + cellY * cellSize
    corner.z = region.baseZ + cellZ * cellSize
    if (canShrink(cellX, cellY, cellZ)) {
      if ((cellX == 0) && transitionSides.contains(Sides.NegativeX)) {
        corner.x += shrink
      } else if ((cellX == gridSize) && transitionSides.contains(Sides.PositiveX)) {
        corner.x -= shrink
      }
      if ((cellY == 0) && transitionSides.contains(Sides.NegativeY)) {
        corner.y += shrink
      } else if ((cellY == gridSize) && transitionSides.contains(Sides.PositiveY)) {
        corner.y -= shrink
      }
      if ((cellZ == 0) && transitionSides.contains(Sides.NegativeZ)) {
        corner.z += shrink
      } else if ((cellZ == gridSize) && transitionSides.contains(Sides.PositiveZ)) {
        corner.z -= shrink
      }
    }
  }

  private def canShrink(cellX: Int, cellY: Int, cellZ: Int): Boolean = {
    // Do not shrink corner (in any direction) if it's close to a face where the other block is rendered at the same
    // (or lower) level of details (ie: not a transition side)
    val dontShrink = (
      ((cellX == 0) && !transitionSides.contains(Sides.NegativeX))
        || ((cellX == gridSize) && !transitionSides.contains(Sides.PositiveX))
        || ((cellY == 0) && !transitionSides.contains(Sides.NegativeY))
        || ((cellY == gridSize) && !transitionSides.contains(Sides.PositiveY))
        || ((cellZ == 0) && !transitionSides.contains(Sides.NegativeZ))
        || ((cellZ == gridSize) && !transitionSides.contains(Sides.PositiveZ))
      )
    val canShrink = !dontShrink
    canShrink
  }

  private def loadRegularCornerGradient(cellX: Int, cellY: Int, cellZ: Int, corner: Corner): Unit = {
    corner.xGradient = getDensity(cellX + 1, cellY, cellZ) - getDensity(cellX - 1, cellY, cellZ)
    corner.yGradient = getDensity(cellX, cellY + 1, cellZ) - getDensity(cellX, cellY - 1, cellZ)
    corner.zGradient = getDensity(cellX, cellY, cellZ + 1) - getDensity(cellX, cellY, cellZ - 1)
  }

  // Either creates or reuses an existing vertex. Returns its index in the vertices buffer
  private def getRegularVertex(cellX: Int, cellY: Int, cellZ: Int, vd: RegularVertexData): Int = {
    val reuseDx = vd.reuseDx
    val reuseDy = vd.reuseDy
    val reuseDz = vd.reuseDz
    val newVertex = vd.newVertex
    val reusableIndex = vd.reusableIndex
    val previousVertexIsAccessible = ((reuseDx == 0) || (cellX > 0)) && ((reuseDy == 0) || (cellY > 0)) && ((reuseDz == 0) || (cellZ > 0))
    if (newVertex) {
      getNewRegularVertex(cellX, cellY, cellZ, vd.cornerAIndex, vd.cornerBIndex, reusable = true, reusableIndex)
    } else if (previousVertexIsAccessible) {
      sharedVertexIndices.getRegular(cellX + reuseDx, cellY + reuseDy, cellZ + reuseDz, reusableIndex)
    } else {
      // We should reuse an existing vertex but its cell is not accessible (not part of our block)
      getNewRegularVertex(cellX, cellY, cellZ, vd.cornerAIndex, vd.cornerBIndex, reusable = false, 0)
    }
  }

  // Returns the index in vertices buffer
  //   reusableIndex: 0 1 2 or 3 if the vertex is reusable
  private def getNewRegularVertex(cellX: Int, cellY: Int, cellZ: Int, cornerAIndex: Int, cornerBIndex: Int, reusable: Boolean, reusableIndex: Int): Int = {
    createRegularVertex(cellX, cellY, cellZ, cornerAIndex, cornerBIndex)
    val index = vertices - 1
    if (reusable) {
      sharedVertexIndices.putRegular(index, cellX, cellY, cellZ, reusableIndex)
    }
    index
  }

  private val epsilon = 0.000001f

  private def createRegularVertex(cellX: Int, cellY: Int, cellZ: Int, cornerAIndex: Int, cornerBIndex: Int): Unit = {
    val cornerADeltas = regularCorners(cornerAIndex)
    val cornerBDeltas = regularCorners(cornerBIndex)
    loadRegularCornerPosition(cellX + cornerADeltas._1, cellY + cornerADeltas._2, cellZ + cornerADeltas._3, cornerA)
    loadRegularCornerPosition(cellX + cornerBDeltas._1, cellY + cornerBDeltas._2, cellZ + cornerBDeltas._3, cornerB)
    cornerA.density = getDensity(cellX + cornerADeltas._1, cellY + cornerADeltas._2, cellZ + cornerADeltas._3)
    cornerB.density = getDensity(cellX + cornerBDeltas._1, cellY + cornerBDeltas._2, cellZ + cornerBDeltas._3)
    loadRegularCornerGradient(cellX + cornerADeltas._1, cellY + cornerADeltas._2, cellZ + cornerADeltas._3, cornerA)
    loadRegularCornerGradient(cellX + cornerBDeltas._1, cellY + cornerBDeltas._2, cellZ + cornerBDeltas._3, cornerB)
    createVertexFromLoadedCorners()
  }

  private def createVertexFromLoadedCorners(): Unit = {
    val interpToward2 =
      if (Math.abs(cornerB.density - cornerA.density) > epsilon)
        (threshHold - cornerA.density) / (cornerB.density - cornerA.density)
      else
        0.5f
    positionsBuffer += cornerA.x + interpToward2 * (cornerB.x - cornerA.x)
    positionsBuffer += cornerA.y + interpToward2 * (cornerB.y - cornerA.y)
    positionsBuffer += cornerA.z + interpToward2 * (cornerB.z - cornerA.z)

    val xGradient: Float = cornerA.xGradient + interpToward2 * (cornerB.xGradient - cornerA.xGradient)
    val yGradient: Float = cornerA.yGradient + interpToward2 * (cornerB.yGradient - cornerA.yGradient)
    val zGradient: Float = cornerA.zGradient + interpToward2 * (cornerB.zGradient - cornerA.zGradient)
    val gradientNorm = Math.sqrt(xGradient * xGradient + yGradient * yGradient + zGradient * zGradient).toFloat
    normalsBuffer += - xGradient / gradientNorm
    normalsBuffer += - yGradient / gradientNorm
    normalsBuffer += - zGradient / gradientNorm
    vertices += 1
  }

  private val regularCorners = Array(
    (0, 0, 0), // Corner 0 == cell origin
    (1, 0, 0), // Corner 1 == 1 toward X
    (0, 1, 0), // Corner 2 == 1 toward Y
    (1, 1, 0),
    (0, 0, 1),
    (1, 0, 1),
    (0, 1, 1),
    (1, 1, 1)
  )

  private def extractTransitionCell(cellX: Int, cellY: Int, cellZ: Int, fullSide: Side, cellU: Int, cellV: Int): Seq[Nothing] = {
    val cellCase = transitionCellCase(cellX, cellY, cellZ, fullSide)
    val cellClass = TransvoxelTables.GetTransitionCellClass(cellCase)
    val triangulationInfo = TransvoxelTables.GetTransitionCellData(cellClass)
    val vds = TransvoxelTables.GetTransitionVertexData(cellCase)
    val cellVerticesIndices = vds.map { vd: TransitionVertexData => getTransitionVertex(cellX, cellY, cellZ, fullSide, vd, cellU, cellV) }
    for (i <- 0 until triangulationInfo.triangleCount) {
      val i1 = triangulationInfo.triangleIndices(3 * i)
      val i2 = triangulationInfo.triangleIndices(3 * i + 1)
      val i3 = triangulationInfo.triangleIndices(3 * i + 2)
      val v1Index = cellVerticesIndices(i1)
      val v2Index = cellVerticesIndices(i2)
      val v3Index = cellVerticesIndices(i3)
      triIndices += v1Index
      triIndices += v2Index
      triIndices += v3Index
    }
    Nil
  }

  private def transitionCellCase(cellX: Int, cellY: Int, cellZ: Int, fullSide: Side): TransitionCellCaseNumber = {
    var caseNumber = 0
    for ((u, v, contrib) <- transitionFullFaceCaseContributions) {
      val inside = getTransitionDensity(cellX, cellY, cellZ, u, v, 0) > threshHold
      caseNumber += (
        if (inside) {
          contrib
        } else {
          0
        }
        )
    }
    new TransitionCellCaseNumber(caseNumber)
  }

  // u, v, contrib
  private val transitionFullFaceCaseContributions: Array[(Int, Int, Int)] = Array(
    (0, 0, 0x01),
    (1, 0, 0x02),
    (2, 0, 0x04),
    (0, 1, 0x80),
    (1, 1, 0x100),
    (2, 1, 0x08),
    (0, 2, 0x40),
    (1, 2, 0x20),
    (2, 2, 0x10)
  )

  private def getTransitionVertex(cellX: Int, cellY: Int, cellZ: Int, fullSide: Side, vd: TransitionVertexData, cellU: Int, cellV: Int): Int = {
    val reuseDu = vd.reuseDu
    val reuseDv = vd.reuseDv
    if (vd.reuse) {
      val previousVertexIsAccessible = ((reuseDu == 0) || (cellU > 0)) && ((reuseDv == 0) || (cellV > 0))
      if (previousVertexIsAccessible) {
        sharedVertexIndices.getTransition(fullSide, cellU + reuseDu, cellV + reuseDv, vd.reusableIndex)
      } else {
        getNewTransitionVertex(cellX, cellY, cellZ, fullSide, vd.cornerAIndex, vd.cornerBIndex)
      }
    } else {
      val vi = getNewTransitionVertex(cellX, cellY, cellZ, fullSide, vd.cornerAIndex, vd.cornerBIndex)
      if (vd.newReusable) {
        sharedVertexIndices.putTransition(vi, fullSide, cellU, cellV, vd.reusableIndex)
      }
      vi
    }
  }

  // Returns the index in vertices buffer
  private def getNewTransitionVertex(cellX: Int, cellY: Int, cellZ: Int, fullSide: Side, cornerAIndex: Int, cornerBIndex: Int): Int = {
    createTransitionVertex(cellX, cellY, cellZ, fullSide, cornerAIndex, cornerBIndex)
    vertices - 1
  }

  private def createTransitionVertex(cellX: Int, cellY: Int, cellZ: Int, fullSide: Side, cornerAIndex: Int, cornerBIndex: Int): Unit = {
    loadTransitionCornerPosition(cellX, cellY, cellZ, fullSide, cornerAIndex, cornerA)
    loadTransitionCornerPosition(cellX, cellY, cellZ, fullSide, cornerBIndex, cornerB)
    cornerA.density = getTransitionCornerDensity(cellX, cellY, cellZ, fullSide, cornerAIndex)
    cornerB.density = getTransitionCornerDensity(cellX, cellY, cellZ, fullSide, cornerBIndex)
    loadTransitionCornerGradient(cellX, cellY, cellZ, fullSide, cornerAIndex, cornerA)
    loadTransitionCornerGradient(cellX, cellY, cellZ, fullSide, cornerBIndex, cornerB)
    createVertexFromLoadedCorners()
  }

  private def getTransitionCornerDensity(cellX: Int, cellY: Int, cellZ: Int, fullSide: Side, cornerIndex: Int): Float = {
    if (cornerIndex < 9) {
      val (u, v) = transitionFullCorners(cornerIndex)
      getTransitionDensity(cellX, cellY, cellZ, u, v, 0)
    } else {
      val (cellU, cellV) = transitionRegularCorners(cornerIndex - 9)
      getDensity(
        cellX + rotationState.originX + cellU * rotationState.uAxisX + cellV * rotationState.vAxisX,
        cellY + rotationState.originY + cellU * rotationState.uAxisY + cellV * rotationState.vAxisY,
        cellZ + rotationState.originZ + cellU * rotationState.uAxisZ + cellV * rotationState.vAxisZ)
    }
  }

  private def loadTransitionCornerPosition(cellX: Int, cellY: Int, cellZ: Int, fullSide: Side, cornerIndex: Int, corner: Corner): Unit = {
    if (cornerIndex < 9) {
      val (u, v) = transitionFullCorners(cornerIndex)
      corner.x = region.baseX + (cellX + rotationState.originX.toFloat + u * 0.5f * rotationState.uAxisX + v * 0.5f * rotationState.vAxisX) * cellSize
      corner.y = region.baseY + (cellY + rotationState.originY.toFloat + u * 0.5f * rotationState.uAxisY + v * 0.5f * rotationState.vAxisY) * cellSize
      corner.z = region.baseZ + (cellZ + rotationState.originZ.toFloat + u * 0.5f * rotationState.uAxisZ + v * 0.5f * rotationState.vAxisZ) * cellSize
    } else {
      val (cellU, cellV) = transitionRegularCorners(cornerIndex - 9)
      loadRegularCornerPosition(
        cellX + rotationState.originX + cellU * rotationState.uAxisX + cellV * rotationState.vAxisX,
        cellY + rotationState.originY + cellU * rotationState.uAxisY + cellV * rotationState.vAxisY,
        cellZ + rotationState.originZ + cellU * rotationState.uAxisZ + cellV * rotationState.vAxisZ,
        corner)
    }
  }

  private def loadTransitionCornerGradient(cellX: Int, cellY: Int, cellZ: Int, fullSide: Side, cornerIndex: Int, corner: Corner): Unit = {
    if (cornerIndex < 9) {
      val (u, v) = transitionFullCorners(cornerIndex)
      corner.xGradient = getTransitionDensity(cellX, cellY, cellZ, u + rotationState.xAxisU, v + rotationState.xAxisV, rotationState.xAxisW) - getTransitionDensity(cellX, cellY, cellZ, u - rotationState.xAxisU, v - rotationState.xAxisV, - rotationState.xAxisW)
      corner.yGradient = getTransitionDensity(cellX, cellY, cellZ, u + rotationState.yAxisU, v + rotationState.yAxisV, rotationState.yAxisW) - getTransitionDensity(cellX, cellY, cellZ, u - rotationState.yAxisU, v - rotationState.yAxisV, - rotationState.yAxisW)
      corner.zGradient = getTransitionDensity(cellX, cellY, cellZ, u + rotationState.zAxisU, v + rotationState.zAxisV, rotationState.zAxisW) - getTransitionDensity(cellX, cellY, cellZ, u - rotationState.zAxisU, v - rotationState.zAxisV, - rotationState.zAxisW)
    } else {
      val (cellU, cellV) = transitionRegularCorners(cornerIndex - 9)
      val cellXdelta = rotationState.originX + cellU * rotationState.uAxisX + cellV * rotationState.vAxisX
      val cellYdelta = rotationState.originY + cellU * rotationState.uAxisY + cellV * rotationState.vAxisY
      val cellZdelta = rotationState.originZ + cellU * rotationState.uAxisZ + cellV * rotationState.vAxisZ
      loadRegularCornerGradient(cellX + cellXdelta, cellY + cellYdelta, cellZ + cellZdelta, corner)
    }
  }

  // Index < 9 : transitionFullCorners
  // Index >= 9 : transitionRegularCorners

  // cornerIndex => u, v
  private val transitionFullCorners: Array[(Int, Int)] = Array(
    (0,   0),
    (1,   0),
    (2,   0),
    (0,   1),
    (1,   1),
    (2,   1),
    (0,   2),
    (1,   2),
    (2,   2)
  )

  // cornerIndex - 9 => cellU, cellV
  private val transitionRegularCorners: Array[(Int, Int)] = Array(
    (0,   0),
    (1,   0),
    (0,   1),
    (1,   1)
  )

}


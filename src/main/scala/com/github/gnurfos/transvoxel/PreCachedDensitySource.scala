package com.github.gnurfos.transvoxel

import com.github.gnurfos.transvoxel.internal.RotationState


/**
  * Density source that pre-caches all possibly queried values in arrays
  *
  * This induces some memory allocations, but tries to minimize the number of calls for a single position.
  * (There can still be a small amount of duplicates, when several transition faces overlap).
  * Also, this is "pessimistic", and depending on the extracted surface, some of the values might not be used at all.
  */
class PreCachedDensitySource(
                            field: ScalarField,
                            region: Block,
                            gridSize: Int,
                            transitionSides: Sides.ValueSet) extends DensitySource(gridSize) {

  private val cellSize: Float = region.size / gridSize

  private val regularDensityCache = new Array[Float]((gridSize + 3) * (gridSize + 3) * (gridSize + 3))
  private var shiftedCellX = 0
  while (shiftedCellX <= gridSize + 2) {
    var shiftedCellY = 0
    while (shiftedCellY <= gridSize + 2) {
      var shiftedCellZ = 0
      while (shiftedCellZ <= gridSize + 2) {
        val x = region.baseX + (shiftedCellX - 1) * cellSize
        val y = region.baseY + (shiftedCellY - 1) * cellSize
        val z = region.baseZ + (shiftedCellZ - 1) * cellSize
        val index = shiftedCellX + (gridSize + 3) * shiftedCellY + (gridSize + 3) * (gridSize + 3) * shiftedCellZ
        regularDensityCache(index) = field.getDensity(x, y, z)
        shiftedCellZ += 1
      }
      shiftedCellY += 1
    }
    shiftedCellX += 1
  }

  def getDensity(cellX: Int, cellY: Int, cellZ: Int): Float = {
    val index = (cellX + 1) + (gridSize + 3) * (cellY + 1) + (gridSize + 3) * (gridSize + 3) * (cellZ + 1)
    regularDensityCache(index)
  }

  private val transitionDensityCache = new Array[Float](Sides.maxId * (2 * gridSize + 3) * (2 * gridSize + 3) * 3)
  private val sideIterator = transitionSides.iterator
  while (sideIterator.hasNext) {
    val side = sideIterator.next()
    val (xyzForBase, xyzForU, xyzForV, xyzForW) = RotationState.transitionFullFaceOrientation(side)
    var shiftedU = 0
    while (shiftedU <= 2 * gridSize + 2) {
      var shiftedV = 0
      while (shiftedV <= 2 * gridSize + 2) {
        var shiftedW = 0
        while (shiftedW <= 2) {
          val index = side.id + Sides.maxId * shiftedU + Sides.maxId * (2 * gridSize + 3) * shiftedV + Sides.maxId * (2 * gridSize + 3) * (2 * gridSize + 3) * shiftedW
          if ((shiftedW == 1) && (shiftedU % 2 == 1) && (shiftedV % 2 == 1)) {
            val cellX = xyzForBase._1 * gridSize + ((shiftedU - 1) / 2) * xyzForU._1 + ((shiftedV - 1) / 2) * xyzForV._1
            val cellY = xyzForBase._2 * gridSize + ((shiftedU - 1) / 2) * xyzForU._2 + ((shiftedV - 1) / 2) * xyzForV._2
            val cellZ = xyzForBase._3 * gridSize + ((shiftedU - 1) / 2) * xyzForU._3 + ((shiftedV - 1) / 2) * xyzForV._3
            transitionDensityCache(index) = getDensity(cellX, cellY, cellZ)
          } else {
            val x = region.baseX + (xyzForBase._1 * gridSize + (shiftedU - 1) * 0.5f * xyzForU._1 + (shiftedV - 1) * 0.5f * xyzForV._1 + (shiftedW - 1) * 0.5f * xyzForW._1) * cellSize
            val y = region.baseY + (xyzForBase._2 * gridSize + (shiftedU - 1) * 0.5f * xyzForU._2 + (shiftedV - 1) * 0.5f * xyzForV._2 + (shiftedW - 1) * 0.5f * xyzForW._2) * cellSize
            val z = region.baseZ + (xyzForBase._3 * gridSize + (shiftedU - 1) * 0.5f * xyzForU._3 + (shiftedV - 1) * 0.5f * xyzForV._3 + (shiftedW - 1) * 0.5f * xyzForW._3) * cellSize
            transitionDensityCache(index) = field.getDensity(x, y, z)
          }
          shiftedW += 1
        }
        shiftedV += 1
      }
      shiftedU += 1
    }
  }

  override def getTransitionDensity(cellX: Int, cellY: Int, cellZ: Int, rotationState: RotationState, u: Int, v: Int, w: Int): Float = {
    val cellOriginU = 2 * (rotationState.originU * (gridSize - 1) + cellX * rotationState.xAxisU + cellY * rotationState.yAxisU + cellZ * rotationState.zAxisU)
    val cellOriginV = 2 * (rotationState.originV * (gridSize - 1) + cellX * rotationState.xAxisV + cellY * rotationState.yAxisV + cellZ * rotationState.zAxisV)
    val cellOriginW = 0
    val shiftedU = cellOriginU + u + 1
    val shiftedV = cellOriginV + v + 1
    val shiftedW = cellOriginW + w + 1
    val index = rotationState.transitionSide.id + Sides.maxId * shiftedU + Sides.maxId * (2 * gridSize + 3) * shiftedV + Sides.maxId * (2 * gridSize + 3) * (2 * gridSize + 3) * shiftedW
    transitionDensityCache(index)
  }

}

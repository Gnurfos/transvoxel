package com.github.gnurfos.transvoxel

import com.github.gnurfos.transvoxel.internal.RotationState


/**
  * Density source doing no caching. Every time density is queried, the underlying ScalarField is called.
  *
  * This can lead to several calls for the same position during a single block extraction, but induces no
  * big memory overhead.
  */
class PassThroughDensitySource(
                                        field: ScalarField,
                                        region: Block,
                                        gridSize: Int) extends DensitySource(gridSize) {

  private val cellSize: Float = region.size / gridSize

  override def getDensity(cellX: Int, cellY: Int, cellZ: Int): Float = {
    val x = region.baseX + cellX * cellSize
    val y = region.baseY + cellY * cellSize
    val z = region.baseZ + cellZ * cellSize
    field.getDensity(x, y, z)
  }

  override def getTransitionDensity(cellX: Int, cellY: Int, cellZ: Int, rotationState: RotationState, u: Int, v: Int, w: Int): Float = {
    val cellOriginU = 2 * (rotationState.originU * (gridSize - 1) + cellX * rotationState.xAxisU + cellY * rotationState.yAxisU + cellZ * rotationState.zAxisU)
    val cellOriginV = 2 * (rotationState.originV * (gridSize - 1) + cellX * rotationState.xAxisV + cellY * rotationState.yAxisV + cellZ * rotationState.zAxisV)
    val x = region.baseX + (rotationState.originX * gridSize + (u + cellOriginU) * 0.5f * rotationState.uAxisX + (v + cellOriginV) * 0.5f * rotationState.vAxisX + w * 0.5f * rotationState.wAxisX) * cellSize
    val y = region.baseY + (rotationState.originY * gridSize + (u + cellOriginU) * 0.5f * rotationState.uAxisY + (v + cellOriginV) * 0.5f * rotationState.vAxisY + w * 0.5f * rotationState.wAxisY) * cellSize
    val z = region.baseZ + (rotationState.originZ * gridSize + (u + cellOriginU) * 0.5f * rotationState.uAxisZ + (v + cellOriginV) * 0.5f * rotationState.vAxisZ + w * 0.5f * rotationState.wAxisZ) * cellSize
    field.getDensity(x, y, z)
  }



}

package com.github.gnurfos.transvoxel

import com.github.gnurfos.transvoxel.internal.RotationState


/**
  * Wrapper used to retrieve density values (typically from a [[ScalarField]])
  */
abstract class DensitySource(val gridSize: Int) {

  /**
    * This method will be called by the extraction algorithm
    *
    * It should return the density, at the "base" (lowest corner) of the cell identified by the given xyz-indices
    *
    * Note that, when computing gradients, this may be queried for cells outside of the extracted block:
    *  - an index of 0 indicates the base of the block
    *  - an index of gridSize indicates the end of the block
    *  - the function can be called for index values between -1 and gridSize + 1 (both included)
    *
    * @param cellX x-index of the cell
    * @param cellY y-index of the cell
    * @param cellZ z-index of the cell
    * @return the density at the queried point
    */
  def getDensity(cellX: Int, cellY: Int, cellZ: Int): Float

  /**
    * This method will be called by the extraction algorithm
    * The default implementation only serves to allow subclasses to implement a simpler method (not using [[internal.RotationState]])
    *
    * About uv-indices (coordinates on the transition face):
    *  - an index of 0 indicates the base of the cell
    *  - an index of 1 indicates the middle of the cell
    *  - an index of 2 indicates the end of the cell
    *  - the function can be called for index values between -1 and 3 (gradient computations necessitate inspecting further away)
    *
    *  About w-index (coordinate perpendicular to the transition face):
    *  - an index of 0 indicates a point on the transition face
    *  - the function can be called for index values between -1 and 1
    *
    * @param cellX x-index of the cell
    * @param cellY y-index of the cell
    * @param cellZ z-index of the cell
    * @param rotationState object indicating what side of the cell is the transition side, and helping rotation calculations
    * @param u u-index on the transition face, within the cell
    * @param v v-index on the transition face, within the cell
    * @param w w-index
    * @return the density at the queried point
    */
  def getTransitionDensity(cellX: Int, cellY: Int, cellZ: Int, rotationState: RotationState, u: Int, v: Int, w: Int): Float =
    getTransitionDensity(cellX, cellY, cellZ, rotationState.transitionSide, u, v, w)

  /**
    * This method is only called by the above and can be implemented instead, for convenience
    */
  def getTransitionDensity(cellX: Int, cellY: Int, cellZ: Int, side: Sides.Value, u: Int, v: Int, w: Int): Float = ???

}


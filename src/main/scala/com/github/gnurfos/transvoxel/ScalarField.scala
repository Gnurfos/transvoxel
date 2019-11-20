package com.github.gnurfos.transvoxel

/**
  * Provider of density values, as a function of space location
  */
trait ScalarField {
  def getDensity(x: Float, y: Float, z: Float) : Float
}

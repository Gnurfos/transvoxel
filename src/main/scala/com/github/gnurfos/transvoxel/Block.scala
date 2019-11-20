package com.github.gnurfos.transvoxel

/**
  * Coordinates of a cubic space for which a mesh should be produced
  * @param baseX X coordinate of the cube origin
  * @param baseY Y coordinate of the cube origin
  * @param baseZ Z coordinate of the cube origin
  * @param size Size of the cube edges
  */
case class Block(baseX: Float, baseY: Float, baseZ: Float, size: Float)

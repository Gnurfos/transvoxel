package com.github.gnurfos

/**
  * Implementation of Eric Lengyel's Transvoxel Algorithm in scala/java.
  *
  * The main entry point is [[com.github.gnurfos.transvoxel.Transvoxel.extract]]

  * Credits:
  *
  * Eric Lengyel's Transvoxel Algorithm.
  *
  * [[https://transvoxel.org/]]
  *
  */
package object transvoxel {
  /**
    * Enumeration for all 6 sides of a cube
    */
  type Side = Sides.Side
}

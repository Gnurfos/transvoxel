package com.github.gnurfos.transvoxel.internal

import com.github.gnurfos.transvoxel.Sides

object RotationState {

  // For a given full face, gives world position and orientation of positive U/V/W
  val transitionFullFaceOrientation = Map(
    //     Side    -> (cell_origin_shift, U_direction, V_direction, W_direction)
    Sides.NegativeZ -> ((0, 0, 0), (1, 0, 0), (0, 1, 0), (0, 0, 1)), // U: +X, V: +Y, W: +Z
    Sides.PositiveZ -> ((1, 0, 1), (-1, 0, 0), (0, 1, 0), (0, 0, -1)),
    Sides.NegativeY -> ((0, 0, 1), (1, 0, 0), (0, 0, -1), (0, 1, 0)), // U: +X, V: -Z, W: +Y
    Sides.PositiveY -> ((0, 1, 0), (1, 0, 0), (0, 0, 1), (0, -1, 0)),
    Sides.NegativeX -> ((0, 0, 1), (0, 0, -1), (0, 1, 0), (1, 0, 0)),
    Sides.PositiveX -> ((1, 0, 0), (0, 0, 1), (0, 1, 0), (-1, 0, 0))
  )

  //
  val transitionReverseOrientation = Map(
    //     Side    -> (UVW_of_origin, UVW_for_x, UVW_for_y, UVW_for_z)
    Sides.NegativeZ -> ((0, 0, 0), (1, 0, 0), (0, 1, 0), (0, 0, 1)),
    Sides.PositiveZ -> ((1, 0, 1), (-1, 0, 0), (0, 1, 0), (0, 0, -1)),
    Sides.NegativeY -> ((0, 1, 0), (1, 0, 0), (0, 0, 1), (0, -1, 0)),
    Sides.PositiveY -> ((0, 0, 1), (1, 0, 0), (0, 0, -1), (0, 1, 0)),
    Sides.NegativeX -> ((1, 0, 0), (0, 0, 1), (0, 1, 0), (-1, 0, 0)),  // +X is +W, +Y is +V, +Z is -U
    Sides.PositiveX -> ((0, 0, 1), (0, 0, -1), (0, 1, 0), (1, 0, 0))
  )
}

class RotationState {
  var transitionSide: Sides.Value = Sides.PositiveX
  // UVWs
  var originU = 0
  var originV = 0
  var originW = 0
  var xAxisU = 0
  var xAxisV = 0
  var xAxisW = 0
  var yAxisU = 0
  var yAxisV = 0
  var yAxisW = 0
  var zAxisU = 0
  var zAxisV = 0
  var zAxisW = 0
  // XYZs
  var originX = 0
  var originY = 0
  var originZ = 0
  var uAxisX = 0
  var uAxisY = 0
  var uAxisZ = 0
  var vAxisX = 0
  var vAxisY = 0
  var vAxisZ = 0
  var wAxisX = 0
  var wAxisY = 0
  var wAxisZ = 0

  def setFor(fullSide: Sides.Value): Unit = {
    val uvws = RotationState.transitionReverseOrientation(fullSide)
    val xyzs = RotationState.transitionFullFaceOrientation(fullSide)
    transitionSide = fullSide
    originU = uvws._1._1
    originV = uvws._1._2
    originW = uvws._1._3
    xAxisU = uvws._2._1
    xAxisV = uvws._2._2
    xAxisW = uvws._2._3
    yAxisU = uvws._3._1
    yAxisV = uvws._3._2
    yAxisW = uvws._3._3
    zAxisU = uvws._4._1
    zAxisV = uvws._4._2
    zAxisW = uvws._4._3
    originX = xyzs._1._1
    originY = xyzs._1._2
    originZ = xyzs._1._3
    uAxisX = xyzs._2._1
    uAxisY = xyzs._2._2
    uAxisZ = xyzs._2._3
    vAxisX = xyzs._3._1
    vAxisY = xyzs._3._2
    vAxisZ = xyzs._3._3
    wAxisX = xyzs._4._1
    wAxisY = xyzs._4._2
    wAxisZ = xyzs._4._3

  }

}

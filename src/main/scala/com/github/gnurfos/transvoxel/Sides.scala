package com.github.gnurfos.transvoxel

object Sides extends Enumeration {

  type Side = Value
  val NegativeX: Value = Value("-x")
  val PositiveX: Value = Value("+x")
  val NegativeY: Value = Value("-y")
  val PositiveY: Value = Value("+y")
  val NegativeZ: Value = Value("-z")
  val PositiveZ: Value = Value("+z")

  // Convenience shortcuts, mostly for java ease of use
  type Set = ValueSet
  def none(): Set = ValueSet.empty
  def all(): Set = values
  def from(sides: Array[Side]): Set = ValueSet(sides: _*)
}


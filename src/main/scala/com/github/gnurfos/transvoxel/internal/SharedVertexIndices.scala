package com.github.gnurfos.transvoxel.internal

import com.github.gnurfos.transvoxel.{Side, Sides}

/**
  * Store indices in the vertex buffer
  */
class SharedVertexIndices(gridSize: Int) {

  // For a given cell and "reusableIndex" (0 to 3): index (in vertices) of the vertex on that edge
  private val regular = new Array[Int](gridSize * gridSize * gridSize * 4)

  def getRegular(cellX: Int, cellY: Int, cellZ: Int, reusableIndex: Int): Int = {
    val storageIndex = cellX + gridSize * cellY + gridSize * gridSize * cellZ + gridSize * gridSize * gridSize * reusableIndex
    regular(storageIndex)
  }

  def putRegular(vertexIndex: Int, cellX: Int, cellY: Int, cellZ: Int, reusableIndex: Int): Unit = {
    val storageIndex = cellX + gridSize * cellY + gridSize * gridSize * cellZ + gridSize * gridSize * gridSize * reusableIndex
    regular(storageIndex) = vertexIndex
  }

  // For a given cell and "reusableIndex" (0 to 9): index (in vertices) of the vertex on that edge
  private val transition = new Array[Int](Sides.maxId * gridSize * gridSize * 10)

  def getTransition(fullSide: Side, cellU: Int, cellV: Int, reusableIndex: Int): Int = {
    val storageIndex = fullSide.id + Sides.maxId * cellU + Sides.maxId * gridSize * cellV + Sides.maxId * gridSize * gridSize * reusableIndex
    transition(storageIndex)
  }

  def putTransition(vertexIndex: Int, fullSide: Side, cellU: Int, cellV: Int, reusableIndex: Int): Unit = {
    val storageIndex = fullSide.id + Sides.maxId * cellU + Sides.maxId * gridSize * cellV + Sides.maxId * gridSize * gridSize * reusableIndex
    transition(storageIndex) = vertexIndex
  }

}

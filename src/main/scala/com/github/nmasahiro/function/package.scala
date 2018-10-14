package com.github.nmasahiro

import breeze.linalg._


package object function {

  type V = DenseVector[Double]

  type StopCondition = PartialFunction[(Int, Double), Boolean]

  val proceed: StopCondition = {
    case (itr: Int, value: Double) => false
  }

  def iterationsExceed(maxItr: Int): StopCondition = {
    case (itr: Int, _) if itr >= maxItr => true
  }

  def minEvalReached(minValue: Double): StopCondition = {
    case (_, value: Double) if value < minValue => true
  }
}


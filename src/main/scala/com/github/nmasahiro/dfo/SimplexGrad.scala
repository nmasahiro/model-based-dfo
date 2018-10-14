package com.github.nmasahiro.dfo

import breeze.linalg._
import breeze.stats.distributions.Rand
import com.github.nmasahiro.function.{FunctionBase, V}

object SimplexGrad {

  def apply(y0: V, f0: Double, Δ: Double, f: FunctionBase): DenseVector[Double] = {
    val n = y0.length
    val Y = y0 :: List.fill[V](n)(DenseVector.zeros[Double](n)).map {
      v => y0 + v.map(_ => (Rand.uniform.draw() * 2 - 1) * Δ)
    }
    val L = new DenseMatrix(n, n, (1 until n+1).map(i => Y(i) - Y.head).flatMap(_.toScalaVector).toArray)
    val invL_T = inv(L)
    val δfY = DenseVector((1 until n+1).map(i => f.evaluate(Y(i)) - f0).toArray)
    val alpha = invL_T * δfY
    alpha
  }

}

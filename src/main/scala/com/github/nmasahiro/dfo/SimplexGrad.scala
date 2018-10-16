/*
 * Copyright (c) 2018 Masahiro Nomura
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.github.nmasahiro.dfo

import breeze.linalg._
import breeze.stats.distributions.Rand
import com.github.nmasahiro.function.V

object SimplexGrad {

  def apply(y0: V, f0: Double, Δ: Double, f: V => Double): DenseVector[Double] = {
    val n = y0.length
    val Y = y0 :: List.fill[V](n)(DenseVector.zeros[Double](n)).map {
      v => y0 + v.map(_ => (Rand.uniform.draw() * 2 - 1) * Δ)
    }
    val L = new DenseMatrix(n, n, (1 until n+1).map(i => Y(i) - Y.head).flatMap(_.toScalaVector).toArray)
    val invL_T = inv(L)
    val δfY = DenseVector((1 until n+1).map(i => f(Y(i)) - f0).toArray)
    val alpha = invL_T * δfY
    alpha
  }

}

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

import breeze.linalg.norm
import com.github.nmasahiro.function._

import scala.annotation.tailrec

case class DFODriver(private val f: V => Double, η: Double, ε_stop: Double, stopCondition: StopCondition) {

  @tailrec
  final def optimize(k: Int, x: V, Δ: Double, μ: Double): V = {
    val fx = f(x)
    // StopCondition check
    if (stopCondition.apply((k, fx))) {
      println(s"stopCondition. k:$k, fx:$fx")
      x
    } else {
      // 1. Model:
      println(s"x:$x, fx:$fx")
      val approxGrad = SimplexGrad(x, fx, Δ, f)
      // 2. Model accuracy checks:
      if (Δ < ε_stop && norm(approxGrad) < ε_stop) {
        println("Algorithm success and stop.")
        x
      } else if (Δ > μ * norm(approxGrad)) {
        println("model inaccurate")
        optimize(k + 1, x, Δ / 2.0, μ)
      } else {
        // 3. Line search
        val d = - approxGrad
        val (flagLineSearch, t) = ArmijoLineSearch(f, x, d, approxGrad, η)
        if (flagLineSearch)
          optimize(k + 1, x + t * d, Δ, μ)
        else
          optimize(k + 1, x, Δ, μ / 2.0)
      }
    }
  }
}

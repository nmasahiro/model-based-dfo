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

import com.github.nmasahiro.function._
import breeze.linalg._

object ArmijoLineSearch {

  def apply(f: FunctionBase, x: V, d: V, approxGrad: V, η: Double): (Boolean, Double) = {
    var successFlag = true
    // 0. Initialise:
    var τ = 1.0
    val N_max = 100
    val ip_d_arpproxGrad = d.t * approxGrad
    // 1. Forward-backward search:
    val fx = f.evaluate(x)
    if (f.evaluate(x + τ * d) < fx + η * τ * ip_d_arpproxGrad) {
      successFlag = true
      τ = 1.0
      while (τ <= math.pow(2, N_max) && f.evaluate(x + τ * d) < fx + η * τ * ip_d_arpproxGrad) {
        τ *= 2.0
      }
      τ /= 2.0
    } else {
      successFlag = false
      while (τ >= math.pow(2, - N_max) && f.evaluate(x + τ * d) >= fx + η * τ * ip_d_arpproxGrad) {
        τ /= 2.0
      }
      if (f.evaluate(x + τ * d) < fx + η * τ * ip_d_arpproxGrad)
        successFlag = true
    }
    // 2. Output:
    if (successFlag)
      (successFlag, τ)
    else
      (successFlag, -1.0)
  }

}

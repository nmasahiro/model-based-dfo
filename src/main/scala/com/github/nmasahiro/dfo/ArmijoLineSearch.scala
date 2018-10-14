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

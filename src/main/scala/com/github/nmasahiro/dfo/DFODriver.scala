package com.github.nmasahiro.dfo

import breeze.linalg.norm
import com.github.nmasahiro.function._

import scala.annotation.tailrec

case class DFODriver(private val f: FunctionBase, η: Double, ε_stop: Double, stopCondition: StopCondition) {

  @tailrec
  final def optimize(k: Int, x: V, Δ: Double, μ: Double): V = {
    val fx = f.evaluate(x)
    // StopCondition check
    if (stopCondition.apply(k, fx)) {
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

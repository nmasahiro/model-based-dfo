package com.github.nmasahiro.dfo

import breeze.linalg.DenseVector
import org.scalatest.{FunSuite, Matchers}
import com.github.nmasahiro.function.Benchmark._
import com.github.nmasahiro.function._

class SimplexGradTest extends FunSuite with Matchers {

  val f: V => Double = (x: V) => sphere(x)

  test("SimplexGrad test") {

    // initial model accuracy parameter
    val Δ = 1.0
    // initial target accuracy parameter
    val μ = 1.0
    // an Armijo parameter
    val η = 0.05
    // minimum decrease angle parameter
    // val ε_d = 0.0 // not use now (because d = - approx_grad is alwayls descent direction)
    // stopping tolerance
    val ε_stop = 1e-4
    // iteration counter
    val K = 200

    // initial point
    val x = DenseVector(Array(1.0, 1.0))
    // min evaluation value
    val minValue = 1e-5

    val driver = DFODriver(f, η, ε_stop,
      iterationsExceed(K)
        orElse minEvalReached(minValue)
        orElse proceed)

    val expected = Array(0.0, 0.0)

    val result = driver.optimize(0, x, Δ, μ).toArray

    for (i <- result.indices) result(i) should be (expected(i) +- 0.1)
  }


}

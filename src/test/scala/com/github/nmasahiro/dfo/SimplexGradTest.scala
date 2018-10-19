package com.github.nmasahiro.dfo

import breeze.linalg.DenseVector
import org.scalatest.{FunSuite, Matchers}
import com.github.nmasahiro.function.Benchmark._
import com.github.nmasahiro.function._
import com.typesafe.config.ConfigFactory

class SimplexGradTest extends FunSuite with Matchers {

  test("SimplexGrad test") {

    val conf = ConfigFactory.load()

    // objective function
    val f: V => Double = (x: V) => conf.getString("dfo.test-common.func") match {
      case "sphere" => sphere(x)
      case "ellipsoid" => ellipsoid(x)
      case "rosenbrock-chain" => rosenbrockChain(x)
      case _ => sphere(x)
    }

    // initial model accuracy parameter
    val Δ = conf.getDouble("dfo.test-common.Δ")

    // initial target accuracy parameter
    val μ = conf.getDouble("dfo.test-common.μ")

    // an Armijo parameter
    val η = conf.getDouble("dfo.test-common.η")

    // minimum decrease angle parameter
    // val ε_d = 0.0 // not use now (because d = - approx_grad is alwayls descent direction)

    // stopping tolerance
    val ε_stop = conf.getDouble("dfo.test-common.ε_stop")

    // iteration counter
    val K = conf.getInt("dfo.test-common.K")

    // min evaluation value
    val minValue = conf.getDouble("dfo.test-common.min_value")

    // verbose
    val verbose = conf.getBoolean("dfo.test-common.verbose")

    // initial point
    val x = DenseVector(Array(1.0, 1.0))

    val driver = DFODriver(f, η, ε_stop,
      iterationsExceed(K)
        orElse minEvalReached(minValue)
        orElse proceed,
      verbose)

    val expected = Array(0.0, 0.0)

    val result = driver.optimize(0, x, Δ, μ).toArray

    for (i <- result.indices) result(i) should be (expected(i) +- 0.1)
  }


}

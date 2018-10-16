package com.github.nmasahiro.function

import breeze.linalg.sum

object Benchmark {

  def sphere(x: V): Double = sum(x.map(xi => xi * xi))

  def ellipsoid(x: V): Double = (0 until x.length).map { i =>
    math.pow(1000.0, i / (x.length - 1) ) * x(i)
  }.map(xi => xi * xi).sum

  def rosenbrockChain(x: V): Double = (0 until x.length - 1).map { i =>
    100 * math.pow(x(i) - (x(i) * x(i)), 2.0 ) + math.pow(x(i) - 1, 2.0 )
  }.sum

}

package com.github.nmasahiro.function

import breeze.linalg.sum

object Benchmark {

  def sphere(x: V): Double = sum(x.map(xi => xi * xi))

}

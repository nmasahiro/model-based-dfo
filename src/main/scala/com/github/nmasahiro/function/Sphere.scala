package com.github.nmasahiro.function

import breeze.linalg.sum

class Sphere extends FunctionBase {

  override def evaluate(x: V): Double = sum(x.map(xi => xi * xi))

}

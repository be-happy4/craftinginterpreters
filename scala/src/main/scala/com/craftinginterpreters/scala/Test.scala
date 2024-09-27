package com.craftinginterpreters.scala

import com.craftinginterpreters.scala.TestQuotes.oddEven

import scala.math.Numeric.BigIntIsIntegral.IntegralOps

trait Param {
  type Result
  val result: Result
}

def post(param: Param): param.Result =
  param.result

case class AVo(a: String = "a")

class ADto(override val result: AVo) extends Param:
  override type Result = AVo

case class BVo(b: String = "b")

class BDto(override val result: BVo) extends Param:
  override type Result = BVo

object Test {
  def main(args: Array[String]): Unit = {
    val avo: ADto#Result = post(ADto(AVo()))
    val bvo: BDto#Result = post(BDto(BVo()))
    println(avo)
    println(bvo)
  }

  inline def v(x: Int) = ${ oddEven('x) }

  type N = IntegralOps
}

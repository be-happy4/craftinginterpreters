package com.craftinginterpreters.scala

extension (x: String)
  def convertToUnicode: String = x.map { c =>
    if (c > 127) f"\\u${c.toInt}%04x" else c
  }.mkString

object Test:
  def f =
    if (1 == 1) 2 else ()
  def f1(): Unit =
    if (1 == 1) 2 else ()
  def f2(): Any =
    if (1 == 1) 2 else ()
  def g =
    if (1 == 1) 2
  def g1(): Unit =
    if (1 == 1) 2
  def g2(): Any =
    if (1 == 1) 2

  def main(args: Array[String]): Unit =
    var x = if (1 == 1) 2 else ()
    var x1: Unit = if (1 == 1) 2 else ()
    var x2: Any = if (1 == 1) 2 else ()

    var y = if (1 == 1) 2
    var y1: Unit = if (1 == 1) 2
    var y2: Any = if (1 == 1) 2

    println(x1)
    println(y1)
    println(x2)
    println(y2)


  def unit: Any = {
    for i <- 1 until 10 do
      i
  }

  def convertJsonToUnicode(json: ujson.Value): ujson.Value = json match
    case ujson.Str(s) => ujson.Str(s.convertToUnicode)
    case ujson.Obj(obj) =>
      val items = obj.map { case (k, v) => k.convertToUnicode -> convertJsonToUnicode(v) }
      ujson.Obj(items.head, items.tail.toSeq *)
    case ujson.Arr(arr) =>
      ujson.Arr(arr.map(convertJsonToUnicode).toSeq *)
    case other => other // For numbers, booleans, and null, leave them unchanged


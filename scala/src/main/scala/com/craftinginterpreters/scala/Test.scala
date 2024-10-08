package com.craftinginterpreters.scala

extension (x: String)
  def convertToUnicode: String = x.map { c =>
    if (c > 127) f"\\u${c.toInt}%04x" else c
  }.mkString

object Test:
  def main(args: Array[String]): Unit =
    //    val json = ujson.read(os.read(os.pwd / "a.json"))
    //    println(json)
    //    println(convertJsonToUnicode(json))
    //    println(json.render(2, true))
    //    val context = params + accessKey;
    val x: Unit = identity(())
    println(x)
    println(x.getClass)
    val y: Unit = unit
    println(y)
    println(y.getClass)

  def unit: Unit = {}

  def convertJsonToUnicode(json: ujson.Value): ujson.Value = json match
    case ujson.Str(s) => ujson.Str(s.convertToUnicode)
    case ujson.Obj(obj) =>
      val items = obj.map { case (k, v) => k.convertToUnicode -> convertJsonToUnicode(v) }
      ujson.Obj(items.head, items.tail.toSeq *)
    case ujson.Arr(arr) =>
      ujson.Arr(arr.map(convertJsonToUnicode).toSeq *)
    case other => other // For numbers, booleans, and null, leave them unchanged


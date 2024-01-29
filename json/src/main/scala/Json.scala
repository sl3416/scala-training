package com.jpmc.json

sealed trait Json extends Product with Serializable

object Json {
  /* Type alias:
  * Either returns an error msg (String) or correctly decoded Json (A) e.g. JSONBoolean decoded to Boolean
  */
  type Result[A] = Either[java.lang.String, A]

  // Creation helpers
  def apply(int: scala.Int): Json = Json.Int(int)
  def apply(str: java.lang.String): Json = Json.String(str)
  def apply(bool: scala.Boolean): Json = Json.Boolean(bool)
  def apply(pairs: (java.lang.String, Json)*): Json.Object = Json.Object(pairs.toMap)
  def apply(values: Json*): Json.Array = Json.Array(values.toList)

  // Json ADTs
  final case class User(name: java.lang.String, age: Int, enabled: Boolean)
  final case class String(value: java.lang.String) extends Json
  final case class Int(value: scala.Int) extends Json
  final case class Boolean(value: scala.Boolean) extends Json
  final case object Null extends Json
  sealed trait Document extends Json
  final case class Object(data: Map[java.lang.String, Json]) extends Document
  final case class Array(data: List[Json]) extends Document

  /** Print a Json into human-readable format */
  def pprint(json: Json): java.lang.String = json match {
    case Json.String(value)  => s""""$value""""
    case Json.Int(value)     => value.toString
    case Json.Boolean(value) => value.toString
    case Json.Null           => "null"
    case doc: Document       => pprintDoc(doc)
  }

  def pprintDoc(doc: Document): java.lang.String = doc match {
    case Json.Array(value) => value.map(pprint).mkString("[", ", ", "]")
    case Json.Object(value) => value.map {
      case (key, value) => s""""$key" : ${pprint(value)}"""
    }.mkString("{", ", ", "}")
  }

  /** Print a Json excluding any nulls into human-readable format */
  def pprintNoNull(json: Json): java.lang.String = json match {
    case Json.String(value) => s""""$value""""
    case Json.Int(value) => value.toString
    case Json.Boolean(value) => value.toString
    case Json.Null => ""
    case Json.Array(data) => data match {
      case Nil => "[]"
      case _ => data.map(pprintNoNull).mkString("[", ", ", "]")
    }
    case Json.Object(data) =>
      val nonNull = data.filter {
        case (_, value) => value != Json.Null
      }

      if (nonNull.isEmpty) ""
      else nonNull.map {
        case (key, value) => s""""$key" : ${pprintNoNull(value)}"""
      }.mkString("{", ", ", "}")
  }
}
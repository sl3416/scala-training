package com.jpmc.parsecs

import json.Json

sealed trait JsonParser[+A]

// Parser for all Json types
object Jsonparser {
  def satisfyString(f: String => Boolean): Parser[String] = Parser.from { input =>
    input.headOption match {
      case Some(a) if f(a.toString) => {
        Result.Success(a.toString, input.drop(1))
      }
      case _ => Result.Failure(s"Cannot parse String <$input>")
    }
  }

  def asJsonString(input: String): Parser[Json.String] = string(input).map(Json.String)

  def toJsonObj(p: Parser[List[(Json.String, Json)]]): Parser[Json] =
    p.map { list =>
      for ((k, v) <- list.toMap) yield (k.value, v)
    }.map(Json.Object)

  val whitespace: Parser[Char] = satisfy(_.isWhitespace)
  def jsonWhitespace(c: Char): Parser[Char] = whitespace.rep0 *> char(c) <* whitespace.rep0

  val jsonString: Parser[Json.String] =
    asJsonString("\"") *>
      satisfyString(_ != s""""""").rep.map(x => x.mkString).map(Json.String) <*
      asJsonString("\"")

  val jsonNumber: Parser[Json.Number] = number.map(Json.Number)
  val jsonBoolean: Parser[Json.Bool] = boolean.map(Json.Bool)
  val jsonNull: Parser[Json] = string("null").as(Json.Null)
  val jsonArray = jsonWhitespace('[') *> json.repSep(jsonWhitespace(',')).map(Json.Array) <* jsonWhitespace(']')
  val jsonObj: Parser[Json] = toJsonObj(jsonWhitespace('{') *> ((jsonString <* jsonWhitespace(':')) ~ json).repSep(jsonWhitespace(',')) <* jsonWhitespace('}'))

  lazy val json: parser[Json] = jsonString | jsonNumber | jsonBoolean | jsonNull | jsonArray | jsonObj

  /** Tests */
  val testString =
    s"""{"name": "name", "isStudent": true, "grades": [71, 45, 90], "null": null, "address": {"city": "abc", "postcode": "AB12 3CD"}, "age": 27}"""
  val testString2 = s"""{"name":"name","isStudent":true,"age":27""" // no space
}

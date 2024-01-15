package com.jpmc.json

object Json {
  /* Type alias:
   * Either returns an error msg (String) or correctly decoded Json (A) e.g. Json Bool to Boolean
   */
  type Result[A] = Either[java.lang.String, A]

  def apply(pairs: java.lang.String, Json)*): Object(pairs.toMap)

  final case class Object(data: Map[java.lang.String, Json]) extends Json {
    def get[A: JsonDecoder](field: java.lang.String): Result[A] =
      data
        .get(field)
        .toRight(s"Failed to find field $field") // handle fail
        .flathap(JsonDecoder[A].decode)
  }

  final case class Array(data: List[Json]) extends Json

  final case class String(value: java.lang.String) extends Json

  final case class Number(value: scala.Int) extends Json

  final case class Bool(value: Boolean) extends Json

  final case object Null extends Json
}
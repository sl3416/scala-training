package com.jpmc.json

object PrettyPrinter extends App {
  /** Prints Json into human readable format */
  def print(expr: Json): String = expr match {
    case Json.Array(data) => data.map(print).mkString("[", ", ", "]")
    case Json.Bool(value) => value.toString
    case Json.String(value) => s""""$value""""
    case Json.Number(value) => value.toString
    case Json.Object(data) => data.map {
      case (key, value) => s""""$key" : ${print(value)}"""
    }.mkString("{", ",", "}")
    case Json.Null => "null"
  }
}
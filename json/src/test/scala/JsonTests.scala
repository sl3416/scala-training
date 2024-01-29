package com.jpmc.json

import com.jpmc.json.Json.{Array, Boolean, Document, Int, Null, Object, String, pprint, pprintDoc, pprintNoNull}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class JsonTests extends AnyFunSuite with ScalaCheckPropertyChecks with Matchers {
  val actual: Json = Object {
    Map(
      "name" -> String("John Doe"),
      "age" -> Int(25),
      "enabled" -> Boolean(true),
      "term_1" -> Object {
        Map("grades" -> Array(List(Null, Int(65), Int(80))))
      },
      "is_null" -> Null
    )
  }

  test("the pprint method should always return a java.lang.String") {
    pprint(actual).getClass.getName should be("java.lang.String")
  }

  test("the pprint method prints a Json with all ADTs") {
    val expected = """{"name" : "John Doe", "is_null" : null, "enabled" : true, "term_1" : {"grades" : [null, 65, 80]}, "age" : 25}"""
    pprint(actual) should be(expected)
  }

  test("the pprintNoNull method prints a Json with no nulls") {
    val expected = """{"name" : "John Doe", "enabled" : true, "term_1" : {"grades" : [, 65, 80]}, "age" : 25}""" // FIXME: the comma should also be removed after a null is removed
    pprintNoNull(actual) should be(expected)
  }

  test("the pprintDoc method prints a Json document") {
    val testDoc: Document = Array(List(Boolean(true), Boolean(false), Boolean(true)))
    val expected = """[true, false, true]"""
    pprintDoc(testDoc) should be(expected)
  }
}
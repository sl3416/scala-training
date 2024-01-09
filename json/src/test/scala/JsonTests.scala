package com.jpmc.json

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class JsonTests extends AnyFunSuite with ScalaCheckPropertyChecks with Matchers {
  test("Some test") {
    forAll { str: String =>
      str should be(str)
    }
  }
}

package com.jpmc.json

trait JsonEncoder[A] {
  def encode(value: A): Json

  def contramap[B](f: B => A): JsonEncoder[B] = b => encode(f(b))
}

object JsonEncoder {
  def apply[A](implicit encoder: JsonEncoder[A]): JsonEncoder[A] = encoder

  /** Reduce boilerplate of creating encoder */
  def from[A](f: A => Json): JsonEncoder[A] = new JsonEncoder[A] {
    override def encode(value: A): Json = f(value)
  }

  /** syntactic sugar mhich allows us to skip couple of steps in our decoding
  we can do value.asJson[Int] instead of decoder.decode[Int](value) **/
  implicit class JsonSyntax[A](a: A) {
    def asJson(implicit encoder: JsonEncoder[A]): Json = encoder.encode(a)
  }

  implicit class ObjectSyntax(key: String) {
    def :=[A: JsonEncoder](value: A): (String, Json) = key -> value.asJson
  }

  implicit def listEncode[A](implicit encoderInner: JsonEncoder[A]): JsonEncoder[List]] =
    (value: List[A]) => Json.Array(value.map(_.asJson))

  /* Json Encoders */
  implicit val booleanEncoder: JsonEncoder[Boolean] = JsonEncoder.from(Json, Bool)
  implicit val stringEncoder: JsonEncoder[java.lang.string] = JsonEncoder.from(Json.String)
  implicit val integerEncoder: JsonEncoder[Int] = JsonEncoder.from(Json.Number)
  implicit val userEncoder: JsonEncoder[Json.User] = JsonEncoder.from { user =>
    Json(
      "name" := user.name,
      "age" := user.age,
      "enabled" := user.enabled
    )
  }

  implicit def optionEncoder[A: JsonEncoder]: JsonEncoder[Option[A]] = JsonEncoder.from {
    case Some(a) => a.asJson
    case None => Json.Null
  }

  val testString = "john doe"
  val testuser = User("john doe", 25, true)
  val testBool = "true"

  val encoded = testUser.asJson
  println(print(encoded))
}
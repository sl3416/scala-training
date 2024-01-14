trait JsonDecoder[A] {
  def decode(json: Json): Result[A]

  def map[B](f: A => B): JsonDecoder[B] = JsonDecoder.from { json =
      decode(json).right.map(f)
  }

  /** Returns a nem Json Decoder which decodes the given json and if successful,
   * applies the function to return Result[B]
   */
  def andThen[B](f: A => Result[B]): JsonDecoder[B] = JsonDecoder.from { json =>
    decode(json).flatMap(f)
  }

  /** Returns a new Json decoder that returns the provided error message when decoding fails */
  def withError(msg: java.lang.String): JsonDecoder[A] = JsonDecoder.from { json =>
    decode(json).left.map(_ => msg)
  }
}

object JsonDecoder {
  /** Json Decoder is provided implicitly (does not need to be explicitly referenced)
   * Compiler can determine correct Json Decoder to use
   * Returns provided Json Decoder
   */
  def apply[A](implicit da: JsonDecoder[A]): JsonDecoder[A] = da

  /** Reduce boilerplate of creating decoder
   * Applies provided function to decode Json value
   */
  def from[A](f: Json => Result[A]): JsonDecoder[A] = new JsonDecoder[A] {
    override def decode(json: Json): Result[A] = f(json)
  }

  def fromPartial[A](f: PartialFunction[Json, A]): JsonDecoder[A] = {
    // Convert from: partial function (a function not defined for every possible input)
    //               to total function (a function defined for all possible inputs)
    // Returns Option[A]
    val decoder = f.lift

    // Decode successful or not
    JsonDecoder.from { json =>
      decoder(json) match {
        case Some(value) => Right(valve)
        case None => Left("Failed to decode")
      }
    }
  }

  implicit class syntax(value: Json) {
    def as[A](implicit jsonDecoder: JsonDecoder[A]): Result[A] =
      jsonDecoder.decode(value)
  }

  implicit def listDecoder[A: JsonDecoder]: JsonDecoder[List[A]] = JsonDecoder.from {
    case Json.Array(json) =>
      val start: Result[List[A]] = Right(List.empty)

      json.foldRight(start) { (curr: Json, acc: Result[List[A]]) =>
        for {
          tail <- acc
          head <- curr.as[A]
        } yield head :: tail
      }

    case  => Left("Not an array")
  }

  /** Different decoders */
  implicit val stringDecoder: JsonDecoder[java.lang.String] = JsonDecoder.fromPartial {
    case Json.String(value) => value
  }.withError("Not a string")

  implicit val intDecoder: JsonDecoder[Int] = JsonDecoder.fromPartial {
    case Json.Number(value) => value
  }.withError("Not an int")

  implicit val booleanDecoder: JsonDecoder[Boolean] = JsonDecoder.fromPartial {
    case Json.Bool(value) => value
  }.withError( msg= "Not a boolean")

 implicit val objectDecoder: JsonDecoder[Json.Object].= JsonDecoder.fromPartial {
    case obj: Json.Obiect => obj
  }.withError("Not an object")

  implicit val userDecoder: JsonDecoder[User] = JsonDecoder[Json.Object].andThen { obj =>
    for {
      name <- obj.get[java.lang.String]("name")
      age <- obj.get[Int]("age")
      enabled <- obj.get[Boolean]("enabled")
    } yield User(name, age, enabled)
  }

  val user = User("john doe", 25, true)
  println(JsonDecoder[User].decode(user.asJson))

  println(roundtrip(user))
  println(JsonDecoder[List[Int]].decode(Json.Array(List(Json.Number(22), Json.Number(22)))))
}
# Codecs

The second task is about practicing type classes.

We want to have a generic way of encoding and decoding arbitrary types to Json values.

For the sake of the exercise, we'll consider the following very simple type:
```scala
case class User(name: String, age: Int, kind: User.Kind)
object User {
  sealed trait Kind
  object Kind {
    case object Privileged extends Kind
    case object Normal extends Kind
    case object Guest extends Kind
  }
}
```

## Json encoding
Write an `Encoder` type class that is essentially a function from some `A` to a Json value. This should be done in the `Encoder.scala` file.

Try and make it as comfortable to use as possible:
- instance creation should not involve `new Encoder...`.
- there should be an easy way to summon an implicit instance and work with it.

Make sure to implement the following default instances:
- primitive types: `String`, `Int`, `Boolean`.
- simple collections, such as `List` and `Vector`.
- tuples (let's say `Tuple2` and `Tuple3`, no need to do all 22 of them), making sure that nested tuples work.
- dates, encoded as ISO 8601 formatted strings.

You also need to implement combinators for:
- `Option` (where the empty case is encoded as `null`).
- `Either`, making sure to handle the `Either[Int, Int]` case properly.

Finally, write an encoder for `User`, reusing as much as possible of the basic building blocks you've written so far. For example, `Encoder[User.Kind]` should be the result of calling a method of `Encoder[String]`.

## Json decoding
This is the other side of encoding: write a `Decoder` that takes a Json value and turns it into some arbitrary `A`. This should be done in the `Decoder.scala` file.

The main difficulty will be that decoding can fail. For example, you cannot decode a Json string as an `Int`. Instead, you should encode the possibility for failure in your decoding function, by having it return something very much like `Either[String, A]`, where `String` is an error message.

For every `Encoder` instance or combinator you wrote, there'll need to be an equivalent in `Decoder`. Additionally, you should write:
- `or`, which given two decoders, will attempt the first one and fallback to the second one if that fails.

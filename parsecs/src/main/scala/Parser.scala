package com.jpmc.parsecs

sealed trait Result[+A] {
  def map[B](f: A => B): Result[B] = {
    this match {
      case Result.Success(a, rest) => Result.Success(f(a)
      case Result.Failure(err) => Result.Failure(err)
    }
  }
}
object Result {
  case class Success[A](value: A, rest: String) extends Result[A]

  case class Failure[A](err: String) extends Result[A]
}

sealed trait Parser[+A] {
  /** Returns a Success or Failure upon parsing */
  def parse(input: String): Result[A]

  /** Applies a function on the result of a parser */
  def map[B](f: A => B): Parser[B] = Parser.from { input =>
    parse(input).map(f)
  }

  /** Applies a function on the result of 2 parsers when successful */
  // `other` is a byname parameter - prevents stack overflow as it's only called when it succeeds, if it fails, it is not called
  def map2[B, C](other: => Parser[B]) = Parser.from { input =>
    parse(input).map(f)
  }

  /** OR:
   * Either parser should be able to parse the input */
  def |[B >: A](other: => Parser[B]): Parser[B] = Parser.from { input =>
    parse(input) match {
      case Result.Success(a, rest) => Result.Success(a, rest)
      case Result.Failure(_) => other.parse(input)
    }
  }

  /** FOLLOWED BY:
   * Applies parssers in sequence */
  def ~[B](other: => Parser[B]): Parser[(A, B)] = map2(other)(Tuple2.apply)

  /** Returns a parser of Option, where `Some` represents a successful parse and `None` represents an unsuccessful parse */
  def option: Parser[Option[A]] = this.map(Option.apply) | Parser.pure(Option.empty) // Either Succeeds (Some(_)) or Failure (returns a constant aka None)

  /** Parses a repetition of elements */
  def rep: Parser[List[A]] = rep | Parser.pure(List.empty)

  /** Parses 0 or more repetition of elements */
  def rep0: Parser[List[A]] = rep | Parser.pure(List.empty)

  /** Renanes result tupe of d parser */
  def as[B](b: => B): Parser[B] = this.map(_ => b)

  /** Ignore what's on right */
  def <*[B](other: => Parser[B]): Parser[A] = map2(other)((a, _) => a)

  /** Ignore what s on left */
  def *>[B](other: => Parser[B]): Parser[B] = map2(other)((_, b) => b)

  /** Parses a repetition of elements separated by a separator */
  def repSep[S](sep: Parser[S]): Parser[List[A]] = {
    val tail = (sep *> this).rep0

    (this ~ tail).map { case (head, tail) => head :: tail }
  }

  // def repSep[S](other: Parser[S]): Parser[List[A]] = (this <* other).rep0.map2(this)((a, b) => a :+ b)

  /** Parses 0 or more repetition of elements separated by a separator */
  def repSep0[S](sep: Parser[S]): Parser[List[A]] = repSep(sep) | Parser.pure(List.empty)
}

object Parser extends App {
  def from[A](f: String => Result[A]): Parser[A] = new Parser[A] {
    override def parse(input: String): Result[A] = f(input)
  }

  /** Disregards input and returns a Success
   * A way to construct a parser without an input */
  def pure[A](a: A): Parser[A] = Parser.from { input => Result.Success(a, input)}

  /** Returns a new parser if the input satisfies the predicate function, f */
  def satisfy(f: Char => Boolean): Parser[Char] = Parser.from { input =>
    input.headOption match {
      case Some(c) if f(c) => Result.Success(c, input.drop(1))
      case _ => Result.Failure("Cannot parse")
    }
  }

  implicit class ResultToEither[A](result: Result[A]) {
    def toEither: Either[String, A] = result match {
      case Result.Success(value, _) => Right(value)
      case Result.Failure(error) => Left(error)
    }
  }

  def char(input: Char): Parser[Char] = satisfy(_ == input)
  def string(input: String): Parser[String] = input.map(char).foldLeft(Parser.pure(""))((acc, a) => acc.map2(a)(_ + _))
//  def string(input: String): Parser[String] = input.map(char).foldRight(Parser.pure(""))((a, acc) => a.map2(acc)(_ + _))

  def boolean: Parser[Boolean] = string("true").as(true) | string("false").as(false)

  def digit: Parser[Int] = satisfy(c => {('0' to '9').toSet[Char].contains(c)}).map(_.toString.toInt)
  def number: Parser[Int] = digit.rep.map(.foldLeft(0)((acc, a) => acc * 10 + a))
}
package com.monovore.decline

import java.net.{URI, URISyntaxException}

import cats.data.{Validated, ValidatedNel}

import scala.concurrent.duration.Duration

trait Argument[A] { self =>
  def defaultMetavar: String
  def read(string: String): ValidatedNel[String, A]
}

object Argument extends PlatformArguments {

  def apply[A](implicit argument: Argument[A]) = argument

  implicit val readString: Argument[String] = new Argument[String] {

    override def read(string: String): ValidatedNel[String, String] = Validated.valid(string)

    override def defaultMetavar: String = "string"
  }

  private def readValueNumericDimension[A](typeName: String)(parse: String => A): Argument[A] = new Argument[A] {
    override def read(string: String): ValidatedNel[String, A] =
      try Validated.valid(parse(string))
      catch { case nfe: NumberFormatException => Validated.invalidNel(s"Invalid $typeName: $string") }

    override def defaultMetavar: String = typeName
  }

  implicit val readInt: Argument[Int] = readValueNumericDimension("integer")(_.toInt)
  implicit val readLong: Argument[Long] = readValueNumericDimension("integer")(_.toLong)
  implicit val readBigInt: Argument[BigInt] = readValueNumericDimension("integer")(BigInt(_))
  implicit val readFloat: Argument[Float] = readValueNumericDimension("floating-point")(_.toFloat)
  implicit val readDouble: Argument[Double] = readValueNumericDimension("floating-point")(_.toDouble)
  implicit val readBigDecimal: Argument[BigDecimal] = readValueNumericDimension("decimal")(BigDecimal(_))
  implicit val readDuration: Argument[Duration] = readValueNumericDimension("duration")(Duration(_))

  implicit val readURI: Argument[URI] = new Argument[URI] {

    override def read(string: String): ValidatedNel[String, URI] =
      try { Validated.valid(new URI(string)) }
      catch { case use: URISyntaxException => Validated.invalidNel(s"Invalid URI: $string (${ use.getReason })") }

    override def defaultMetavar: String = "uri"
  }
}

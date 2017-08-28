package com.monovore.decline

import cats.data.Validated._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.duration.Duration

class ArgumentSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

  "String arguments" should {

    "pass through unchanged" in forAll { str: String =>
      Argument[String].read(str) should equal(Valid(str))
    }
  }

  "Integer arguments" should {

    "parse normal integers" in forAll { int: Int =>
      Argument[Int].read(int.toString) should equal(Valid(int))
    }

    "parse long integers" in forAll { int: Long =>
      Argument[Long].read(int.toString) should equal(Valid(int))
    }

    "parse big integers" in forAll { int: BigInt =>
      Argument[BigInt].read(int.toString) should equal(Valid(int))
    }
  }

  "Duration arguments" should {

    implicit val arbDuration: Arbitrary[Duration] =
      Arbitrary(Gen.frequency(
        1 -> Gen.const(Duration.Inf),
        1 -> Gen.const(Duration.MinusInf),
        1 -> Gen.const(Duration.Zero),
        6 -> Gen.chooseNum(Long.MinValue + 1, Long.MaxValue).map(Duration.fromNanos)))

    def string(duration: Duration) = duration match {
      case d@(Duration.Inf | Duration.MinusInf) => d.toString.split("\\.")(1)
      case d => d.toString
    }

    "parse durations" in forAll { duration: Duration =>
      Argument[Duration].read(string(duration)) should equal(Valid(duration))
    }
  }
}

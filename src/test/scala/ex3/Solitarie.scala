package ex3

import ex3.Solitaire.{possibleMoves, next}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolitarieSpec extends AnyFlatSpec with Matchers:
  val width = 7
  val height = 7

  "possibleMoves" should "give all possible moves" in:
    possibleMoves((3,3), width, height) should be(Set((0,3), (6,3), (3,0), (3,6), (1,1), (5,5), (1,5), (5,1)))
    possibleMoves((1,1), width, height) should be(Set((4,1),(1,4),(3,3)))

  "next" should "continue the given solution" in:
    next(Seq((1,1)), width, height) should be(Set(Seq((4,1),(1,1)),Seq((1,4),(1,1)),Seq((3,3),(1,1))))



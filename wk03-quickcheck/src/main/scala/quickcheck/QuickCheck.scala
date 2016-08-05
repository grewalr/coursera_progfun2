package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap
{
  lazy val genHeap: Gen[H] =
    for
    {
      x <- arbitrary[A]
      y <- oneOf(const(empty), genHeap)
    } yield insert(x, y)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // Properties
  property("gen1") = forAll
  {
    h: H =>
      val m = if(isEmpty(h)) 0 else findMin(h)
      findMin(insert(m, h)) == m
  }

  property("min1") = forAll
  {
    x: A =>
      val h = insert(x, empty)
      findMin(h) == x
  }

  // Insert any two elements into an empty heap, finding the minimum of the resulting heap should get
  // the smallest of the two elements back
  property("hint1") = forAll
  {
    (x: A, y: A) =>
      val h1 = insert(x, empty)
      val h2 = insert(y, h1)
      if (y > x) findMin(h2) == x else findMin(h2) == y
  }

  // Insert an element into an empty heap, then delete the min, the resulting heap should be empty
  property("hint2") = forAll
  {
    x: A =>
      val h1 = insert(x, empty)
      val h2 = deleteMin(h1)
      isEmpty(h2)
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and
  // deleting minima. (Hint: recursion and helper functions are your friends.)
  property("hint3") = forAll
  {
    x: H =>
    ???
  }


}

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

  // Insert any two elements into an empty heap, finding the minimum of the resulting
  // heap should get the smallest of the two elements back
  property("hint1") = forAll
  {
    (x: A, y: A) =>
      val h1 = insert(x, empty)
      val h2 = insert(y, h1)
      if (y > x) findMin(h2) == x else findMin(h2) == y
  }

  // Insert an element into an empty heap, then delete the min, the resulting
  // heap should be empty
  property("hint2") = forAll
  {
    x: A =>
      val h1 = insert(x, empty)
      val h2 = deleteMin(h1)
      isEmpty(h2)
  }

  // Given any heap, you should get a sorted sequence of elements when continually
  // finding and deleting minima.
  property("hint3") = forAll
  {
    h: H =>
      def isSorted(h: H): Boolean =
      {
        if(isEmpty(h)) true
        else
        {
          val minH = findMin(h)
          val delMin = deleteMin(h)
          isEmpty(delMin) || (minH <= findMin(delMin) && isSorted(delMin))
        }
      }

      isSorted(h)
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one
  // or the other
  property("hint4") = forAll
  {
    (h1: H, h2: H) =>
      val minOfH1: A = findMin(h1)
      val minOfH2: A = findMin(h2)
      val minOfMeldOfH1H2: A = findMin(meld(h1, h2))
      minOfMeldOfH1H2 == minOfH1 || minOfMeldOfH1H2 == minOfH2
  }

  // Meld on both heaps should be equal to removing from one heap and insert into the other
  property("meld on equal heaps") = forAll
  {
    (h1: H, h2: H) =>
      def heapEqual(h1: H, h2: H): Boolean =
      {
        if(isEmpty(h1) && isEmpty(h2)) true
        else
        {
          val min1 = findMin(h1)
          val min2 = findMin(h2)
          min1 == min2 && heapEqual(deleteMin(h1), deleteMin(h2))
        }
      }
      heapEqual(meld(h1, h2),
        meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}

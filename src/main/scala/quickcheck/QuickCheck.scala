package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

import scala.annotation.tailrec
import scala.collection.immutable

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  )

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("smallestOfTheTwoElements") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val smallestElement = if (a > b) b else a
    findMin(h) == smallestElement
  }

  property("emptyResultingHeap") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("sortedSequenceOfElements") = forAll { (h: H) =>
    @tailrec
    def remainingMin(remaining: H, deleteList: List[Int]): List[Int] = remaining match
      case h if h == empty => Nil
      case _ =>
        val min = findMin(remaining)
        remainingMin(deleteMin(remaining), min :: deleteList)

    val list = remainingMin(h, Nil)
    list == list.sorted
  }

  property("contains inserted element") = forAll { (a: Int, h: H) =>
    val newHeap = insert(a, h)

    @tailrec
    def contains(remaining: H): Boolean = remaining match {
      case Nil => false
      case h :: _ if findMin(remaining) == a => true
      case _ :: tail => contains(deleteMin(remaining))
    }

    contains(newHeap)
  }

  property("minimumOfOneOrTheOtherHeaps") = forAll { (h1: H, h2: H) =>
    if isEmpty(h1) && isEmpty(h2) then
      isEmpty(meld(h1, h2))
    else if isEmpty(h1) && !isEmpty(h2) then
      isEmpty(h1)
    else if !isEmpty(h1) && isEmpty(h2) then
      isEmpty(h2)
    else
      val minH1 = findMin(h1)
      val minH2 = findMin(h2)
      val meldH = meld(h1, h2)
      val minMeldH = findMin(meldH)
      minMeldH == minH1 || minMeldH == minH2
  }










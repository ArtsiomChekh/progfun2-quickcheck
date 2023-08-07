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
      h <- genHeap
    } yield insert(v, h)
  )

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("smallest of the two elements") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val smallestElement = if (a > b) b else a
    findMin(h) == smallestElement
  }

  property("empty heap after insert and delete") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("sorted sequence of elements when continually finding and deleting minima") = forAll { (h: H) =>
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
    def contains(remaining: H): Boolean = remaining match
      case Nil => false
      case h :: _ if findMin(remaining) == a => true
      case _ :: tail => contains(deleteMin(remaining))

    contains(newHeap)
  }

  property("minimum of one or the other heaps") = forAll { (h1: H, h2: H) =>
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

  property("meld nonempty heap with empty heap equals first heap") = forAll { (a: Int) =>
    val h1 = insert(a, empty)
    val h2 = empty
    val meldHeap = meld(h1, h2)
    meldHeap == h1
  }

  property("meld empty heap with nonempty heap equals second heap") = forAll { (a: Int) =>
    val h1 = empty
    val h2 = insert(a, empty)
    val meldHeap = meld(h1, h2)
    meldHeap == h2
  }

  property("delete min 2 elems results empty") =
    forAll { (a: Int, b: Int) =>
      val h1 = insert(a, insert(b, empty))
      deleteMin(deleteMin(h1)) == empty
    }

  property("meld two heaps then deleting and inserting min element of one of them") = forAll { (h1: H, h2: H) =>
    @tailrec
    def loop(h1: H, h2: H): Boolean = (h1, h2) match
      case (Nil, Nil) => true
      case _ => findMin(h1) == findMin(h2) && loop(deleteMin(h1), deleteMin(h2))

    if isEmpty(h1) && isEmpty(h2) then true
    else if isEmpty(h1) then loop(meld(h1, h2), meld(h1, h2))
    else if isEmpty(h2) then loop(meld(h1, h2), meld(h1, h2))
    else loop(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }


















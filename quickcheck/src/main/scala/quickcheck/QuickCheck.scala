package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("findMin") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("FindMin should retrieve a min element from a heap which contains 2 elements") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == math.min(a, b)
  }

  property("isEmpty") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("A list retrieved from a heap should be sorted") = forAll { (h: H) =>
    isSorted(genList(h))
  }

  property("FindMin from meld heap should equal a min element between 2 heaps") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2))==math.min(findMin(h1), findMin(h2))
  }

  property("A list which is retrieved from meld heap should be sorted") = forAll { (h1: H, h2: H) =>
    isSorted(genList(meld(h1, h2)))
  }

  property("DeleteMin should delete a min element") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    deleteMin(h) == insert(math.max(a, b), empty)
  }

  property("A list which is retrieved from a heap should equal a list the heap is originated") = forAll { l: List[Int] =>
    val ll = l.sorted
    val h = ll.foldLeft(empty)((accu, curr) => insert(curr, accu))
    genList(h) == ll
  }

  property("Tail of a list should equal a list retrieved from a heap which is deleted min") = forAll { l: List[Int] =>
    if (l.size == 0) true
    else {
      val ll = l.sorted
      val h = ll.foldLeft(empty)((accu, curr) => insert(curr, accu))
      ll.tail == genList(deleteMin(h))
    }
  }

  def isSorted(list: List[Int]) = list == list.sorted

  def genList(h: H): List[Int] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: genList(deleteMin(h))
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    j <- oneOf(const(empty), genHeap)
  } yield insert(i, j)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}

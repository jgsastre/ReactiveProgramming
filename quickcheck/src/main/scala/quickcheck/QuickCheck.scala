package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }
  
  property("ordered") = forAll { (h: H) =>
    def ordered : (Int, H) => Boolean = (a, x) => {
      if (isEmpty(x)) true 
      else {
        val min_value = findMin(x)
        (a <= min_value) && ordered(min_value, deleteMin(x))
      }
    }
    val newH = insert(0, h)
    ordered(findMin(newH), deleteMin(newH))
  }
  
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) <= findMin(deleteMin(h))
  }
  
  property("min3") = forAll { (a: Int, b: Int) =>
    val min_value = if (a < b) a else b
    val max_value = if (a < b) b else a
    findMin(insert(a, insert(b, empty))) == min_value &&
    findMin(deleteMin(insert(a, insert(b, empty)))) == max_value
  }
  
  property("add_erase") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }
  
  property("min.melding.pair") = forAll { (h: H, j: H) =>
    val i = meld(h, j)
    if (isEmpty(h) && isEmpty(j))
      true
    else if (isEmpty(h))
      findMin(i) == findMin(j)
    else if (isEmpty(j))
      findMin(i) == findMin(h)
    else {
      val min_value = if (findMin(h) < findMin(j)) findMin(h) else findMin(j)
      findMin(i) == min_value
    }
  }
  
  property("list") = forAll { list : List[Int] =>
    val orderedList = list.sorted
    def create(l : List[Int]) : H = l match {
      case Nil => empty
      case x :: xs => insert(x, create(xs))
    }
    
    def check(l : List[Int], h : H) : Boolean = l match {
      case Nil => isEmpty(h)
      case x :: xs => (x == findMin(h)) && check(xs, deleteMin(h))
    }
    check(orderedList, create(list))
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}

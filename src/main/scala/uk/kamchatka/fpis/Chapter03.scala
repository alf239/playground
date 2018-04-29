package uk.kamchatka.fpis

import uk.kamchatka.fpis.List._

object Chapter03 extends App {
  println(foldLeft(Nil, 0)((_, _) => ???))
  println(sum(List(1, 2, 3)))
  println(length(List(1, 2, 3)))
  println(product(List(1, 2, 3)))
  println(reverse(List(1, 2, 3)))
  println(append(List(1, 2, 3), List(4, 5, 6)))
  println(flatten(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))
  println(map(List(1, 2, 3))(_ + 1))
  println(map(List(1.0, 2.1, 3.14159))(_.toString))
  println(filter(List(1 to 10: _*))(_ % 2 == 0))
  println(filter2(List(1 to 10: _*))(_ % 2 == 0))
  println(flatMap(List(1 to 10: _*))(x => List(x, x + 0.5)))
  println(zipWith(List(1 to 10: _*), List((1 to 10).map(_.toString): _*))((i, s) => s"int: $i, string: $s"))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3, 4)))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3, 4, 6)))
  println(Tree.size(Leaf(1)))
  println(Tree.size(Branch(Leaf(1), Leaf(2))))
  println(Tree.maximum(Branch(Leaf(1), Leaf(2))))
  println(Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
  println(Tree.map((x: Int) => x * 2)(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
}

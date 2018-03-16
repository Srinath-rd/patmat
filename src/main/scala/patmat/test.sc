

abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
t1.left
t1.right


// Part 1: Basics
def weight(tree: CodeTree): Int = tree match {
  case Fork(_, _, _ , weight) => weight
  case Leaf(_, weight) => weight
}
//  ??? // tree match ...

def chars(tree: CodeTree): List[Char] = tree match {
  case Fork(_, _ , chars, _)=> chars
  case Leaf(char, _) => List(char)
}

def makeCodeTree(left: CodeTree, right: CodeTree) =
  Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

def string2Chars(str: String): List[Char] = str.toList

def times(chars: List[Char]): List[(Char, Int)] = chars.groupBy(identity).map(t => (t._1, t._2.length)).toList

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs.sortBy(x => x._2).map(x => Leaf(x._1, x._2))

def combine(trees: List[CodeTree]): List[CodeTree] = {
//  if(trees.length < 2) trees
//  else  combine(t)
  trees match {
    case List(x, y, _*)  =>  if(trees.size < 2) trees else {

      val result =  combine(List(makeCodeTree(x, y)))
//      println("Hey: x: " + x + "y:" + y + "result: " + result);
      (result :: trees.takeRight(trees.length - 2) :: Nil).flatten
    }
    case _  => trees
  }
}

weight(t1)
weight(t2)
chars(t1)
chars(t2)
val b = string2Chars("hello, world").groupBy(identity).map(t => (t._1, t._2.length)).toList

times(List('a', 'b', 'a'))
makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))

val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
combine(leafList)

case class Blah(security: String, price: Double)
val myList = List(Blah("a", 2.0), Blah("b", 4.0))
val sum = myList.foldLeft(0.0)(_ + _.price)
val sum1 = myList.foldLeft(0.0)((d, blah) => d + blah.price)

val a = Array(12, 6, 15, 2, 20, 9)
a.reduceLeft((i:Int, i1) => i + i1)

val list = List((1, 2), (1, 2), (1, 2))
list.foldLeft((0, 0)) { case ((accA, accB), (a, b)) => (accA + a, accB + b) }
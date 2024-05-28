package kuplrg

object Implementation extends Template {

  def sqsum(x: Int, y: Int): Int = (x * x) + (y * y)

  def concat(left: String, right: String): String = left + right

  def subN(n: Int): Int => Int = b => b - n

  def twice(f: Int => Int): Int => Int = x => f(f(x))

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  def sumOnlyOdd(l: List[Int]): Int = l.filter(_ % 2 == 1).sum

  def foldWith(f: (Int, Int) => Int): List[Int] => Int = l => l.foldLeft(0)(f)

  def toSet(l: List[Int], from: Int): Set[Int] = l.zipWithIndex.filter{ case (_, index) => index >= from }.map(_._1).toSet

  def getOrZero(map: Map[String, Int], key: String): Int = map.getOrElse(key, 0)

  def setMinus(s1: Set[Int], s2: Set[Int]): Set[Int] = s1.diff(s2)

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  def has(value: Int): Tree => Boolean = 
    case Tree.Leaf(v) => v == value
    case Tree.Branch(left, v, right) => v == value || has(value)(left) || has(value)(right)

  def maxDepthOf(value: Int): Tree => Option[Int] = tree => {
    def findMaxDepth(t: Tree, currentDepth: Int): Option[Int] = t match {
      case Leaf(v) if v == value => Some(currentDepth)
      case Leaf(_) => None
      case Branch(left, v, right) =>
        val leftDepth = findMaxDepth(left, currentDepth + 1)
        val rightDepth = findMaxDepth(right, currentDepth + 1)
        if(v == value) {
          val currentMax = Some(currentDepth)
          List(leftDepth, rightDepth, currentMax).flatten.maxOption
        } else {
          List(leftDepth, rightDepth).flatten.maxOption
        }
    }

    findMaxDepth(tree, 0)
  }

  def mul(t: Tree): Int = t match {
    case Leaf(n) => n
    case Branch(l, n, r) => mul(l) * n * mul(r)
  }

  def countLeaves(t: Tree): Int = t match {
    case Leaf(n) => 1
    case Branch(l, n, r) => countLeaves(l) + countLeaves(r)
  }

  def postOrder(t: Tree): List[Int] = t match {
    case Leaf(n) => List(n)
    case Branch(l, n, r) => postOrder(l) ::: postOrder(r) ::: List(n)
  }

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  def countLiterals(expr: BE): Int = expr match {
    case BE.True => 1
    case BE.False => 1
    case BE.And(left, right) => countLiterals(left) + countLiterals(right)
    case BE.Or(left, right) => countLiterals(left) + countLiterals(right)
    case BE.Not(subExpr) => countLiterals(subExpr)
  }

  def countNots(expr: BE): Int = expr match {
    case BE.True => 0
    case BE.False => 0
    case BE.And(left, right) => countNots(left) + countNots(right)
    case BE.Or(left, right) => countNots(left) + countNots(right)
    case BE.Not(subExpr) => 1 + countNots(subExpr)
  }

  def depth(expr: BE): Int = expr match {
    case BE.True => 0
    case BE.False => 0
    case BE.And(left, right) => 1 + (depth(left) max depth(right))
    case BE.Or(left, right) => 1 + (depth(left) max depth(right))
    case BE.Not(subExpr) => 1 + depth(subExpr)
  }

  def getString(expr: BE): String = expr match {
    case BE.True => "true"
    case BE.False => "false"
    case BE.And(left, right) => "(" + getString(left) + " & " + getString(right) + ")"
    case BE.Or(left, right) => "(" + getString(left) + " | " + getString(right) + ")"
    case BE.Not(subExpr) => "!" + getString(subExpr)
  }

  def eval(expr: BE): Boolean = expr match {
    case BE.True => true
    case BE.False => false
    case BE.And(left, right) => eval(left) && eval(right)
    case BE.Or(left, right) => eval(left) || eval(right)
    case BE.Not(subExpr) => !eval(subExpr)
  }
}

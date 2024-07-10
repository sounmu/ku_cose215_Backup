package kuplrg

import scala.util.Random._
import scala.math._
import kuplrg.SpecBase.len

object Generator {

  // L = { a^i b^j c^k | i, j, k >= 0 and (i = j or i = k) }
  def abc_ij_ik_accept(lenMax: Int)(): Word = {
    val i = nextInt(lenMax + 1)
    if (nextInt(2) == 0) {
      val j = i
      val k = nextInt(lenMax + 1)
      "a" * i + "b" * j + "c" * k
    } else {
      val k = i
      val j = nextInt(lenMax + 1)
      "a" * i + "b" * j + "c" * k
    }
  }

  def abc_ij_ik_random(lenMax: Int)(): Word = {
    val i = nextInt(lenMax + 1)
    val j = nextInt(lenMax + 1)
    val k = nextInt(lenMax + 1)
    "a" * i + "b" * j + "c" * k
  }

  // L = { a^i b^j | (i = 2n + 1 and j = 3n + 1) for n >= 0 }

  def a2n1_b3n1_accept(lenMax: Int)(): Word = {
    val n = nextInt(lenMax + 1)
    "a" * (2 * n + 1) + "b" * (3 * n + 1)
  }

  def a2n1_b3n1_random1(lenMax: Int)(): Word = {
    // a^i b^j
    "a" * nextInt(lenMax) + "b" * nextInt(lenMax)
  }

  def a2n1_b3n1_random2(lenMax: Int)(): Word = {
    // complete random contains a and b
    val seq =
      for i <- 0 to lenMax
      yield nextInt(2) match {
        case 0 => "a"
        case 1 => "b"
      }
    seq.mkString
  }

  // L = { a^i b^j c^k | i, j, k >= 0 and j = i + 2k }

  def abc_j_i2k_accept(lenMax: Int)(): Word = {
    val i = nextInt(lenMax + 1)
    val k = nextInt(lenMax + 1)
    val j = i + 2 * k
    "a" * i + "b" * j + "c" * k
  }

  def abc_j_i2k_random1(lenMax: Int)(): Word = {
    // a^i b^j c^k
    val i = nextInt(lenMax + 1)
    val j = nextInt(lenMax + 1)
    val k = nextInt(lenMax + 1)
    "a" * i + "b" * j + "c" * k
  }

  def abc_j_i2k_random2(lenMax: Int)(): Word = {
    // complete random contains a, b and c
    val seq =
      for i <- 0 to lenMax
      yield nextInt(3) match {
        case 0 => "a"
        case 1 => "b"
        case 2 => "c"
      }
    seq.mkString
  }

  trait Par
  case class Brace(in: Par, level: Int) extends Par:
    override def toString(): String =
      val open: String = level match {
        case 0 => "("
        case 1 => "{"
        case 2 => "["
      }
      val close: String = level match {
        case 0 => ")"
        case 1 => "}"
        case 2 => "]"
      }
      s"$open${in}$close"
  case class Seq(l: Par, r: Par) extends Par:
    override def toString(): String = s"$l$r"
  case object EmptyPar extends Par:
    override def toString(): String = ""

  def generateRandomPar(depth: Int): Par = {
    if (depth == 0) {
      EmptyPar
    } else {
      nextInt(2) match {
        case 0 =>
          Seq(generateRandomPar(depth - 1), generateRandomPar(depth - 1))
        case 1 => Brace(generateRandomPar(depth - 1), nextInt(3))
      }
    }
  }

  def generateRandomParWithOrder(depth: Int, level: Int): Par = {
    if (depth == 0) {
      EmptyPar
    } else {
      nextInt(2) match {
        case 0 =>
          Seq(
            generateRandomParWithOrder(depth - 1, max(level - nextInt(2), 0)),
            generateRandomParWithOrder(depth - 1, max(level - nextInt(2), 0)),
          )
        case 1 =>
          val next_level = nextInt(level + 1)
          Brace(
            generateRandomParWithOrder(depth - 1, max(next_level - nextInt(2), 0)),
            next_level,
          )
      }
    }
  }

  // L = { w \in { '(', ')', '{', '}', '[', ']' }* | w is well-formed and
  // satisfies the order: '()' <= '{}' <= '[]' }

  def ord_brace_accept(depth: Int)(): Word = {
    generateRandomParWithOrder(depth, 2).toString
  }

  def ord_brace_random(depth: Int)(): Word = {
    generateRandomPar(depth).toString
  }

  trait AE
  case class Add(e1: AE, e2: AE) extends AE:
    override def toString(): String = s"$e1+$e2"
  case class Mul(e1: AE, e2: AE) extends AE:
    override def toString(): String = s"$e1*$e2"
  case class Num(val n: Int) extends AE:
    override def toString(): String = n.toString

  def generateRandomAE(depth: Int): AE = {
    if (depth == 0) {
      Num(nextInt(10))
    } else {
      nextInt(2) match {
        case 0 => Add(generateRandomAE(depth - 1), generateRandomAE(depth - 1))
        case 1 => Mul(generateRandomAE(depth - 1), generateRandomAE(depth - 1))
      }
    }
  }

  // L = { w \in { '0', '1', '+', '*' }* | w is an arithmetic expression
  // evaluated to an even number }
  def ae_even_random(depth: Int)(): Word = generateRandomAE(depth).toString

  // L = { a_1 a_2 ... a_2n \in {a, b}* | n >= 1 and a_i = a_{n+i} for all
  // 1 <= i <= n }

  def eq_pair_reject(lenMax: Int)(): Word = {
    val n = nextInt(lenMax) + 1
    val seq =
      for i <- 0 until n
      yield nextInt(2) match {
        case 0 => "a"
        case 1 => "b"
      }
    val rev_seq =
      for c <- seq
      yield c match {
        case "a" => "b"
        case "b" => "a"
      }
    (seq ++ rev_seq).mkString
  }

  def eq_pair_random(lenMax: Int)(): Word = {
    val n = nextInt(lenMax) + 1
    val seq =
      for i <- 0 until 2 * n
      yield nextInt(2) match {
        case 0 => "a"
        case 1 => "b"
      }
    seq.mkString
  }

}

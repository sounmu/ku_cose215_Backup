package kuplrg

import scala.Console.*
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

/** The line separator */
val LINE_SEP = sys.props("line.separator")

/** The type definitions of states and symbols */
type State = Int

/** The type definitions of states and symbols */
type Symbol = Char

/** The type definition of words */
type Word = String

/** The type definitions of states and symbols */
type Alphabet = String

/** A helper function to extract first symbol and rest of word */
object `<|` { def unapply(w: Word) = w.headOption.map((_, w.drop(1))) }

/** A helper function to print a message in green */
def show(msg: String): Unit = println(green(msg))
def green(str: String): String = s"$GREEN$str$RESET"

/** The word acceptable type */
trait Acceptable:

  /** The set of symbols */
  def symbols: Set[Symbol]

  /** The acceptance of a word by the language
    *
    * @param w
    *   the word to be checked for acceptance
    * @return
    *   `true` if the word is accepted, `false` otherwise
    */
  def accept(w: Word): Boolean

  /** The language of the regular expression. */
  def lang: Lang = Lang(symbols, accept)

  /** Checks if the language is equal to the given language
    *
    * @param expected
    *   the expected language
    * @param trial
    *   the number of random words to be checked
    */
  def mustEqual(expected: Acceptable, trial: Int): Unit =
    mustEqual(accept, expected, trial)

  def mustEqual(
    accept: Word => Boolean,
    expected: Acceptable,
    trial: Int,
  ): Unit =
    val list = expected.symbols.toList.sorted
    val m = list.length
    def check(s: Word): Unit =
      val result = accept(s)
      val answer = expected.accept(s)
      if (result != answer)
        val neg = if (answer) "" else " not"
        error(s"the word '$s' should$neg be in the language.")
    def aux(n: Int, k: Int, prevSize: Int): Unit =
      val curSize = (prevSize * m) min n
      for {
        i <- 0 until curSize
        (s, _) = (0 until k).foldLeft(("", i)) {
          case ((s, j), _) => (list(j % m).toString + s, j / list.length)
        }
      } check(s)
      if (curSize < n) aux(n - curSize, k + 1, curSize)
    m match
      case 0 => check("")
      case 1 =>
        val s = symbols.head.toString
        (0 until trial / 10).map(i => check(s * i))
      case _ => aux(trial, 1, 1)

/** The languages */
case class Lang(
  symbols: Set[Symbol],
  contains: Word => Boolean,
) extends Acceptable:

  /** The acceptance of a word by the regular expression.
    *
    * @param word
    *   the word to be checked for acceptance
    * @return
    *   `true` if the word is accepted, `false` otherwise
    */
  def accept(w: Word): Boolean = contains(w)

/** The parser of a context-free grammar */
final private class Parser private (cfg: CFG, input: String, debug: Boolean) {
  enum Point:
    case Entry(i: Int, nt: Nt)
    case Inner(i: Int, nt: Nt, j: Int, k: Int)
    case Exit(i: Int, nt: Nt)

  import Point.*, ParseTree.*, ParseTreePtr.*, ParseResult.*
  val CFG(nts, symbols, start, origRules) = cfg
  val rules = nts.map(x => x -> origRules(x).map(_.seq.toArray).toArray).toMap
  val init = Entry(0, start)
  val result = MMap(init -> Set((0, Vector[ParseTreePtr]())))
  val back = MMap[Exit, Set[Inner]]()
  class Worklist[T](init: Iterable[T] = Nil) {
    var list = init.toList
    var set = Set[T]()
    def +=(t: T): Unit =
      if (!set(t))
        list ::= t
        set += t
    def get: Option[T] = list match
      case Nil => None
      case t :: rest =>
        list = rest
        set -= t
        Some(t)
  }
  val worklist = Worklist[Point](List(init))

  def getResult(point: Point): Set[(Int, Vector[ParseTreePtr])] =
    result.getOrElseUpdate(point, Set())
  def putResult(point: Point, newSet: Set[(Int, Vector[ParseTreePtr])]): Unit =
    val oldSet = getResult(point)
    if (!newSet.subsetOf(oldSet))
      result(point) = oldSet ++ newSet
      worklist += point

  def getBack(exit: Exit): Set[Inner] =
    back.getOrElseUpdate(exit, Set())
  def putBack(exit: Exit, inner: Inner): Unit =
    back(exit) = getBack(exit) + inner
    worklist += exit

  def showLog =
    println("- result:")
    for {
      (key, set) <- result.toList.sortBy(_._1.toString)
    } {
      println(s"  $key ->")
      for {
        (to, tree) <- set.toList.sortBy((x, y) => (x, y.toString))
      } println(s"    $to: ${tree.mkString("[", ", ", "]")}")
    }
    println("- back:")
    for {
      (key, set) <- back.toList.sortBy(_._1.toString)
    } {
      println(s"  $key ->")
      for {
        to <- set.toList.sortBy(_.toString)
      } println(s"    $to")
    }

  @tailrec
  def parse: ParseResult[ParseTree] =
    worklist.get match
      case None =>
        if (debug) showLog
        toResult(NodePtr(start, 0, input.length))
      case Some(work) =>
        work match
          case Entry(i, nt) =>
            val set = getResult(work)
            for (j <- 0 until rules(nt).length)
              putResult(Inner(i, nt, j, 0), set)
          case Inner(i, nt, j, k) =>
            val rhs = rules(nt)(j)
            if (k < rhs.length) rhs(k) match
              case a: Symbol =>
                for {
                  (to, ps) <- getResult(work)
                  if to < input.length && a == input(to)
                } putResult(
                  Inner(i, nt, j, k + 1),
                  Set((to + 1, ps :+ LeafPtr(a))),
                )
              case targetNt: Nt =>
                for ((to, ps) <- getResult(work))
                  putResult(Entry(to, targetNt), Set((to, Vector())))
                  putBack(Exit(to, targetNt), Inner(i, nt, j, k + 1))
            else putResult(Exit(i, nt), getResult(work))
          case exit @ Exit(i, nt) =>
            for {
              next <- getBack(exit)
              prev = Inner(next.i, next.nt, next.j, next.k - 1)
              (to, rs) <- getResult(prev)
              if to == i
            } putResult(
              next,
              getResult(work).map {
                case (to, _) => (to, rs :+ NodePtr(nt, i, to))
              },
            )
        parse

  def toResult(ptr: ParseTreePtr): ParseResult[ParseTree] = ptr match
    case NodePtr(nt, from, until) =>
      val res = ParseResult(for {
        (to, ps) <- getResult(Exit(from, nt))
        if to == until
      } yield ps)
      for {
        ps <- res
        tree <- ps
          .map(toResult)
          .foldLeft(Success(Vector[ParseTree]())) {
            case (vs, trees) =>
              for {
                v <- vs
                t <- trees
              } yield v :+ t
          }
          .map(Node(nt, _))
      } yield tree
    case LeafPtr(symbol) => Success(Leaf(symbol))
}

/** The parser of a context-free grammar */
object Parser {
  def apply(cfg: CFG, input: String, debug: Boolean): ParseResult[ParseTree] =
    new Parser(cfg, input, debug).parse
}

/** Parse tree of a context-free grammar */
enum ParseTree:
  case Node(nt: Nt, children: Vector[ParseTree])
  case Leaf(symbol: Symbol)
  def +(child: ParseTree): ParseTree = this match
    case Node(nt, children) => Node(nt, children :+ child)
    case Leaf(symbol)       => ???
  override def toString: String = this match
    case Node(nt, children) => s"[$nt ${children.mkString(" ")}]"
    case Leaf(symbol)       => s"'$symbol'"

enum ParseTreePtr:
  case NodePtr(nt: Nt, from: Int, until: Int)
  case LeafPtr(symbol: Symbol)

enum ParseResult[+T]:
  case Ambiguous extends ParseResult[Nothing]
  case Success(data: T) extends ParseResult[T]
  case Failure extends ParseResult[Nothing]

  def map[U](f: T => U): ParseResult[U] = this match
    case Ambiguous     => Ambiguous
    case Success(data) => Success(f(data))
    case Failure       => Failure

  def flatMap[U](f: T => ParseResult[U]): ParseResult[U] = this match
    case Ambiguous     => Ambiguous
    case Success(data) => f(data)
    case Failure       => Failure

  def isSuccessful: Boolean = this match
    case Success(_) => true
    case _          => false

  def isAmbiguous: Boolean = this == Ambiguous

  def isFailure: Boolean = this == Failure

object ParseResult:
  def apply[T](seq: Iterable[T]): ParseResult[T] = seq.size match
    case 0 => Failure
    case 1 => Success(seq.head)
    case _ => Ambiguous

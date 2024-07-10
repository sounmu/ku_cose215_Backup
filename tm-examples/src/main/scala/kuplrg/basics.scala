package kuplrg

import scala.Console.*
import scala.collection.mutable

/** The line separator */
val LINE_SEP = sys.props("line.separator")

/** The type definitions of states and symbols */
type State = Int

/** The type definitions of states and symbols */
type Symbol = Char

/** The type definition of words */
type Word = String

/** A helper function to extract first symbol and rest of word */
object `<|` { def unapply(w: Word) = w.headOption.map((_, w.drop(1))) }

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

/** The computable type */
trait Computable {

  /** The set of symbols */
  def symbols: Set[Symbol]

  /** The computation of a word
    *
    * @param w
    *   the word as input for the computation
    * @return
    *   `Some(w)` if the computation is successful, `None` otherwise
    */
  def compute(w: Word): Option[Word]

  /** The computer of the computable definition */
  def computer: Computer = Computer(symbols, compute)

  /** Checks if the language is equal to the given language
    *
    * @param expected
    *   the expected language
    * @param trial
    *   the number of random words to be checked
    */
  def mustEqual(expected: Computable, trial: Int): Unit =
    mustEqual(compute, expected, trial)

  def mustEqual(
    compute: Word => Option[Word],
    expected: Computable,
    trial: Int,
  ): Unit =
    val list = expected.symbols.toList.sorted
    val m = list.length
    def check(w: Word): Unit =
      val result = compute(w)
      val answer = expected.compute(w)
      if (result != answer) answer match
        case Some(x) => error(s"the word '$w' should be computed to '$x'.")
        case None    => error(s"the word '$w' should not be computed.")
    def aux(n: Int, k: Int, prevSize: Int): Unit =
      val curSize = (prevSize * m) min n
      for {
        i <- 0 until curSize
        (w, _) = (0 until k).foldLeft(("", i)) {
          case ((w, j), _) => (list(j % m).toString + w, j / list.length)
        }
      } check(w)
      if (curSize < n) aux(n - curSize, k + 1, curSize)
    m match
      case 0 => check("")
      case 1 =>
        val w = symbols.head.toString
        (0 until trial / 10).map(i => check(w * i))
      case _ => aux(trial, 1, 1)
}

/** The computer */
case class Computer(
  symbols: Set[Symbol],
  process: Word => Option[Word],
) extends Computable {

  /** The computation of a word */
  def compute(w: Word): Option[Word] = process(w)
}

package kuplrg

import scala.util.parsing.combinator.*
import scala.util.matching.Regex

/** The definition of regular expression (RE) */
enum RE extends Acceptable:

  /** Empty */
  case Emp

  /** Epsilon */
  case Eps

  /** Symbol */
  case Sym(symbol: Symbol)

  /** Union */
  case Union(left: RE, right: RE)

  /** Concatenation */
  case Concat(left: RE, right: RE)

  /** Kleene star */
  case Star(re: RE)

  /** The acceptance of a word by the regular expression.
    *
    * @param word
    *   the word to be checked for acceptance
    * @return
    *   `true` if the word is accepted, `false` otherwise
    */
  def accept(word: Word): Boolean = regex.fold(false)(_.matches(word))

  /** Dump the regular expression. */
  def dump: Unit =
    show(s"* A regular expression is dumped:")
    println(s"  ${green("* String form:")} ${stringForm}")
    println(s"  ${green("* Scala object:")} ${this}")

  /** The string form of the regular expression */
  lazy val stringForm: String = this match
    case Emp                => "</>"
    case Eps                => "<e>"
    case Sym(symbol)        => s"$symbol"
    case Union(left, right) => s"${left.stringForm}|${right.stringForm}"
    case Concat(left, right) =>
      val l = left match
        case Union(_, _) => s"(${left.stringForm})"
        case _           => left.stringForm
      val r = right match
        case Union(_, _) => s"(${right.stringForm})"
        case _           => right.stringForm
      s"$l$r"
    case Star(re) =>
      val s = re match
        case Union(_, _) | Concat(_, _) => s"(${re.stringForm})"
        case _                          => re.stringForm
      s"$s*"

  /** All symbols in the regular expression */
  lazy val symbols: Set[Symbol] = this match
    case Emp                 => Set()
    case Eps                 => Set()
    case Sym(symbol)         => Set(symbol)
    case Union(left, right)  => left.symbols ++ right.symbols
    case Concat(left, right) => left.symbols ++ right.symbols
    case Star(re)            => re.symbols

  /** A regex format of the regular expression */
  lazy val regex: Option[Regex] = this match
    case Emp         => None
    case Eps         => Some("".r)
    case Sym(symbol) => Some(s"$symbol".r)
    case Union(left, right) =>
      (left.regex, right.regex) match
        case (Some(l), Some(r)) => Some(s"($l|$r)".r)
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case (None, None)       => None
    case Concat(left, right) =>
      for {
        l <- left.regex
        r <- right.regex
      } yield s"($l$r)".r
    case Star(re) => Some(re.regex.fold("".r) { r => s"($r)*".r })

  /** The language of the regular expression. */
  lazy val lang: Lang = Lang(symbols, accept)

  /** Convert a regular expression to a DFA */
  def toDFA: DFA = toNFA.toDFA

  /** Convert a regular expression to an NFA */
  def toNFA: NFA = toENFA.toNFA

  /** Convert a regular expression to an epsilon-NFA */
  def toENFA: ENFA = Implementation.reToENFA(this)

  /** The simplified regular expression */
  lazy val simplified: RE = this match
    case Union(l, r) =>
      (l.simplified, r.simplified) match
        case (Emp, r)                           => r
        case (l, Emp)                           => l
        case (Eps, Star(r))                     => Star(r)
        case (Star(l), Eps)                     => Star(l)
        case (l, Star(r)) if l == r             => Star(r)
        case (Star(l), r) if l == r             => Star(l)
        case (Union(Eps, l), Star(r)) if l == r => Star(r)
        case (Union(l, Eps), Star(r)) if l == r => Star(r)
        case (Star(l), Union(Eps, r)) if l == r => Star(l)
        case (Star(l), Union(r, Eps)) if l == r => Star(l)
        case (x, Concat(Star(y), z)) if x == z  => Concat(Star(y), z)
        case (Concat(Star(x), y), z) if y == z  => Concat(Star(x), y)
        case (l, r) if l == r                   => l
        case (l, r)                             => Union(l, r)
    case Concat(l, r) =>
      (l.simplified, r.simplified) match
        case (Emp, _)                           => Emp
        case (_, Emp)                           => Emp
        case (Eps, r)                           => r
        case (l, Eps)                           => l
        case (Union(Eps, l), Star(r)) if l == r => Star(r)
        case (Union(l, Eps), Star(r)) if l == r => Star(r)
        case (Star(l), Union(Eps, r)) if l == r => Star(l)
        case (Star(l), Union(r, Eps)) if l == r => Star(l)
        case (l, r)                             => Concat(l, r)
    case Star(re) =>
      re.simplified match
        case Emp           => Eps
        case Eps           => Eps
        case Union(Eps, r) => Star(r)
        case Union(l, Eps) => Star(l)
        case re            => Star(re)
    case _ => this

// A parser for regular expressions
object RE extends RegexParsers with PackratParsers:
  def apply(reStr: String): RE = parseAll(union, reStr).get
  lazy val symbol: Parser[Symbol] = "[0-9a-z]".r ^^ { case s => s.head }
  lazy val union: PackratParser[RE] =
    rep1sep(concat, "|") ^^ { _.reduce(Union(_, _)) }
  lazy val concat: PackratParser[RE] =
    rep1(star) ^^ { _.reduce(Concat(_, _)) }
  lazy val star: PackratParser[RE] =
    term ~ rep("*") ^^ { case re ~ s => s.foldLeft(re)((re, _) => Star(re)) }
  lazy val term: PackratParser[RE] =
    "</>" ^^^ Emp |
    "<e>" ^^^ Eps |
    symbol ^^ { Sym(_) } |
    "(" ~> union <~ ")"

package kuplrg

/** The type definitions of tape symbols */
type TapeSymbol = Char

/** The type definition of tape */
type Tape = String

/** The type definitions of move for tape head */
enum HeadMove { case L, R }

/** The type definition of configurations for TMs */
case class Config(state: State, tape: Tape, index: Int) {

  /** The string form of the configuration */
  def stringForm: String =
    val space = "\n" + " " * index
    tape + space + "^" + space + s"$state"
}

/** The definition of Turing machines (TM)
  *
  * @constructor
  *   create a new TM
  *
  * @param states
  *   the set of states
  * @param symbols
  *   the set of symbols
  * @param tapeSymbols
  *   the set of tape symbols
  * @param trans
  *   the transition function
  * @param initState
  *   the initial state
  * @param blank
  *   the blank symbol
  * @param finalStates
  *   the set of final states
  */
case class TM(
  states: Set[State],
  symbols: Set[Symbol],
  tapeSymbols: Set[TapeSymbol],
  trans: Map[(State, TapeSymbol), (State, TapeSymbol, HeadMove)],
  initState: State,
  blank: TapeSymbol,
  finalStates: Set[State],
) extends Acceptable
  with Computable {

  import TM.*
  import HeadMove.*

  /** One-step move in a TM */
  def move(config: Config): Option[Config] =
    val Config(curSt, tape, k) = config
    val (n, x) = (tape.size, tape(k))
    for {
      (newSt, y, to) <- trans.get((curSt, x))
      next = Config(newSt, tape.updated(k, y), k + (if (to == L) -1 else 1))
    } yield normalize(next)

  /** The initial configuration of a TM */
  def init(word: Word): Config = word match
    case a <| x => Config(initState, word, 0)
    case _      => Config(initState, blank.toString, 0)

  /** The configuration at which the TM halts */
  final def haltsAt(config: Config): Config = move(config) match
    case None       => config
    case Some(next) => haltsAt(next)

  /** The acceptance of a word by TM */
  def accept(w: Word): Boolean = finalStates.contains(haltsAt(init(w)).state)

  /** The computation of a word by TM */
  def compute(word: Word): Option[Word] =
    val Config(state, tape, k) = haltsAt(init(word))
    val (n, x) = (tape.size, tape(k))
    if (k == 0 && finalStates.contains(state)) {
      if (x == blank && n == 1) Some("")
      else if (tape.forall(symbols.contains)) Some(tape.mkString)
      else None
    } else None

  /** The path of a word in a TM */
  def pathOf(word: Word): List[Config] =
    def get(c: Config, acc: List[Config]): List[Config] = move(c) match
      case None    => (c :: acc).reverse
      case Some(d) => get(d, c :: acc)
    val b = blank.toString
    get(init(word), Nil)

  /** The dump of the path of a word in a TM */
  def dumpPath(w: Word): Unit = for { c <- pathOf(w) } println(c.stringForm)

  /** Checks if the TM is valid, or throws an exception */
  lazy val mustValid: this.type =
    val diff = symbols -- tapeSymbols
    if (diff.nonEmpty)
      error(s"Some input symbols are not tape symbols: ${diff.mkString(", ")}")
    for {
      q <- states
      x <- tapeSymbols
      (p, y, d) <- trans.get((q, x))
      if !states.contains(p) || !tapeSymbols.contains(y)
      tranStr = s"$q -> $p [$x -> $y] $d"
    } error(s"Invalid transition: $tranStr")
    if (!states.contains(initState))
      error(s"Invalid initial state: $initState")
    if (!tapeSymbols.contains(blank))
      error(s"Invalid blank symbol: $blank")
    if (symbols.contains(blank))
      error(s"The blank symbol should not be an input symbol: $blank")
    val invalidFinals = finalStates.filter(!states.contains(_))
    if (invalidFinals.nonEmpty)
      error(s"Invalid final states: ${invalidFinals.mkString(", ")}")
    this

  /** The normalized configuration of a TM */
  private def normalize(config: Config): Config =
    val Config(state, tape, k) = config
    val (n, b) = (tape.size, blank.toString)
    val (n0, tape0, k0) =
      if (k < 0) (-k + n, (b * -k) + tape, 0)
      else (n, tape, k)
    val (tape1, k1) =
      if (k0 >= n0) (tape0 + (b * (k0 - n0 + 1)), k0)
      else (tape0, k0)
    val c = Config(state, tape1, k1)
    val l = c.tape.indexWhere(_ != blank)
    val r = c.tape.reverseIterator.indexWhere(_ != blank)
    val (lower, upper) =
      if (l == -1) (c.index, c.index)
      else (l min c.index, (c.tape.size - 1 - r) max c.index)
    Config(c.state, c.tape.slice(lower, upper + 1), c.index - lower)
}

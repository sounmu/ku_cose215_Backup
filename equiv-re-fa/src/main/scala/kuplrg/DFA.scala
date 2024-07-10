package kuplrg

import kuplrg.RE.*

/** The definition of deterministic finite automaton (DFA)
  *
  * @constructor
  *   create a new DFA
  *
  * @param states
  *   the set of states
  * @param symbols
  *   the set of symbols
  * @param trans
  *   the transition function
  * @param initState
  *   the initial state
  * @param finalStates
  *   the set of final states
  */
case class DFA(
  states: Set[State],
  symbols: Set[Symbol],
  trans: Map[(State, Symbol), State],
  initState: State,
  finalStates: Set[State],
) extends FA:

  /** The extended transition function of DFA
    *
    * @param q
    *   the current state
    * @param w
    *   the current word
    * @return
    *   the next state
    */
  def extTrans(q: State, w: Word): State = w match
    case ""     => q
    case x <| w => extTrans(trans(q, x), w)

  /** The acceptance of a word by DFA
    *
    * @param w
    *   the word to be checked for acceptance
    * @return
    *   `true` if the word is accepted, `false` otherwise
    */
  def accept(w: Word): Boolean =
    finalStates.contains(extTrans(initState, w))

  /** The normalized DFA with states from 1 to n */
  def normalized: DFA =
    if (states.max == states.size && states.min == 1) this
    else
      val stateList = states.toList.sorted
      val map = stateList.zip(1 to states.size).toMap
      val n = stateList.length
      val newTrans = (for {
        q <- stateList
        a <- symbols
      } yield (map(q), a) -> map(trans(q, a))).toMap
      DFA(
        states = (1 to n).toSet,
        symbols = symbols,
        trans = newTrans,
        initState = map(initState),
        finalStates = finalStates.flatMap(map.get),
      )

object DFA:
  /** Create a new DFA from a encoded data
    *
    * @param n
    *   the number of states
    * @param symbols
    *   the set of symbols as a string
    * @param trans
    *   the encoded transition function
    * @param finalStates
    *   the encoded set of final states
    *
    * @return
    *   a new DFA
    */
  def apply(
    n: Int,
    symbols: String,
    trans: BigInt,
    finalStates: BigInt,
  ): DFA =
    val stateList = (1 to n).toList
    val symbolList = symbols.toList.sorted
    val m = symbolList.length
    val transUpper = BigInt(n).pow(n * m)
    if (trans < 0 || trans >= transUpper)
      error(s"Transition function should be in the range [0, $transUpper)")
    val finalUpper = BigInt(2).pow(n)
    if (finalStates < 0 || finalStates >= finalUpper)
      error(s"Final states should be in the range [0, $finalUpper)")
    DFA(
      states = stateList.toSet,
      symbols = symbolList.toSet,
      trans =
        val pairs = stateList.flatMap(q => symbolList.map((q, _)))
        val (map, _) = pairs.foldLeft((Map[(State, Symbol), State](), trans)) {
          case ((m, t), (q, a)) =>
            (m + ((q, a) -> (t % n + 1).toInt), t / n)
        }
        map
      ,
      initState = 1,
      finalStates =
        val (set, _) = stateList.foldLeft((Set[State](), finalStates)) {
          case ((s, f), q) => (if (f % 2 == 1) s + q else s, f / 2)
        }
        set,
    )

  def apply(n: Int, symbols: String, trans: Int, finals: Int): DFA =
    DFA(n, symbols, BigInt(trans), BigInt(finals))

  def apply(n: Int, symbols: String, trans: String, finals: String): DFA =
    DFA(n, symbols, BigInt(trans, ENCODE_BASE), BigInt(finals, ENCODE_BASE))

  val ENCODE_BASE = 36

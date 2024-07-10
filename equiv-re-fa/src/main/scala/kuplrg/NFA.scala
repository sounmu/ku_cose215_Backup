package kuplrg

/** The definition of non-deterministic finite automaton (NFA)
  *
  * @constructor
  *   create a new NFA
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
case class NFA(
  states: Set[State],
  symbols: Set[Symbol],
  trans: Map[(State, Symbol), Set[State]],
  initState: State,
  finalStates: Set[State],
) extends FA:

  /** The extended transition function of NFA
    *
    * @param qs
    *   the current set of states
    * @param w
    *   the current word
    * @return
    *   the set of next possible states
    */
  def extTrans(qs: Set[State], w: Word): Set[State] = w match
    case ""     => qs
    case x <| w => extTrans(qs.flatMap(q => trans(q, x)), w)

  /** The acceptance of a word by NFA
    *
    * @param w
    *   the word to be checked for acceptance
    * @return
    *   `true` if the word is accepted, `false` otherwise
    */
  def accept(w: Word): Boolean =
    extTrans(Set(initState), w).intersect(finalStates).nonEmpty

  /** The normalized NFA with states from 1 to n */
  def normalized: NFA =
    if (states.max == states.size && states.min == 1) this
    else
      val stateList = states.toList.sorted
      val map = stateList.zip(1 to states.size).toMap
      val n = stateList.length
      val newTrans = (for {
        ((q, a), ps) <- trans
      } yield (map(q), a) -> ps.map(map)).withDefaultValue(Set())
      NFA(
        states = (1 to n).toSet,
        symbols = symbols,
        trans = newTrans,
        initState = map(initState),
        finalStates = finalStates.flatMap(map.get),
      )

object NFA:
  /** Create a new NFA from a encoded data
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
    *   a new NFA
    */
  def apply(
    n: Int,
    symbols: String,
    trans: BigInt,
    finalStates: BigInt,
  ): NFA =
    val stateList = (1 to n).toList
    val symbolList = symbols.toList.sorted
    val m = symbolList.length
    val base = BigInt(2).pow(n)
    val transUpper = base.pow(n * m)
    if (trans < 0 || trans >= transUpper)
      error(s"Transition function should be in the range [0, $transUpper)")
    val finalUpper = BigInt(2).pow(n)
    if (finalStates < 0 || finalStates >= finalUpper)
      error(s"Final states should be in the range [0, $finalUpper)")
    NFA(
      states = stateList.toSet,
      symbols = symbolList.toSet,
      trans =
        val pairs = for { q <- stateList; a <- symbolList } yield (q, a)
        val (map, _) = pairs.foldLeft[
          (Map[(State, Symbol), Set[State]], BigInt),
        ]((Map.empty, trans)) {
          case ((m, t), (q, a)) if t % base == 0 => (m, t / base)
          case ((m, t), (q, a)) =>
            val (next, r) = (1 to n).foldLeft((Set[State](), t)) {
              case ((s, r), p) => (if (r % 2 == 1) s + p else s, r / 2)
            }
            (m + ((q, a) -> next), r)
        }
        map.withDefaultValue(Set())
      ,
      initState = 1,
      finalStates =
        val (set, _) = stateList.foldLeft((Set[State](), finalStates)) {
          case ((s, f), q) => (if (f % 2 == 1) s + q else s, f / 2)
        }
        set,
    )

  def apply(n: Int, symbols: String, trans: Int, finals: Int): NFA =
    NFA(n, symbols, BigInt(trans), BigInt(finals))

  def apply(n: Int, symbols: String, trans: String, finals: String): NFA =
    NFA(n, symbols, BigInt(trans, ENCODE_BASE), BigInt(finals, ENCODE_BASE))
  val ENCODE_BASE = 36

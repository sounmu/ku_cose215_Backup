package kuplrg

/**
 * The definition of non-deterministic finite automaton (NFA)
 *
 * @constructor create a new NFA
 *
 * @param states      the set of states
 * @param symbols     the set of symbols
 * @param trans       the transition function
 * @param initState   the initial state
 * @param finalStates the set of final states
 */
case class NFA(
  states: Set[State],
  symbols: Set[Symbol],
  trans: Map[(State, Symbol), Set[State]],
  initState: State,
  finalStates: Set[State],
) extends FA:

  /**
   * The extended transition function of NFA
   *
   * @param qs the current set of states
   * @param w  the current word
   * @return the set of next possible states
   */
  def extTrans(qs: Set[State], w: Word): Set[State] = w match
    case ""     => qs
    case x <| w => extTrans(qs.flatMap(q => trans(q, x)), w)


  /**
   * The acceptance of a word by NFA
   *
   * @param w the word to be checked for acceptance
   * @return `true` if the word is accepted, `false` otherwise
   */
  def accept(w: Word): Boolean =
    extTrans(Set(initState), w).intersect(finalStates).nonEmpty

package kuplrg

/**
 * The definition of deterministic finite automaton (DFA)
 *
 * @constructor create a new DFA
 *
 * @param states      the set of states
 * @param symbols     the set of symbols
 * @param trans       the transition function
 * @param initState   the initial state
 * @param finalStates the set of final states
 */
case class DFA(
  states: Set[State],
  symbols: Set[Symbol],
  trans: Map[(State, Symbol), State],
  initState: State,
  finalStates: Set[State],
) extends FA:

  /**
   * The extended transition function of DFA
   *
   * @param q the current state
   * @param w the current word
   * @return the next state
   */
  def extTrans(q: State, w: Word): State = w match
    case ""     => q
    case x <| w => extTrans(trans(q, x), w)

  /**
   * The acceptance of a word by DFA
   *
   * @param w the word to be checked for acceptance
   * @return `true` if the word is accepted, `false` otherwise
   */
  def accept(w: Word): Boolean =
    finalStates.contains(extTrans(initState, w))

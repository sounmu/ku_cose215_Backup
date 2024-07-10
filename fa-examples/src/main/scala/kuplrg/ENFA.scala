package kuplrg

/**
 * The definition of epsilon-NFA (ENFA)
 *
 * @constructor create a new ENFA
 *
 * @param states      the set of states
 * @param symbols     the set of symbols
 * @param trans       the transition function
 * @param initState   the initial state
 * @param finalStates the set of final states
 */
case class ENFA(
  states: Set[State],
  symbols: Set[Symbol],
  trans: Map[(State, Option[Symbol]), Set[State]],
  initState: State,
  finalStates: Set[State],
) extends FA:

  /**
   * The definitions of epsilon-closures
   *
   * @param q the current state
   * @return the set of states reachable from `q` by epsilon-transitions
   */
  def eclo(q: State): Set[State] =
    def aux(rest: List[State], visited: Set[State]): Set[State] = rest match
      case Nil          => visited
      case p :: targets => aux(
        rest    = (trans((p, None)) -- visited).toList ++ targets,
        visited = visited + p,
      )
    aux(List(q), Set())

  /**
   * The epsilon-closure of a set of states
   *
   * @param qs the current set of states
   * @return the set of states reachable from `qs` by epsilon-transitions
   */
  def eclo(qs: Set[State]): Set[State] = qs.flatMap(eclo)

  /**
   * The extended transition function of epsilon-NFA
   *
   * @param qs the current set of states
   * @param w  the current word
   * @return the set of next possible states
   */
  def extTrans(qs: Set[State], w: Word): Set[State] = w match
    case ""     => eclo(qs)
    case x <| w => extTrans(eclo(qs).flatMap(q => trans(q, Some(x))), w)

  /**
   * The acceptance of a word by epsilon-NFA
   *
   * @param w the word to be checked for acceptance
   * @return `true` if the word is accepted, `false` otherwise
   */
  def accept(w: Word): Boolean =
    extTrans(Set(initState), w).intersect(finalStates).nonEmpty

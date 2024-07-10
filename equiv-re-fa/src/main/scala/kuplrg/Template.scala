package kuplrg

trait Template {
  def enfaToDFA(enfa: ENFA): DFA
  def reToENFA(re: RE): ENFA
  def dfaToRE(dfa: DFA): RE

  /** A simplified epsilon-NFA satisfying the following properties:
    *
    * - States are sequentially numbered from `from` to `to`
    * - The initial state `from` is the initial state
    * - The last state `to` is the final state
    * - No transition to the initial state
    * - No transition from the final state
    *
    * @constructor
    *   create a new simplified epsilon-NFA
    * @param from
    *   the initial state
    * @param edges
    *   the set of edges
    * @param to
    *   the final state
    */
  case class SFA(
    from: State,
    edges: Set[SFA.Edge],
    to: State,
  ):

    /** Convert a simplified epsilon-NFA to an epsilon-NFA */
    def toENFA: ENFA = ENFA(
      states = (from to to).toSet,
      symbols = edges.flatMap(_.symbol).toSet,
      trans = edges
        .groupBy(t => (t.from, t.symbol))
        .map((k, v) => (k, v.map(_.to).toSet))
        .toMap
        .withDefaultValue(Set()),
      initState = from,
      finalStates = Set(to),
    )

  object SFA:
    /** A transition edge allowing epsilon-transition
      *
      * @param from
      *   the source state
      * @param symbol
      *   the symbol to be consumed or `None` for epsilon-transition
      * @param to
      *   the target state
      */
    case class Edge(from: State, symbol: Option[Symbol], to: State)
}

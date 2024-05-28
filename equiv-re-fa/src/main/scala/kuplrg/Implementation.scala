package kuplrg

object Implementation extends Template {

  import RE.*
  import SFA.Edge
  import Fuzzer.*

  /**
   * This is the playground for you to run your implementation.
   * Do whatever you want here and run `sbt run` to see the result.
   */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // example RE
    val re1 = RE("</>")
    val re2 = RE("a|b")
    val re3 = RE("0*")

    // example DFA
    val dfa1 = DFA(1, "01", "0", "1")
    val dfa2 = DFA(2, "01", "3", "2")
    val dfa3 = DFA(2, "ab", "5", "2")

    // example ENFA
    val enfa1 = ENFA(4, "ab", "fs8wsgw", "8")
    val enfa2 = ENFA(6, "ab", "ost0183xsrlx05m", "w")
    val enfa3 = ENFA(6, "01", "urgp7qvlhgmm39c", "w")

    // You can dump an FA to see it in the automaton viewer.
    // After running this program, open `viewer/index.html` in your browser.
    dfa1.dump

    println("--------------------------------------------------")

    // You can dump an RE to see its string form and original form.
    re1.dump

    println("--------------------------------------------------")

    // You can generate random FA or RE to test your implementation.
    //
    // fuzzDFA(n = 3, symbols = "01").dump
    // fuzzENFA(n = 3, symbols = "01").dump
    fuzzRE(depth = 3, symbols = "01").dump

    println("--------------------------------------------------")
  }

  // ---------------------------------------------------------------------------
  // Problem 1: Regular Expressions to epsilon-NFA
  // ---------------------------------------------------------------------------
  /** Convert a regular expression to an epsilon-NFA
   *
   *  @param re
   *  @return
   *    the equivalent epsilon-NFA
   */
  def reToENFA(re: RE): ENFA = reToSFA(re, 1).toENFA

  /** Convert a regular expression to a simplified epsilon-NFA with a given
    * initial state `i`
    *
    * @param re
    *   the regular expression
    * @param i
    *   the initial state
    * @return
    *   the simplified epsilon-NFA
    */
  def reToSFA(re: RE, i: State): SFA = re match
    case Emp => SFA(
      from = i,
      edges = Set(
        Edge(i, None, i),
      ),
      to = i + 1,
    )
    case Eps => SFA(
      from = i,
      edges = Set(
        Edge(i, None, i+1),
      ),
      to = i + 1,
    )
    case Sym(symbol) => SFA(
      from = i,
      edges = Set(
        Edge(i, Option(symbol), i+1),
      ),
      to = i + 1,
    )
    case Union(left, right) =>
      val SFA(_, ledges, j) = reToSFA(left, i + 1)
      val SFA(_, redges, k) = reToSFA(right, j + 1)
      SFA(
        from = i,
        edges = ledges ++ redges ++ Set(
          Edge(i, None, i + 1),
          Edge(i, None, j + 1),
          Edge(j, None, k + 1),
          Edge(k, None, k + 1),
        ),
        to = k + 1,
      )
    case Concat(left, right) => 
      val SFA(_, ledges, j) = reToSFA(left, i)
      val SFA(_, redges, k) = reToSFA(right, j + 1)
      SFA(
        from = i,
        edges = ledges ++ redges ++ Set(
          Edge(j, None, j+1),
        ),
        to = k,
      )
    case Star(re) => 
      val SFA(_, returnedges, j) = reToSFA(re, i + 1)
      SFA(
        from = i,
        edges = returnedges ++ Set(
          Edge(i, None, i + 1),
          Edge(j, None, i + 1),
          Edge(i, None, j + 1),
          Edge(j, None, j + 1),
        ),
        to = j + 1,
      )

  // ---------------------------------------------------------------------------
  // Problem 2: DFA to Regular Expressions
  // ---------------------------------------------------------------------------
  /** Convert a DFA to a regular expression.
    *
    * @param dfa
    *   the given DFA
    * @return
    *   the equivalent regular expression
    */
  
  def dfaToRE(dfa: DFA): RE =
    dfa.finalStates
      .map(Implementation.reForPaths(dfa, dfa.initState, _, dfa.states.size))
      .foldLeft(Emp)(Union(_, _))

  /** A regular expression accepting paths from `i` to `j` with intermediate
    * states bounded by `k` in a given DFA `dfa`. Assume that the given DFA
    * `dfa` is already normalized (i.e., the states of DFA are 1, 2, ..., n).
    *
    * @param dfa
    *   the given DFA
    * @param i
    *   the initial state
    * @param j
    *   the final state
    * @param k
    *   the bound of intermediate states
    */
  def reForPaths(dfa: DFA, i: State, j: State, k: State): RE = k match
    case 0 => 
      val transitionRE = dfa.symbols.flatMap { symbol =>
        dfa.trans.get((i, symbol)).filter(_ == j).map(_ => Sym(symbol))
      }.toList match {
        case Nil => Emp
        case list => list.reduce(Union(_, _))
      }
      if (i == j) Union(Eps, transitionRE)
      else transitionRE
    case _ =>
      val withoutK = reForPaths(dfa, i, j, k - 1)
      val throughK = Concat(Concat(reForPaths(dfa, i, k, k - 1), Star(reForPaths(dfa, k, k, k - 1))), reForPaths(dfa, k, j, k - 1))
      Union(withoutK, throughK)

  // ---------------------------------------------------------------------------
  // Problem 3: epsilon-NFA to DFA
  // ---------------------------------------------------------------------------
  /** Convert an epsilon-NFA to a DFA.
    *
    * @param enfa
    *   the epsilon-NFA
    * @return
    *   the equivalent DFA
    */
  def enfaToDFA(enfa: ENFA): DFA =
    // An auxiliary function to find all reachable set of states in epsilon-NFA
    // using depth-first search, and return the mapping from each of them to the
    // corresponding state in DFA.
    def aux(
      qsList: List[Set[State]],
      k: Int,
      map: Map[Set[State], Int],
    ): Map[Set[State], State] = qsList match
      case Nil => map
      case qs :: rest =>
        // The next possible set of states
        val next: Set[Set[State]] =
          enfa.symbols.map(a => enfa.extTrans(qs, a.toString))
        // The set of already visited set of states
        val visited: Set[Set[State]] = map.keySet
        // The set of states
        val yets: Set[Set[State]] = next -- visited - qs
        aux(
          yets.toList ++ rest, 
          k + 1,
          map + (qs -> k),
        )
    val init: Set[State] = enfa.eclo(enfa.initState)
    // The mapping from reachable set of states in epsilon-NFA to the
    // corresponding state in DFA
    val map: Map[Set[State], State] = aux(List(init), 1, Map.empty)

    val dfaTrans: Map[(Int, Symbol), Int] = map.keys.flatMap { stateSet =>
      enfa.symbols.flatMap { symbol =>
        val targetSet = enfa.extTrans(stateSet, symbol.toString)
        map.get(targetSet).map { targetDfaState =>
          ((map(stateSet), symbol), targetDfaState)
        }
      }
    }.toMap

    val dfaFinalStates: Set[Int] = map.collect {
      case (stateSet, dfaState) 
      if stateSet.exists(enfa.finalStates.contains) => dfaState
    }.toSet

    val dfa = DFA(
      states = map.values.toSet,
      symbols = enfa.symbols,
      trans = dfaTrans,
      initState = map(init),
      finalStates = dfaFinalStates,
    )

    dfa
  }
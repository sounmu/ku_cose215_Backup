package kuplrg

object Implementation extends Template {

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // You can check your implementation here.

    println("--------------------------------------------------")
  }

  // Convert a PDA with final states to a PDA with empty stacks
  def pdafs2es(pda: PDA): PDA = {
    //println(pda.states.max)
    val newStartState = pda.states.max + 1
    val newFinalState = newStartState + 1
    val newInitAlphabet: String = "z"

    val emptyStackTrans = for {
      state <- pda.finalStates
      stackSymbol <- pda.alphabets + newInitAlphabet
    } yield ((state, None, stackSymbol), Set((newFinalState, List.empty[String])))

    val newFinalTrans = for {
      stackSymbol <- pda.alphabets + newInitAlphabet
    } yield ((newFinalState, None, stackSymbol), Set((newFinalState, List.empty[String])))
    
    val newStartTrans = Set(
      ((newStartState, None, newInitAlphabet), Set((pda.initState, List(pda.initAlphabet, newInitAlphabet))))
    )

    val existingTrans = pda.trans.map { case (k, v) => (k, v.toSet) }.toSet

    val combinedTransSeq = existingTrans ++ emptyStackTrans ++ newFinalTrans ++ newStartTrans

    val combinedTrans: Map[(Int, Option[Char], String), Set[(Int, List[String])]] =
      combinedTransSeq
        .groupBy(_._1)
        .view
        .mapValues(_.flatMap(_._2).toSet)
        .toMap

    PDA(
      states = pda.states + newStartState + newFinalState,
      symbols = pda.symbols,
      alphabets = pda.alphabets + newInitAlphabet,
      trans = combinedTrans.withDefaultValue(Set()),
      initState = newStartState,
      initAlphabet = newInitAlphabet,
      finalStates = Set(),
    )
  }

  // Convert a PDA with empty stacks to a PDA with final states
  def pdaes2fs(pda: PDA): PDA = {
    //println(pda.states.max)
    val newStartState = pda.states.max + 1
    val newFinalState = newStartState + 1
    val newInitAlphabet: String = "z"

    val epsilonTrans = for {
      state <- pda.states
    } yield ((state, None, newInitAlphabet), Set((newFinalState, List(newInitAlphabet))))

    val newStartTrans = Set(
      ((newStartState, None, newInitAlphabet), Set((pda.initState, List(pda.initAlphabet, newInitAlphabet))))
    )

    val existingTrans = pda.trans.map { case (k, v) => (k, v.toSet) }.toSet

    val combinedTransSeq = existingTrans ++ epsilonTrans ++ newStartTrans

    val combinedTrans: Map[(Int, Option[Char], String), Set[(Int, List[String])]] =
      combinedTransSeq
        .groupBy(_._1)
        .view
        .mapValues(_.flatMap(_._2).toSet)
        .toMap

    PDA(
      states = pda.states + newStartState + newFinalState,
      symbols = pda.symbols,
      alphabets = pda.alphabets + newInitAlphabet,
      trans = combinedTrans.withDefaultValue(Set()),
      initState = newStartState,
      initAlphabet = newInitAlphabet,
      finalStates = Set(newFinalState),
    )
  }

  // Convert a CFG to a PDA with empty stacks
  def cfg2pdaes(cfg: CFG): PDA = {
    val q = 0
    val s = "S" 
    val nts1: Set[String] = cfg.nts.map(_.toString) //nonterminal이 S
    val cfgsymbols: Set[Char] = cfg.symbols //terminal이 a, b
    val start: String = cfg.start.toString
    val rule: Map[kuplrg.Nt, List[kuplrg.Rhs]] = cfg.rules
    val newsymbolAlphabet: Set[kuplrg.Symbol] = cfg.symbols

    val state: Set[Int] = Set(q)

    val initialTransition: Map[(Int, Option[Char], String), Set[(Int, List[String])]] = Map()
    
    def rhsConvert(rhs: kuplrg.Rhs): List[String] = {
      rhs.seq.map {
        case nt: Nt => nt.toString
        case s: Symbol => s.toString
      }
    }

    val transitionWithProduct = rule.foldLeft(initialTransition) {
    case (trans, (variable, rules)) =>
      rules.foldLeft(trans) {
        case (innerTrans, rule1) =>
          val alpha = rhsConvert(rule1)
          val varStr = variable.toString
          innerTrans.updated(
            (q, None, varStr),
            innerTrans.getOrElse((q, None, varStr), Set()) + ((q, alpha))
          )
      }
    }

    val finalTransition = cfgsymbols.foldLeft(transitionWithProduct){
      case (trans, terminal) =>
        trans.updated(
          (q, Some(terminal), terminal.toString), Set((q, List()))
        )
    }

    PDA(
      states = state,
      symbols = cfg.symbols,
      alphabets = nts1 ++ cfgsymbols.map(_.toString),
      trans = finalTransition.withDefaultValue(Set()), 
      initState = q,
      initAlphabet = s,
      finalStates = Set() // empty stack이라 비워두기.
    )
  }

  // Convert a PDA with empty stacks to a CFG
  def pdaes2cfg(pda: PDA): CFG = ???

}

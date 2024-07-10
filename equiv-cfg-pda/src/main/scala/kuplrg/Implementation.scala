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
    val newInitAlphabet: String = "z'"

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
  def pdaes2cfg(pda: PDA): CFG = { // 입실론 <e>

    def String2Rhs(str: String): kuplrg.Rhs = { //대문자 소문자 비교로 Nt인지 Symbol인지
      val seq: List[Nt | Symbol] = str.split(" ").toList.map {
        case s if s.forall(c => c.isUpper || c.isDigit) => s: Nt
        case s => s.head: Symbol
      }
      Rhs(seq)
    }
    val n = pda.states.size

    val Nts: Set[String] = (for {
      ((q, a, x), set) <- pda.trans
      (p, gamma) <- set
    } yield s"A$q$x$p").toSet

    //println(Nts)
    val start: String = "S"
    val nonTerminals: Set[String] = (Nts ++ Set(start))
    //println(nonTerminals)
    val initialProduct: Map[Nt, List[Rhs]] = (for {
      q <- 0 until n
      x <- pda.alphabets
      p <- 0 until n
    } yield s"A$q$x$p").map(key => key -> List[Rhs]()).toMap + (start -> pda.states.toList.map(num => String2Rhs(s"A0Z$num"))) //S0Z0, S0Z1

    //println(initialProduct)
    //transition : Map[(Int, Option[Char], String), Set[(Int, List[String])]]
    val rules: Map[Nt, List[Rhs]] = pda.trans.foldLeft(initialProduct) { case (acc, ((q, a, x), set)) =>
      set.foldLeft(acc) { case (innerAcc, (p, gamma)) => 
        val newRules = for {
          i <- 0 until n
          key = s"A$q$x$i"
        } yield {
          val valueSome: List[Rhs] = a match {
            case Some(symbol) =>
              if (gamma.isEmpty) {
                List(String2Rhs(symbol.toString))
              } else {
                val rhsSeqs = for (j <- 0 until n) yield {
                  val rhsSeq = (symbol.toString +: gamma.zipWithIndex.map { case (g, idx) =>
                    if (idx == 0) s"A$p$g$j"
                    else if (idx == gamma.size - 1) s"A${j+idx-1}$g$i"
                    else s"A${j+idx-1}$g${j+idx}"
                  }).mkString(" ")
                  String2Rhs(rhsSeq)
                }
                rhsSeqs.toList
              }
            case None => List()
          }

          val valueNone: List[Rhs] = a match {
            case None => 
              if (gamma.isEmpty) {
                List(Rhs(Nil))
              } else {
                val rhsSeqs = for (j <- 1 until n) yield {
                  val rhsSeq = gamma.zipWithIndex.map { case (g, idx) =>
                    if (idx == 0) s"A$j$g$i"
                    else if (idx == gamma.size - 1) s"A${i+idx-1}$g$i"
                    else s"A${i+idx-1}$g${i+idx}"
                  }.mkString(" ")
                  String2Rhs(rhsSeq)
                }
                rhsSeqs.toList
              }
            case Some(_) => List()
          }
          if (gamma.isEmpty) {
            s"A$q$x$p" -> (valueSome ++ valueNone)
          } else {
            key -> (valueSome ++ valueNone)
          }
        } //p는 Set안에 Int, gamma는 List[]

          newRules.foldLeft(innerAcc) { case (mapAcc, (newKey, newValue)) =>
            mapAcc.updated(newKey, mapAcc.getOrElse(newKey, List()) ++ newValue)
          }
      }
    }
    
   
    //println(completeRules)
    CFG(nonTerminals, pda.symbols, start, rules)
  }
}
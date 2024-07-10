package kuplrg

object Fuzzer {

  import RE.*

  /** Random number generator. */
  val rand = new scala.util.Random

  /** Fuzzes a DFA with `n` states and `symbols` symbols. */
  def fuzzDFA(n: Int, symbols: String): DFA = DFA(
    (1 to n).toSet,
    symbols.toSet,
    (for {
      q <- (1 to n).toSet
      a <- symbols.toSet
    } yield (q, a) -> (randInt(n) + 1)).toMap,
    1,
    (1 to n).toSet.filter(_ => randBool),
  )

  /** Fuzzes an NFA. */
  def fuzzNFA(n: Int, symbols: String, density: Double = 0.5): NFA = NFA(
    (1 to n).toSet,
    symbols.toSet,
    (for {
      q <- (1 to n).toSet
      a <- symbols.toSet
      set = randIntSet(1, n, density)
    } yield (q, a) -> set).toMap.withDefaultValue(Set()),
    1,
    (1 to n).toSet.filter(_ => randBool(density)),
  )

  /** Fuzzes an ENFA. */
  def fuzzENFA(n: Int, symbols: String, density: Double = 0.5): ENFA = ENFA(
    (1 to n).toSet,
    symbols.toSet,
    (for {
      q <- (1 to n).toSet
      a <- symbols.toSet.map(Some(_)) + None
      set = randIntSet(1, n, density)
    } yield (q, a) -> set).toMap.withDefaultValue(Set()),
    1,
    (1 to n).toSet.filter(_ => randBool(density)),
  )

  /** Fuzzes a RE with `depth` and `symbols` symbols. */
  def fuzzRE(depth: Int, symbols: String): RE =
    fuzzRE(depth, symbols.toSet.toList)

  /** Fuzzes a RE with `depth` and `symbols` symbols. */
  def fuzzRE(depth: Int, symbols: List[Symbol]): RE =
    if (depth == 0 || randBool(0.5 / depth)) fuzzREBase(symbols)
    else fuzzREInd(depth, symbols)

  /** Fuzzes a base RE with `symbols` symbols. */
  def fuzzREBase(symbols: List[Symbol]): RE = randInt(3) match
    case 0 => Emp
    case 1 => Eps
    case 2 => Sym(symbols(randInt(symbols.length)))

  /** Fuzzes an inductive RE with `depth` and `symbols` symbols. */
  def fuzzREInd(depth: Int, symbols: List[Symbol]): RE = randInt(3) match
    case 0 => Union(fuzzRE(depth - 1, symbols), fuzzRE(depth - 1, symbols))
    case 1 => Concat(fuzzRE(depth - 1, symbols), fuzzRE(depth - 1, symbols))
    case 2 => Star(fuzzRE(depth - 1, symbols))

  /** Random integer set in the range `[from, to]` with density `density`. */
  def randIntSet(from: Int, to: Int, density: Double = 0.5): Set[Int] =
    (from to to).toSet.filter(_ => randBool(density))

  /** Random boolean. */
  def randBool: Boolean = rand.nextBoolean

  /** Random boolean with probability `probForTrue`. */
  def randBool(probForTrue: Double): Boolean = rand.nextDouble < probForTrue

  /** Random integer in the range `[0, bound)`. */
  def randInt(bound: Int): Int = rand.nextInt(bound)
}

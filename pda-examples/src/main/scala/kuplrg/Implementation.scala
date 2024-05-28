package kuplrg

object Implementation extends Template {

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // You can check your implementation here.
    val accept = pda_an_bn_final.acceptByFinalState
    println(s"pda_an_bn_final.acceptByFinalState(\"ab\")   = ${accept("ab")}")
    println(s"pda_an_bn_final.acceptByFinalState(\"aba\")  = ${accept("aba")}")
    println(s"pda_an_bn_final.acceptByFinalState(\"aabb\") = ${accept("aabb")}")

    println("--------------------------------------------------")
  }

  // PDA accpeting L = { a^n b^n | n >= 0 } by final states
  val pda_an_bn_final: PDA = PDA(
    states = Set(0, 1, 2),
    symbols = Set('a', 'b'),
    alphabets = Set("X", "Z"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("X", "Z"))),
      (0, Some('a'), "X") -> Set((0, List("X", "X"))),
      (0, None, "Z") -> Set((1, List("Z"))),
      (0, None, "X") -> Set((1, List("X"))),
      (1, Some('b'), "X") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List("Z"))),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(2),
  )

  // PDA accepting L = { w w^R | w \in {0, 1}* } by final states
  def pda_even_pal_final: PDA = PDA(
    states = Set(0, 1, 2),
    symbols = Set('0', '1'),
    alphabets = Set("X", "Y", "Z"),
    trans = Map(
      (0, Some('0'), "Z") -> Set((0, List("X", "Z"))),
      (0, Some('0'), "X") -> Set((0, List("X", "X"))),
      (0, Some('0'), "Y") -> Set((0, List("X", "Y"))),
      (0, Some('1'), "Z") -> Set((0, List("Y", "Z"))),
      (0, Some('1'), "Y") -> Set((0, List("Y", "Y"))),
      (0, Some('1'), "X") -> Set((0, List("Y", "X"))),

      (0, None, "Z") -> Set((1, List("Z"))),
      (0, None, "X") -> Set((1, List("X"))),
      (0, None, "Y") -> Set((1, List("Y"))),

      (1, Some('0'), "X") -> Set((1, List())),
      (1, Some('1'), "Y") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List("Z"))),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(2),
  )

  // PDA accpeting L = { w \in {a, b}* | N_a(w) > N_b(w) } by empty stacks
  def pda_more_as_empty: PDA = PDA(
    states = Set(0, 1),
    symbols = Set('a', 'b'),
    alphabets = Set("P", "N", "Z"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("P", "Z"))),
      (0, Some('a'), "P") -> Set((0, List("P", "P"))),
      (0, Some('a'), "N") -> Set((0, List())),
      (0, Some('b'), "Z") -> Set((0, List("N", "Z"))),
      (0, Some('b'), "P") -> Set((0, List())),
      (0, Some('b'), "N") -> Set((0, List("N", "N"))),
      (0, None, "P") -> Set((1, List())),
      (1, None, "P") -> Set((1, List())),
      (1, None, "Z") -> Set((1, List())),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(),
  )

  // PDA accepting L = { a^i b^j c^k | i, j, k >= 0 and (i = j or i = k) } by
  // final states
  def pda_abc_ij_ik_final: PDA = PDA( // 0 -> 1 -> 2 -> 3 , 0 -> 4 -> 5 -> 3 final state에 있고 no remaining symbol
    states = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    symbols = Set('a', 'b', 'c'),
    alphabets = Set("X", "Z"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((1, List("X", "Z"))),
      (0, Some('b'), "Z") -> Set((7, List("Z"))), // b로 시작할 경우 c가 들어오는 순간 안됨.
      (0, Some('c'), "Z") -> Set((3, List("Z"))),
      // i = j
      (1, Some('a'), "X") -> Set((1, List("X", "X"))),
      (1, Some('b'), "X") -> Set((2, List())), // ab 까지 있을 경우.
      (1, None, "X") -> Set((4, List("X"))), // 4로 전이해서 a와 c 비교하게.

      (2, Some('b'), "X") -> Set((2, List())),  // a와 b 비교.
      /*(2, Some('c'), "Z") -> Set((2, List("Z"))),
      (2, Some('c'), "X") -> Set((2, List("X"))),*/
      (2, None, "Z") -> Set((3, List("Z"))),

      (3, Some('c'), "Z") -> Set((3, List("Z"))),
      // i = k
      (4, Some('a'), "X") -> Set((4, List("X", "X"))),
      (4, None, "X") -> Set((5, List("X"))),

      (5, Some('b'), "Z") -> Set((5, List("Z"))),
      (5, Some('b'), "X") -> Set((5, List("X"))),
      (5, None, "X") -> Set((6, List("X"))),

      (6, Some('c'), "X") -> Set((6, List())),
      (6, None, "Z") -> Set((9, List("Z"))),

      (7, Some('b'), "Z") -> Set((7, List("Z"))),
      (7, None, "Z") -> Set((8, List("Z"))),

      (8, Some('c'), "Z") -> Set((8, List("Z"))),
      (8, Some('c'), "X") -> Set((8, List())),

    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(3, 7, 9),
  )

  // PDA accepting L = { a^i b^j | (i = 2n + 1 and j = 3n + 1) for n >= 0 } by
  // empty stacks
  def pda_a2n1_b3n1_empty: PDA = PDA( // i는 스택 3개 넣고 j는 2개 빼기, 근데 2개 뺄 수가 있나..상태 전이를 입실론 전이 말고 딴거로 해서 
    states = Set(0, 1, 2, 3, 4, 5),
    symbols = Set('a', 'b'),
    alphabets = Set("X", "Z"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((1, List("X", "Z"))),

      (1, Some('a'), "Z") -> Set((1, List("X", "X", "Z"))),
      (1, Some('a'), "X") -> Set((1, List("X", "X", "X"))),

      (1, Some('b'), "X") -> Set((2, List())),

      (2, Some('b'), "X") -> Set((4, List())), // 1, 1 & 3, 4 & 5, 7 &
      (4, None, "X") -> Set((5, List())),
      (5, None, "X") -> Set((2, List())),
      (2, None, "Z") -> Set((3, List())),
      
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(),
  )

  // PDA accpeting L = { a^i b^j c^k | i, j, k >= 0 and j = i + 2k } by final
  // states
  def pda_abc_j_i2k_final: PDA = PDA( // 
    states = Set(0, 1, 2, 3, 4),
    symbols = Set('a', 'b'),
    alphabets = Set("P", "T", "Z"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("P", "Z"))),
      (0, Some('a'), "P") -> Set((0, List("P", "P"))),
      (0, None, "Z") -> Set((1, List("Z"))), // P, T가 있을 때 그냥 state 변경
      (0, None, "P") -> Set((1, List("P"))),

      (1, Some('b'), "Z") -> Set((1, List("T", "Z"))),
      (1, Some('b'), "T") -> Set((1, List("T", "T"))),
      (1, Some('b'), "P") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List("Z"))),
      (1, None, "T") -> Set((2, List("T"))),

      (2, Some('c'), "T") -> Set((4, List())),
      (4, None, "T") -> Set((2, List())),

      (2, None, "Z") -> Set((3, List("Z"))),     
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(3),
  )

  // PDA accpeting L = { w \in { '(', ')', '{', '}', '[', ']' }* | w is
  // well-formed and satisfies the order: '()' <= '{}' <= '[]' } by empty stacks
  def pda_ord_brace_empty: PDA = PDA( // 
    states = Set(0, 1),
    symbols = Set('(', ')', '{', '}', '[', ']'),
    alphabets = Set("P", "Q", "R", "Z"),
    trans = Map(
      (0, Some('('), "Z") -> Set((0, List("P", "Z"))),
      (0, Some('('), "P") -> Set((0, List("P", "P"))),
      (0, Some('('), "Q") -> Set((0, List("P", "Q"))),
      (0, Some('('), "R") -> Set((0, List("P", "R"))),
      (0, Some(')'), "P") -> Set((0, List())),

      (0, Some('{'), "Z") -> Set((0, List("Q", "Z"))),
      (0, Some('{'), "Q") -> Set((0, List("Q", "Q"))),
      (0, Some('{'), "R") -> Set((0, List("Q", "R"))),
      (0, Some('}'), "Q") -> Set((0, List())), 

      (0, Some('['), "Z") -> Set((0, List("R", "Z"))),
      (0, Some('['), "R") -> Set((0, List("R", "R"))),
      (0, Some(']'), "R") -> Set((0, List())), 

      (0, None, "Z") -> Set((1, List())),     
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(),
  )

  // PDA accpeting L = { w \in { '0', '1', '+', '*' }* | w is an arithmetic
  // expression evaluated to an even number } by final states
  def pda_ae_even_final: PDA = PDA( // 
    states = Set(0, 1, 2, 3, 4, 5), // 3이면 짝수, 4이면 홀수.
    symbols = Set('0', '1', '*', '+'),   
    alphabets = Set("E", "O", "Z"),
    trans = Map(
      (0, Some('0'), "Z") -> Set((1, List("E", "Z"))),
      (0, Some('1'), "Z") -> Set((1, List("O", "Z"))),

      (0, Some('0'), "E") -> Set((1, List("E", "E"))),
      (0, Some('0'), "O") -> Set((1, List("E", "O"))),
      (0, Some('1'), "E") -> Set((1, List("O", "E"))),
      (0, Some('1'), "O") -> Set((1, List("O", "O"))),

      (1, Some('+'), "E") -> Set((0, List("E"))), 
      (1, Some('+'), "O") -> Set((0, List("O"))),

      (1, Some('*'), "E") -> Set((2, List("E"))), 
      (1, Some('*'), "O") -> Set((2, List("O"))),

      (2, Some('0'), "E") -> Set((1, List("E"))), 
      (2, Some('1'), "E") -> Set((1, List("E"))),
      (2, Some('0'), "O") -> Set((1, List("E"))),
      (2, Some('1'), "O") -> Set((1, List("O"))),

      (1, None, "E") -> Set((3, List())),
      (1, None, "O") -> Set((4, List())),
      (3, None, "O") -> Set((4, List())),
      (3, None, "E") -> Set((3, List())),
      (4, None, "O") -> Set((3, List())),
      (4, None, "E") -> Set((4, List())),
      (3, None, "Z") -> Set((5, List("Z"))),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(5),
  )

  // PDA accpeting L = { a_1 a_2 ... a_2n \in {a, b}* | n >= 1 and a_i = a_{n+i}
  // for some 1 <= i <= n } by empty stacks
  def pda_eq_pair_empty: PDA = PDA(
    states = Set(0, 1, 2, 3, 4, 5, 6, 7),
    symbols = Set('a', 'b'),
    alphabets = Set("A", "B", "X", "Z"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((1, List("A", "Z")), (0, List("X", "Z"))), //q1으로 이동해서 전반부 A.
      (0, Some('a'), "X") -> Set((1, List("A", "X")), (0, List("X", "X"))),

      (0, Some('b'), "Z") -> Set((2, List("B", "Z")), (0, List("X", "Z"))), //q2으로 이동해서 전반부 B.
      (0, Some('b'), "X") -> Set((2, List("B", "X")), (0, List("X", "X"))), // X는 자릿수 맞춰서 얘가 됐는지 아닌지 확인하는 용도.

      // 전반부에서 기준 A일때 문자열 스택에 다 넣기.
      (1, Some('a'), "A") -> Set((1, List("X", "A"))), 
      (1, Some('a'), "X") -> Set((1, List("X", "X"))), 
      (1, Some('b'), "A") -> Set((1, List("X", "A"))), 
      (1, Some('b'), "X") -> Set((1, List("X", "X"))), 

      (1, None, "X") -> Set((4, List("X"))),
      (1, None, "A") -> Set((4, List("A"))),

      // 전반부에서 기준 B일때 문자열 스택에 다 넣기.
      (2, Some('a'), "B") -> Set((2, List("X", "B"))), 
      (2, Some('a'), "X") -> Set((2, List("X", "X"))), 
      (2, Some('b'), "B") -> Set((2, List("X", "B"))), 
      (2, Some('b'), "X") -> Set((2, List("X", "X"))), 

      (2, None, "X") -> Set((5, List("X"))),
      (2, None, "B") -> Set((5, List("B"))),

      // 후반부에서 기준 A일때 문자 비교하기.
      (4, Some('a'), "X") -> Set((4, List("X")), (6, List("X"))), //문자 a를 무시하고 넘어가거나 
      // 상태 6으로 넘어가 얘가 같은 인덱스인지 확인.
      (4, Some('a'), "A") -> Set((4, List("A")), (6, List())),
      (4, Some('b'), "A") -> Set((4, List("A"))),

      //(6, Some('a'), "A") -> Set((6, List("A"))),
      (6, Some('a'), "X") -> Set((6, List())),
      (6, None, "Z") -> Set((3, List())),  // Accept

       // 후반부에서 기준 B일때 문자 비교하기.
      (5, Some('b'), "X") -> Set((5, List("X")), (7, List("X"))), //문자 b를 무시하고 넘어가거나 
      // 상태 7으로 넘어가 얘가 같은 인덱스인지 확인.
      (5, Some('b'), "B") -> Set((5, List("B")), (7, List("B"))),
      (5, Some('a'), "B") -> Set((5, List("B"))),

      (7, Some('b'), "B") -> Set((7, List("B"))),
      (7, Some('b'), "X") -> Set((7, List())),
      (7, None, "B") -> Set((3, List())),  // Accept

      (3, None, "A") -> Set((3, List())),
      (3, None, "B") -> Set((3, List())),
      (3, None, "X") -> Set((3, List())),
      (3, None, "Z") -> Set((3, List())),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(),
  )
}

/*def pda_eq_pair_empty: PDA = PDA( //
    states = Set(0, 1, 2, 3, 4, 5), 
    symbols = Set('a', 'b'),
    alphabets = Set("X", "Z"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("X", "Z"))),
      (0, Some('a'), "X") -> Set((0, List("X", "X"))),
      (0, Some('b'), "Z") -> Set((0, List("X", "Z"))),
      (0, Some('b'), "X") -> Set((0, List("X", "X"))),

      (0, Some('a'), "Z") -> Set((1, List("Z"))),
      (0, Some('a'), "X") -> Set((1, List("X"))),
      (0, Some('b'), "Z") -> Set((3, List("Z"))),
      (0, Some('b'), "X") -> Set((3, List("X"))),

      (1, None, "Z") -> Set((1, List())),
      (1, Some('a'), "X") -> Set((1, List())),
      (1, Some('b'), "X") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List("Z"))),

      (2, Some('a'), "Z") -> Set((2, List("X", "Z"))),
      (2, Some('b'), "Z") -> Set((2, List("X", "Z"))),
      (2, Some('a'), "X") -> Set((2, List("X", "X"))),
      (2, Some('b'), "X") -> Set((2, List("X", "X"))),
      (2, Some('b'), "Z") -> Set((5, List("Z"))),
      (2, Some('b'), "X") -> Set((5, List("X"))),

      (3, Some('a'), "X") -> Set((3, List())),
      (3, Some('b'), "X") -> Set((3, List())),
      (3, None, "Z") -> Set((3, List())),
      (3, None, "Z") -> Set((4, List("Z"))),

      (4, Some('a'), "Z") -> Set((4, List("X", "Z"))),
      (4, Some('b'), "Z") -> Set((4, List("X", "Z"))),
      (4, Some('a'), "X") -> Set((4, List("X", "X"))),
      (4, Some('b'), "X") -> Set((4, List("X", "X"))),
      (4, Some('a'), "Z") -> Set((5, List("Z"))),
      (4, Some('a'), "X") -> Set((5, List("X"))),

      (5, Some('a'), "X") -> Set((5, List())),
      (5, Some('b'), "X") -> Set((5, List())),
      (5, None, "Z") -> Set((5, List())),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(),
  )
}*/
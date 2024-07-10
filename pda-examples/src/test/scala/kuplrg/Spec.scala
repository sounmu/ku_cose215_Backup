package kuplrg

import Implementation.*

import scala.util.parsing.combinator.*

extension (ac: Acceptable) {
  /** Checks if the language is equal to the given language using a generator
    *
    * @param expected
    * @param trial
    * @param gen
    */
  def mustEqualWithGenerator(
    expected: Acceptable,
    trial: Int,
    gen: () => Word,
  ): Unit =
    mustEqualWithGenerator(ac.accept, expected, trial, gen)

  def mustEqualWithGenerator(
    accept: Word => Boolean,
    expected: Acceptable,
    trial: Int,
    gen: () => Word,
    debug: Boolean = false,
  ): Unit =
    var accept_cnt = 0
    var reject_cnt = 0

    for _ <- 0 until trial do
      val s = gen()
      val result = ac.accept(s)
      val answer = expected.accept(s)
      val neg = if answer then {
        accept_cnt += 1
        ""
      } else {
        reject_cnt += 1
        " not"
      }
      if result != answer then
        error(s"the word '$s' should$neg be in the language.")
    if debug then {
      println(s"Accept: $accept_cnt")
      println(s"Reject: $reject_cnt")
    }
  }

class Spec extends SpecBase with RegexParsers with PackratParsers {

  // Check if there is no epsilon transition increasing the stack in the PDA
  def checkValid(pda: PDA): PDA =
    val incEpsTrans = pda.incEpsTrans
    if (incEpsTrans.nonEmpty)
      val str = (for {
        ((q, a, x), (p, seq)) <- incEpsTrans.sortBy(_._1)
        aStr = a.fold("ε")(_.toString)
        str = s"\n         * $q -> $p - $aStr [$x -> ${seq.mkString(" ")}]"
      } yield str).mkString
      error("The PDA has epsilon transitions increasing the stack: " + str)
    pda

  // L = { w w^R | w \in {0, 1}* }
  {
    val lang: Lang = Lang(
      "01".toSet,
      w => w.length % 2 == 0 && w == w.reverse,
    )
    val even_final_check = () => {
      val converted_lang = checkValid(pda_even_pal_final).langByFinalStates
      converted_lang.mustEqual(lang, 10_000)
    }
    check(
      even_final_check(),
      weight = 10,
    )
  }

  // L = { w \in {a, b}* | N_a(w) > N_b(w) }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => w.count(_ == 'a') > w.count(_ == 'b'),
    )
    val more_as_check = () => {
      val converted_lang = checkValid(pda_more_as_empty).langByEmptyStacks
      converted_lang.mustEqual(lang, 10_000)
    }
    check(
      more_as_check(),
      weight = 10,
    )
  }

  // L = { a^i b^j c^k | i, j, k >= 0 and (i = j or i = k) }
  {
    val lang: Lang = Lang(
      "abc".toSet,
      w =>
        "a*b*c*".r.matches(w) && (
          w.count(_ == 'a') == w.count(_ == 'b') ||
          w.count(_ == 'a') == w.count(_ == 'c')
        ),
    )
    val abc_ij_ik_check = () => {
      val converted_lang = checkValid(pda_abc_ij_ik_final).langByFinalStates
      converted_lang.mustEqual(lang, 10_000)
      converted_lang.mustEqualWithGenerator(lang, 1_000, Generator.abc_ij_ik_accept(10))
      converted_lang.mustEqualWithGenerator(lang, 1_000, Generator.abc_ij_ik_random(10))
    }
    check(
      abc_ij_ik_check(),
      weight = 10,
    )
  }

  // L = { a^i b^j | (i = 2n + 1 and j = 3n + 1) for n >= 0 }
  {
    val lang: Lang = Lang(
      "ab".toSet,
      w => {
        val a = w.count(_ == 'a')
        val b = w.count(_ == 'b')
        "a*b*".r.matches(w) && a % 2 == 1 && b == (a - 1) / 2 * 3 + 1
      },
    )
    val a2n1_b3n1_check = () => {
      val converted_lang = checkValid(pda_a2n1_b3n1_empty).langByEmptyStacks
      converted_lang.mustEqual(lang, 1_000)
      converted_lang.mustEqualWithGenerator(lang, 10_000, Generator.a2n1_b3n1_accept(10))
      converted_lang.mustEqualWithGenerator(lang, 1_000, Generator.a2n1_b3n1_random1(10))
      converted_lang.mustEqualWithGenerator(lang, 1_000, Generator.a2n1_b3n1_random2(10))
    }
    check(
      a2n1_b3n1_check(),
      weight = 10,
    )
  }

  // L = { a^i b^j c^k | i, j, k >= 0 and j = i + 2k }
  {
    def lang: Lang = Lang(
      "abc".toSet,
      w => {
        val a = w.count(_ == 'a')
        val b = w.count(_ == 'b')
        val c = w.count(_ == 'c')
        "a*b*c*".r.matches(w) && b == a + 2 * c
      },
    )
    val abc_j_i2k_check = () => {
      val converted_lang = checkValid(pda_abc_j_i2k_final).langByFinalStates
      converted_lang.mustEqual(lang, 1_000)
      converted_lang.mustEqualWithGenerator(lang, 10_000, Generator.abc_j_i2k_accept(10))
      converted_lang.mustEqualWithGenerator(lang, 1_000, Generator.abc_j_i2k_random1(10))
      converted_lang.mustEqualWithGenerator(lang, 1_000, Generator.abc_j_i2k_random2(10))
    }
    check(
      abc_j_i2k_check(),
      weight = 15,
    )
  }

  // L = { w \in { '(', ')', '{', '}', '[', ']' }* | w is well-formed and
  // satisfies the order: '()' <= '{}' <= '[]' }
  {
    val lang: Lang = Lang(
      "(){}[]".toSet, {
        type P = PackratParser[Unit]
        def unit[T](p: Parser[T]): P = p ^^^ ()
        lazy val round: P = unit(("(" ~ round ~ ")" | "") ~ opt(round))
        lazy val curly: P = unit(("{" ~ curly ~ "}" | round) ~ opt(curly))
        lazy val square: P = unit(("[" ~ square ~ "]" | curly) ~ opt(square))
        w => parseAll(square, w).successful
      },
    )
    val ord_brace_check = () => {
      val converted_lang = checkValid(pda_ord_brace_empty).langByEmptyStacks
      converted_lang.mustEqual(lang, 50_000)
      converted_lang.mustEqualWithGenerator(lang, 5_000, Generator.ord_brace_accept(5))
      converted_lang.mustEqualWithGenerator(lang, 5_000, Generator.ord_brace_random(5))
    }
    check(
      ord_brace_check(),
      weight = 15,
    )
  }

  // L = { w \in { '0', '1', '+', '*' }* | w is an arithmetic expression
  // evaluated to an even number }
  {
    val lang = Lang(
      "01+*".toSet, {
        type P = PackratParser[Int]
        lazy val base: P = "0" ^^^ 0 | "1" ^^^ 1 | "(" ~> add <~ ")"
        lazy val mul: P = base ~ rep("*" ~> base) ^^ {
          case x ~ xs => xs.foldLeft(x)(_ * _)
        }
        lazy val add: P = mul ~ rep("+" ~> mul) ^^ {
          case x ~ xs => xs.foldLeft(x)(_ + _ % 2)
        }
        w => parseAll(add, w).map(_ % 2 == 0).getOrElse(false)
      },
    )
    val ae_even_check = () => {
      val converted_lang = checkValid(pda_ae_even_final).langByFinalStates
      converted_lang.mustEqual(lang, 20_000)
      converted_lang.mustEqualWithGenerator(lang, 5_000, Generator.ae_even_random(5))
    }
    check(
      ae_even_check(),
      weight = 15,
    )
  }

  // L = { a_1 a_2 ... a_2n \in {a, b}* | n >= 1 and a_i = a_{n+i} for some
  // 1 <= i <= n }
  {
    val lang = Lang(
      "ab".toSet,
      w => {
        w.length % 2 == 0 &&
        (w.take(w.length / 2) zip w.drop(w.length / 2)).exists(_ == _)
      },
    )
    val eq_pair_check = () => {
      val converted_lang = checkValid(pda_eq_pair_empty).langByEmptyStacks
      converted_lang.mustEqual(lang, 10_000)
      converted_lang.mustEqualWithGenerator(lang, 1_000, Generator.eq_pair_reject(10))
      converted_lang.mustEqualWithGenerator(lang, 1_000, Generator.eq_pair_random(10))
    }
    check(
      eq_pair_check(),
      weight = 15,
    )
  }

  /* Write your own tests */
}

package kuplrg

import Implementation.*
import cats.syntax.writer

extension (ac: Acceptable) {
  def mustComputablyEqualWithGenerator(
    expected: Acceptable,
    trial: Int,
    gen: () => Word,
  ): Unit =
    mustComputablyEqualWithGenerator(ac.accept, expected, trial, gen, false)

  def mustComputablyEqualWithGenerator(
    accept: Word => Boolean,
    expected: Acceptable,
    trial: Int,
    gen: () => Word,
    debug: Boolean,
  ): Unit =
    var accept_cnt = 0
    var reject_cnt = 0

    for _ <- 0 until trial do
      val w = gen()
      val result = ac.accept(w)
      val answer = expected.accept(w)
      val neg = if answer then {
        accept_cnt += 1
        ""
      } else {
        reject_cnt += 1
        " not"
      }
      if result != answer then
        error(s"the word '$w' should$neg be in the language.")

    if debug then
      println(s"Accept: $accept_cnt")
      println(s"Reject: $reject_cnt")
}

extension (cp: Acceptable & Computable) {
  def mustEqualWithGenerator(
    expected: Computable,
    trial: Int,
    gen: () => Word,
  ): Unit =
    mustEqualWithGenerator(cp.compute, expected, trial, gen, false)

  def mustEqualWithGenerator(
    compute: Word => Option[Word],
    expected: Computable,
    trial: Int,
    gen: () => Word,
    debug: Boolean = false,
  ): Unit =
    var accept_cnt = 0
    var reject_cnt = 0

    for _ <- 0 until trial do
      val w = gen()
      val result = compute(w)
      val answer = expected.compute(w)
      if (result != answer) answer match
        case Some(x) => error(s"the word '$w' should be computed to '$x'.")
        case None    => error(s"the word '$w' should not be computed.")

    if debug then
      println(s"Accept: $accept_cnt")
      println(s"Reject: $reject_cnt")
}


class Spec extends SpecBase {

  // L = { a^{n^2} | n ≥ 0 }
  {
    val lang = Lang(
      "a".toSet,
      w => math.sqrt(w.length).isWhole,
    )
    check(tm_square.mustValid.mustEqual(lang, 1_000))
  }

  // L = { a^n | n is a fibonacci number }
  {
    val lang = Lang(
      "a".toSet,
      w => {
        val n = w.length
        def isFib(prev: Int, cur: Int): Boolean =
          if (cur < n) isFib(cur, prev + cur)
          else cur == n
        n == 0 || n == 1 || isFib(1, 2)
      },
    )
    val tm_square_check = () => {
      tm_fib.mustValid.mustEqual(lang, 1_000)
    }
    check(tm_square_check())
  }

  // L = { w \in {a, b, c}* | N_a(w) = N_b(w) = N_c(w) }
  {
    val lang = Lang(
      "abc".toSet,
      w =>
        val a = w.count(_ == 'a')
        val b = w.count(_ == 'b')
        val c = w.count(_ == 'c')
        a == b && b == c,
    )
    val tm_eq_abc_check = () => {
      val tm_checked = tm_eq_abc.mustValid
      tm_checked.mustEqual(lang, 1_000)
      tm_checked.mustComputablyEqualWithGenerator(lang, 1_000, Generator.tm_eq_abc_accept(5))
    }
    check(tm_eq_abc_check())
  }

  // f(w ∈ {0, 1}*) = w' where w' = w - 1 if w starts with 1, otherwise f(w) is
  // not defined
  {
    val func: Computer = Computer(
      "01".toSet,
      w =>
        if (w.startsWith("1")) Some((BigInt(w, 2) - 1).toString(2))
        else None,
    )
    check(tm_dec.mustValid.mustEqual(func, 10_000))
  }

  // f(x+y) = z where x, y ∈ {0, 1}* start with 1 and z = x + y
  {
    val pattern = "([01]*)\\+([01]*)".r
    val func: Computer = Computer(
      "01+".toSet,
      _ match
        case pattern(x, y) if x.startsWith("1") && y.startsWith("1") =>
          Some((BigInt(x, 2) + BigInt(y, 2)).toString(2))
        case _ => None,
    )
    check(tm_add.mustValid.mustEqual(func, 20_000))
  }

  /* Write your own tests */
}

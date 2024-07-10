package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // Number of trials for `mustEqual`
  val TRIAL = 20_000

  // L = { a b^n | n >= 0 }
  val lang_a_b_star: Lang = (
    "ab".toSet,
    "ab*".r.matches
  )

  // tests for `dfa_a_star_b`
  check(dfa_a_b_star.mustValid)
  check(dfa_a_b_star.mustEqual(lang_a_b_star, TRIAL))

  // L = { w \in {0, 1}* | N(w) \equiv 1 (mod 3) }
  // where N(w) is the natural number represented by w in binary
  val lang_div_3_1: Lang = (
    "01".toSet,
    w => Integer.parseInt(w, 2) % 3 == 1
  )

  // tests for `dfa_div_3_1`
  check(dfa_div_3_1.mustValid)
  check(dfa_div_3_1.mustEqual(lang_div_3_1, TRIAL))

  // L = { w \in {0, 1}* | w contains a subsequence 101 }
  val lang_subseq_101: Lang = (
    "01".toSet,
    ".*1.*0.*1.*".r.matches
  )

  // tests for `dfa_subseq_101`
  check(dfa_subseq_101.mustValid)
  check(dfa_subseq_101.mustEqual(lang_subseq_101, TRIAL))

  // L = { w \in {0, 1}* | w contains odd number of 0's and odd number of 1's }
  val lang_odd_0_1: Lang = (
    "01".toSet,
    w => w.count(_ == '0') % 2 == 1 && w.count(_ == '1') % 2 == 1,
  )

  // tests for `dfa_odd_0_1`
  check(dfa_odd_0_1.mustValid)
  check(dfa_odd_0_1.mustEqual(lang_odd_0_1, TRIAL))


  // L = { w \in {0, 1}* | w contains exactly two 0's }
  val lang_two_0: Lang = (
    "01".toSet,
    _.count(_ == '0') == 2
  )

  // tests for `nfa_two_0`
  check(nfa_two_0.mustValid)
  check(nfa_two_0.mustEqual(lang_two_0, TRIAL))

  // L = { (ab)^n | n >= 0 }
  val lang_substr_011: Lang = (
    "01".toSet,
    _.contains("011")
  )

  // tests for `nfa_substr_011`
  check(nfa_substr_011.mustValid)
  check(nfa_substr_011.mustEqual(lang_substr_011, TRIAL))

  // L = { w \in {0, 1}* | w contains "00" or "11" }
  val lang_has_00_or_11: Lang = (
    "01".toSet,
    w => w.contains("00") || w.contains("11"),
  )

  // tests for `nfa_has_00_or_11`
  check(nfa_has_00_or_11.mustValid)
  check(nfa_has_00_or_11.mustEqual(lang_has_00_or_11, TRIAL))

  // L = { (ab)^n | n >= 0 }
  val lang_ab_plus: Lang = (
    "ab".toSet,
    "(ab)+".r.matches
  )

  // tests for `enfa_ab_plus`
  check(enfa_ab_plus.mustValid)
  check(enfa_ab_plus.mustEqual(lang_ab_plus, TRIAL))

  // L = { 0^n | n >= 0 } U { 1^n | n >= 0 }
  val lang_same_digits: Lang = (
    "01".toSet,
    w => w.forall(_ == '0') || w.forall(_ == '1')
  )

  // tests for `enfa_same_digits`
  check(enfa_same_digits.mustValid)
  check(enfa_same_digits.mustEqual(lang_same_digits, TRIAL))

  // L = { (a (b c^i b)^j a)^k | i, j, k >= 1 }
  val lang_complex: Lang = (
    "abc".toSet,
    "(a(bc+b)+a)+".r.matches
  )

  // tests for `enfa_same_digits`
  check(enfa_complex.mustValid)
  check(enfa_complex.mustEqual(lang_complex, TRIAL))

  /* Write your own tests */
}

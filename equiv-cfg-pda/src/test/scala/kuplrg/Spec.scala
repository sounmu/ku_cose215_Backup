package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // Number of trials for `mustEqual`
  private val TRIAL = 1_000

  // ---------------------------------------------------------------------------
  // Tests for `pdafs2es`
  // ---------------------------------------------------------------------------
  check(
    pdafs2es(pda1).langByEmptyStacks.mustEqual(pda1.langByFinalStates, TRIAL),
  )
  check(
    pdafs2es(pda2).langByEmptyStacks.mustEqual(pda2.langByFinalStates, TRIAL),
  )
  check(
    pdafs2es(pda3).langByEmptyStacks.mustEqual(pda3.langByFinalStates, TRIAL),
  )
  check(
    pdafs2es(pda4).langByEmptyStacks.mustEqual(pda4.langByFinalStates, TRIAL),
  )
  check(
    pdafs2es(pda5).langByEmptyStacks.mustEqual(pda5.langByFinalStates, TRIAL),
  )
  check(
    pdafs2es(pda6).langByEmptyStacks.mustEqual(pda6.langByFinalStates, TRIAL),
  )
  check(
    pdafs2es(pda7).langByEmptyStacks.mustEqual(pda7.langByFinalStates, TRIAL),
  )
  check(
    pdafs2es(pda8).langByEmptyStacks.mustEqual(pda8.langByFinalStates, TRIAL),
  )
  check(
    pdafs2es(pda9).langByEmptyStacks.mustEqual(pda9.langByFinalStates, TRIAL),
  )
  check(
    pdafs2es(pda10).langByEmptyStacks.mustEqual(pda10.langByFinalStates, TRIAL),
  )

  // ---------------------------------------------------------------------------
  // Tests for `pdaes2fs`
  // ---------------------------------------------------------------------------
  check(
    pdaes2fs(pda11).langByFinalStates.mustEqual(pda11.langByEmptyStacks, TRIAL),
  )
  check(
    pdaes2fs(pda12).langByFinalStates.mustEqual(pda12.langByEmptyStacks, TRIAL),
  )
  check(
    pdaes2fs(pda13).langByFinalStates.mustEqual(pda13.langByEmptyStacks, TRIAL),
  )
  check(
    pdaes2fs(pda14).langByFinalStates.mustEqual(pda14.langByEmptyStacks, TRIAL),
  )
  check(
    pdaes2fs(pda15).langByFinalStates.mustEqual(pda15.langByEmptyStacks, TRIAL),
  )
  check(
    pdaes2fs(pda16).langByFinalStates.mustEqual(pda16.langByEmptyStacks, TRIAL),
  )
  check(
    pdaes2fs(pda17).langByFinalStates.mustEqual(pda17.langByEmptyStacks, TRIAL),
  )
  check(
    pdaes2fs(pda18).langByFinalStates.mustEqual(pda18.langByEmptyStacks, TRIAL),
  )
  check(
    pdaes2fs(pda19).langByFinalStates.mustEqual(pda19.langByEmptyStacks, TRIAL),
  )
  check(
    pdaes2fs(pda20).langByFinalStates.mustEqual(pda20.langByEmptyStacks, TRIAL),
  )

  // ---------------------------------------------------------------------------
  // Tests for `cfg2pdaes`
  // ---------------------------------------------------------------------------
  val cfg1 = CFG("S -> 'a' S | <e> ;")
  check(cfg2pdaes(cfg1).langByEmptyStacks.mustEqual(lang_an, TRIAL))
  val cfg2 = CFG("S -> 'a' S 'b' | <e> ;")
  check(cfg2pdaes(cfg2).langByEmptyStacks.mustEqual(lang_an_bn, TRIAL))
  val cfg3 = CFG("S -> 'a' S 'b' 'b' | <e> ;")
  check(cfg2pdaes(cfg3).langByEmptyStacks.mustEqual(lang_an_b2n, TRIAL))
  val cfg4 = CFG("S -> 'a' S 'a' | 'b' S 'b' | <e> ;")
  check(cfg2pdaes(cfg4).langByEmptyStacks.mustEqual(lang_w_wR, TRIAL))
  val cfg5 = CFG("S -> 'a' S 'b' S | 'b' S 'a' S | <e> ;")
  check(cfg2pdaes(cfg5).langByEmptyStacks.mustEqual(lang_na_eq_nb, TRIAL))
  val cfg6 = CFG(
    "S -> <e> | 'a' S 'b' S | 'c' S 'b' S | 'b' S 'a' S | 'b' S 'c' S ;",
  )
  check(cfg2pdaes(cfg6).langByEmptyStacks.mustEqual(lang_a_eq_b_c, TRIAL))
  val cfg7 = CFG("S -> 'a' 'a' S 'b' 'b' | 'a' 'b' ;")
  check(cfg2pdaes(cfg7).langByEmptyStacks.mustEqual(lang_a2n1_b2n1, TRIAL))
  val cfg8 = CFG("S -> 'a' 'a' S 'b' 'b' | <e> ;")
  check(cfg2pdaes(cfg8).langByEmptyStacks.mustEqual(lang_a2n_b2n, TRIAL))
  val cfg9 = CFG("S -> 'b' S 'b' 'b' | A ; A -> 'a' A | <e> ;")
  check(cfg2pdaes(cfg9).langByEmptyStacks.mustEqual(lang_bn_am_b2n, TRIAL))
  val cfg10 = CFG("S -> A S | <e> ; A -> '(' S ')' | '{' S '}' | <e> ;")
  check(cfg2pdaes(cfg10).langByEmptyStacks.mustEqual(lang_balanced, TRIAL))

  // ---------------------------------------------------------------------------
  // Tests for `pades2cfg`
  // ---------------------------------------------------------------------------
  check(pdaes2cfg(pda11).lang.mustEqual(lang_an_bn, TRIAL))
  check(pdaes2cfg(pda12).lang.mustEqual(lang_an_b2n, TRIAL))
  check(pdaes2cfg(pda13).lang.mustEqual(lang_w_wR, TRIAL))
  check(pdaes2cfg(pda14).lang.mustEqual(lang_na_eq_nb, TRIAL))
  check(pdaes2cfg(pda15).lang.mustEqual(lang_na_neq_nb, TRIAL))
  check(pdaes2cfg(pda16).lang.mustEqual(lang_not_w_w, TRIAL))
  check(pdaes2cfg(pda17).lang.mustEqual(lang_a2n1_b2n1, TRIAL))
  check(pdaes2cfg(pda18).lang.mustEqual(lang_a2n_b2n, TRIAL))
  check(pdaes2cfg(pda19).lang.mustEqual(lang_bn_am_b2n, TRIAL))
  check(pdaes2cfg(pda20).lang.mustEqual(lang_balanced, TRIAL))

  // ---------------------------------------------------------------------------
  // Test Data
  // ---------------------------------------------------------------------------
  // pre-defined alphabets
  val X = "X"
  val Y = "Y"
  val Z = "Z"

  // PDA for lang_an_bn by final states
  val pda1: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(2),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('b'), X) -> (1, List()),
    (1, None, Z) -> (2, List(Z)),
  )

  // PDA for lang_an_b2n by final states
  val pda2: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(2),
    (0, Some('a'), Z) -> (0, List(X, X, Z)),
    (0, Some('a'), X) -> (0, List(X, X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('b'), X) -> (1, List()),
    (1, None, Z) -> (2, List(Z)),
  )

  // PDA for lang_w_wR by final states
  val pda3: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(2),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, Some('a'), Y) -> (0, List(X, Y)),
    (0, Some('b'), Z) -> (0, List(Y, Z)),
    (0, Some('b'), X) -> (0, List(Y, X)),
    (0, Some('b'), Y) -> (0, List(Y, Y)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (0, None, Y) -> (1, List(Y)),
    (1, Some('a'), X) -> (1, List()),
    (1, Some('b'), Y) -> (1, List()),
    (1, None, Z) -> (2, List(Z)),
  )

  // PDA for lang_na_eq_nb by final states
  val pda4: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(1),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, Some('a'), Y) -> (0, List()),
    (0, Some('b'), Z) -> (0, List(Y, Z)),
    (0, Some('b'), X) -> (0, List()),
    (0, Some('b'), Y) -> (0, List(Y, Y)),
    (0, None, Z) -> (1, List(Z)),
  )

  // PDA for lang_na_neq_nb by final states
  val pda5: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(1),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, Some('a'), Y) -> (0, List()),
    (0, Some('b'), Z) -> (0, List(Y, Z)),
    (0, Some('b'), X) -> (0, List()),
    (0, Some('b'), Y) -> (0, List(Y, Y)),
    (0, None, X) -> (1, List()),
    (0, None, Y) -> (1, List()),
  )

  // PDA for lang_not_w_w by final states
  val pda6: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(6),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, Some('b'), Z) -> (0, List(X, Z)),
    (0, Some('b'), X) -> (0, List(X, X)),
    (0, Some('a'), Z) -> (1, List(Z)),
    (0, Some('a'), X) -> (1, List(X)),
    (0, Some('b'), Z) -> (3, List(Z)),
    (0, Some('b'), X) -> (3, List(X)),
    (1, Some('a'), X) -> (1, List()),
    (1, Some('b'), X) -> (1, List()),
    (1, None, Z) -> (6, List()),
    (1, None, Z) -> (2, List(Z)),
    (3, Some('a'), X) -> (3, List()),
    (3, Some('b'), X) -> (3, List()),
    (3, None, Z) -> (6, List()),
    (3, None, Z) -> (4, List(Z)),
    (2, Some('a'), Z) -> (2, List(X, Z)),
    (2, Some('a'), X) -> (2, List(X, X)),
    (2, Some('b'), Z) -> (2, List(X, Z)),
    (2, Some('b'), X) -> (2, List(X, X)),
    (4, Some('a'), Z) -> (4, List(X, Z)),
    (4, Some('a'), X) -> (4, List(X, X)),
    (4, Some('b'), Z) -> (4, List(X, Z)),
    (4, Some('b'), X) -> (4, List(X, X)),
    (2, Some('b'), Z) -> (5, List(Z)),
    (2, Some('b'), X) -> (5, List(X)),
    (4, Some('a'), Z) -> (5, List(Z)),
    (4, Some('a'), X) -> (5, List(X)),
    (5, Some('a'), X) -> (5, List()),
    (5, Some('b'), X) -> (5, List()),
    (5, None, Z) -> (6, List(Z)),
  )

  // PDA for lang_a2n1_b2n1 by final states
  val pda7: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(3),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('b'), X) -> (2, List()),
    (2, Some('b'), X) -> (1, List()),
    (2, None, Z) -> (3, List(Z)),
  )

  // PDA for lang_a2n_b2n by final states
  val pda8: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(3),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('b'), X) -> (2, List()),
    (1, None, Z) -> (3, List(Z)),
    (2, Some('b'), X) -> (1, List()),
  )

  // PDA for lang_bn_am_b2n by final states
  val pda9: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(3),
    (0, Some('b'), Z) -> (0, List(X, X, Z)),
    (0, Some('b'), X) -> (0, List(X, X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('a'), Z) -> (1, List(Z)),
    (1, Some('a'), X) -> (1, List(X)),
    (1, None, Z) -> (2, List(Z)),
    (1, None, X) -> (2, List(X)),
    (2, Some('b'), X) -> (2, List()),
    (2, None, Z) -> (3, List(Z)),
  )

  // PDA for lang_balanced by final states
  val pda10: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(1),
    (0, Some('('), Z) -> (0, List(X, Z)),
    (0, Some('('), X) -> (0, List(X, X)),
    (0, Some('('), Y) -> (0, List(X, Y)),
    (0, Some('{'), Z) -> (0, List(Y, Z)),
    (0, Some('{'), X) -> (0, List(Y, X)),
    (0, Some('{'), Y) -> (0, List(Y, Y)),
    (0, Some(')'), X) -> (0, List()),
    (0, Some('}'), Y) -> (0, List()),
    (0, None, Z) -> (1, List(Z)),
  )

  // PDA for lang_an_bn by empty stacks
  val pda11: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('b'), X) -> (1, List()),
    (1, None, Z) -> (1, List()),
  )

  // PDA for lang_an_b2n by empty stacks
  val pda12: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(),
    (0, Some('a'), Z) -> (0, List(X, X, Z)),
    (0, Some('a'), X) -> (0, List(X, X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('b'), X) -> (1, List()),
    (1, None, Z) -> (1, List()),
  )

  // PDA for lang_w_wR by empty stacks
  val pda13: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, Some('a'), Y) -> (0, List(X, Y)),
    (0, Some('b'), Z) -> (0, List(Y, Z)),
    (0, Some('b'), X) -> (0, List(Y, X)),
    (0, Some('b'), Y) -> (0, List(Y, Y)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (0, None, Y) -> (1, List(Y)),
    (1, Some('a'), X) -> (1, List()),
    (1, Some('b'), Y) -> (1, List()),
    (1, None, Z) -> (1, List()),
  )

  // PDA for lang_na_eq_nb by empty stacks
  val pda14: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, Some('a'), Y) -> (0, List()),
    (0, Some('b'), Z) -> (0, List(Y, Z)),
    (0, Some('b'), X) -> (0, List()),
    (0, Some('b'), Y) -> (0, List(Y, Y)),
    (0, None, Z) -> (0, List()),
  )

  // PDA for lang_na_neq_nb by empty stacks
  val pda15: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, Some('a'), Y) -> (0, List()),
    (0, Some('b'), Z) -> (0, List(Y, Z)),
    (0, Some('b'), X) -> (0, List()),
    (0, Some('b'), Y) -> (0, List(Y, Y)),
    (0, None, X) -> (1, List()),
    (0, None, Y) -> (1, List()),
    (1, None, X) -> (1, List()),
    (1, None, Y) -> (1, List()),
    (1, None, Z) -> (1, List()),
  )

  // PDA for lang_not_w_w by empty stacks
  val pda16: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, Some('b'), Z) -> (0, List(X, Z)),
    (0, Some('b'), X) -> (0, List(X, X)),
    (0, Some('a'), Z) -> (1, List(Z)),
    (0, Some('a'), X) -> (1, List(X)),
    (0, Some('b'), Z) -> (3, List(Z)),
    (0, Some('b'), X) -> (3, List(X)),
    (1, Some('a'), X) -> (1, List()),
    (1, Some('b'), X) -> (1, List()),
    (1, None, Z) -> (1, List()),
    (1, None, Z) -> (2, List(Z)),
    (3, Some('a'), X) -> (3, List()),
    (3, Some('b'), X) -> (3, List()),
    (3, None, Z) -> (1, List()),
    (3, None, Z) -> (4, List(Z)),
    (2, Some('a'), Z) -> (2, List(X, Z)),
    (2, Some('a'), X) -> (2, List(X, X)),
    (2, Some('b'), Z) -> (2, List(X, Z)),
    (2, Some('b'), X) -> (2, List(X, X)),
    (4, Some('a'), Z) -> (4, List(X, Z)),
    (4, Some('a'), X) -> (4, List(X, X)),
    (4, Some('b'), Z) -> (4, List(X, Z)),
    (4, Some('b'), X) -> (4, List(X, X)),
    (2, Some('b'), Z) -> (5, List(Z)),
    (2, Some('b'), X) -> (5, List(X)),
    (4, Some('a'), Z) -> (5, List(Z)),
    (4, Some('a'), X) -> (5, List(X)),
    (5, Some('a'), X) -> (5, List()),
    (5, Some('b'), X) -> (5, List()),
    (5, None, Z) -> (1, List()),
  )

  // PDA for lang_a2n1_b2n1 by empty stacks
  val pda17: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('b'), X) -> (2, List()),
    (2, Some('b'), X) -> (1, List()),
    (2, None, Z) -> (2, List()),
  )

  // PDA for lang_a2n_b2n by empty stacks
  val pda18: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('b'), X) -> (2, List()),
    (1, None, Z) -> (1, List()),
    (2, Some('b'), X) -> (1, List()),
  )

  // PDA for lang_bn_am_b2n by empty stacks
  val pda19: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(),
    (0, Some('b'), Z) -> (0, List(X, X, Z)),
    (0, Some('b'), X) -> (0, List(X, X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('a'), Z) -> (1, List(Z)),
    (1, Some('a'), X) -> (1, List(X)),
    (1, None, Z) -> (2, List(Z)),
    (1, None, X) -> (2, List(X)),
    (2, Some('b'), X) -> (2, List()),
    (2, None, Z) -> (2, List()),
  )

  // PDA for lang_balanced by empty stacks
  val pda20: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(),
    (0, Some('('), Z) -> (0, List(X, Z)),
    (0, Some('('), X) -> (0, List(X, X)),
    (0, Some('('), Y) -> (0, List(X, Y)),
    (0, Some('{'), Z) -> (0, List(Y, Z)),
    (0, Some('{'), X) -> (0, List(Y, X)),
    (0, Some('{'), Y) -> (0, List(Y, Y)),
    (0, Some(')'), X) -> (0, List()),
    (0, Some('}'), Y) -> (0, List()),
    (0, None, Z) -> (0, List()),
  )

  // A language L = { a^n | n >= 0 }
  val lang_an: Lang = Lang(
    "a".toSet,
    w => w.forall(_ == 'a'),
  )

  // A language L = { a^n b^n | n >= 0 }
  val lang_an_bn: Lang = Lang(
    "ab".toSet,
    w =>
      w.length % 2 == 0 &&
      w.substring(0, w.length / 2).forall(_ == 'a') &&
      w.substring(w.length / 2).forall(_ == 'b'),
  )

  // A language L = { a^n b^{2n} | n >= 0 }
  val lang_an_b2n: Lang = Lang(
    "ab".toSet,
    w =>
      w.length % 3 == 0 &&
      w.substring(0, w.length / 3).forall(_ == 'a') &&
      w.substring(w.length / 3).forall(_ == 'b'),
  )

  // A language L = { w w^R | w in {a, b}* }
  val lang_w_wR: Lang = Lang(
    "ab".toSet,
    w =>
      w.length % 2 == 0 &&
      (0 until (w.length / 2)).forall(i => w(i) == w(w.length - i - 1)),
  )

  // A language L = { w | N_a(w) = N_b(w) } where N_a(w) and N_b(w) are the
  // number of a's and b's in w
  val lang_na_eq_nb: Lang = Lang(
    "ab".toSet,
    w => w.count(_ == 'a') == w.count(_ == 'b'),
  )

  // A language L = { w | N_a(w) != N_b(w) } where N_a(w) and N_b(w) are the
  // number of a's and b's in w
  val lang_na_neq_nb: Lang = Lang(
    "ab".toSet,
    w => w.count(_ == 'a') != w.count(_ == 'b'),
  )

  // A language L = { w | N_a(w) = N_b(w) - N_c(w) } where N_a(w), N_b(w) and
  // N_c(w) are the number of a's, b's and c's in w
  val lang_a_eq_b_c: Lang = Lang(
    "abc".toSet,
    w => w.count(_ == 'a') == w.count(_ == 'b') - w.count(_ == 'c'),
  )

  // A language L = { x | no w in {a, b}* s.t. x = w w }
  val lang_not_w_w: Lang = Lang(
    "ab".toSet,
    w =>
      !(
        w.length % 2 == 0 &&
        w.substring(0, w.length / 2) == w.substring(w.length / 2)
      ),
  )

  // A language L = { a^{2n+1} b^{2n+1} | n >= 0 }
  val lang_a2n1_b2n1: Lang = Lang(
    "ab".toSet,
    w =>
      w.length % 4 == 2 &&
      w.substring(0, w.length / 2).forall(_ == 'a') &&
      w.substring(w.length / 2).forall(_ == 'b'),
  )

  // A language L = { a^{2n} b^{2n} | n >= 0 }
  val lang_a2n_b2n: Lang = Lang(
    "ab".toSet,
    w =>
      w.length % 4 == 0 &&
      w.substring(0, w.length / 2).forall(_ == 'a') &&
      w.substring(w.length / 2).forall(_ == 'b'),
  )

  // A language L = { b^n a^m b^{2n} | n, m >= 0 }
  val lang_bn_am_b2n: Lang = Lang(
    "ab".toSet,
    w =>
      val len = w.length
      if (w contains 'a') {
        val s = w.indexOf('a')
        val e = w.lastIndexOf('a') + 1
        s * 2 == (len - e) && w.substring(s, e).forall(_ == 'a')
      } else len % 3 == 0,
  )

  // A language L = { w | w is a balanced parentheses with `(`, `)`, `{`, `}` }
  val lang_balanced: Lang = Lang(
    "(){}".toSet,
    w =>
      w.foldLeft((true, List[Char]())) {
        case ((true, stack), a) if a == '(' || a == '{' => (true, a :: stack)
        case ((true, '(' :: stack), ')')                => (true, stack)
        case ((true, '{' :: stack), '}')                => (true, stack)
        case _                                          => (false, Nil)
      } == (true, Nil),
  )

  /* Write your own checks */
}

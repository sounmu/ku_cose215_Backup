package kuplrg

import Implementation.*

class Spec extends SpecBase {
  // tests for `sqsum`
  test(sqsum(0, 0), 0)
  test(sqsum(2, 3), 13)
  test(sqsum(-3, 4), 25)

  // hidden tests for `sqsum`
  test(sqsum(5, 12), 169)
  test(sqsum(-5, -12), 169)
  test(sqsum(0, 10), 100)
  test(sqsum(10, 0), 100)
  test(sqsum(-7, 7), 98)
  test(sqsum(20, 21), 841)
  test(sqsum(-15, 15), 450)

  // tests for `concat`
  test(concat("Hello ", "World!"), "Hello World!")
  test(concat("COSE", "212"), "COSE212")
  test(concat("COSE", "215"), "COSE215")

  // hidden tests for `concat`
  test(concat("PL", "RG"), "PLRG")
  test(concat("Scala", ""), "Scala")
  test(concat("", "Test"), "Test")
  test(concat("123", "456"), "123456")
  test(concat("Jihyeok ", "Park"), "Jihyeok Park")
  test(concat("Computation", "Theory"), "ComputationTheory")
  test(concat("    ", "    "), "        ")

  // tests for `subN`
  test(subN(3)(5), 2)
  test(subN(4)(13), 9)
  test(subN(243)(-942), -1185)

  // hidden tests for `subN`
  test(subN(10)(3), -7)
  test(subN(0)(0), 0)
  test(subN(-5)(10), 15)
  test(subN(20)(-40), -60)
  test(subN(50)(50), 0)
  test(subN(7)(-14), -21)
  test(subN(100)(1), -99)

  // tests for `twice`
  test(twice(_ + 3)(1), 7)
  test(twice(subN(3))(10), 4)
  test(twice(_ * 10)(42), 4200)

  // hidden tests for `twice`
  test(twice(_ - 2)(10), 6)
  test(twice(_ / 2)(20), 5)
  test(twice(_ * 3)(1), 9)
  test(twice(_ + 100)(-50), 150)
  test(twice(_ - 5)(20), 10)
  test(twice(x => x * x)(2), 16)
  test(twice(x => x / 3)(27), 3)

  // tests for `compose`
  test(compose(_ + 3, _ * 2)(1), 5)
  test(compose(_ * 10, _ + 1)(42), 430)
  test(compose(subN(3), subN(2))(10), 5)

  // hidden tests for `compose`
  test(compose(_ - 1, _ * 3)(10), 29)
  test(compose(_ / 2, _ + 10)(20), 15)
  test(compose(_ + 5, _ - 3)(2), 4)
  test(compose(_ * 2, _ / 4)(16), 8)
  test(compose(_ - 10, _ * 10)(3), 20)
  test(compose(x => x * x, _ - 2)(3), 1)
  test(compose(_ + 3, x => x * x)(4), 19)

  // tests for `sumOnlyOdd`
  test(sumOnlyOdd(List(2)), 0)
  test(sumOnlyOdd(List(1, 2, 3)), 4)
  test(sumOnlyOdd(List(4, 2, 3, 7, 5)), 15)

  // hidden tests for `sumOnlyOdd`
  test(sumOnlyOdd(List(1, 3, 5, 7, 9)), 25)
  test(sumOnlyOdd(List(0, 2, 4, 6, 8)), 0)
  test(sumOnlyOdd(List(11, 22, 33, 44, 55)), 99)
  test(sumOnlyOdd(List()), 0)
  test(sumOnlyOdd(List(101, 102, 103)), 204)
  test(sumOnlyOdd(List(1, 2, 3, 4, 5, 6, 7, 8, 9)), 25)
  test(sumOnlyOdd(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), 25)

  // tests for `foldWith`
  test(foldWith(_ + _)(List(1, 2, 3)), 6)
  test(foldWith(_ - _)(List(5, 9, 2, 3)), -19)
  test(foldWith(_ * 2 + _)(List(4, 7, 3, 2)), 68)

  // hidden tests for `foldWith`
  test(foldWith(_ + _)(List(10, 20, 30)), 60)
  test(foldWith(_ * _)(List(1, 2, 3, 4)), 0)
  test(foldWith(_ - _)(List(100, 10, 5)), -115)
  test(foldWith(_ / _)(List(120, 2, 3)), 0)
  test(foldWith(_ max _)(List(1, 3, 5, 7, 2)), 7)
  test(foldWith(_ min _)(List(10, 5, 15, 5)), 0)
  test(foldWith((x, y) => x + y * 2)(List(1, 2, 3, 4)), 20)

  // tests for `toSet`
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 0), Set(1, 2, 4, 5, 7))
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 2), Set(2, 4, 7))
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 4), Set(2, 4))

  // hidden tests for `toSet`
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 5), Set(2, 4))
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 1), Set(5, 2, 7, 4, 2, 4))
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 3), Set(7, 4, 2, 4))
  test(toSet(List(1), 0), Set(1))
  test(toSet(List(1, 2, 3, 4, 5, 6, 7, 8, 9), 8), Set(9))
  test(
    toSet(List(1, 2, 3, 4, 5, 6, 7, 8, 9), 0),
    Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
  )
  test(toSet(List(1, 2, 3, 4, 5, 6, 7, 8, 9), 4), Set(5, 6, 7, 8, 9))

  // tests for `getOrZero`
  val m: Map[String, Int] = Map("Park" -> 3, "Kim" -> 5)
  test(getOrZero(m, "Park"), 3)
  test(getOrZero(m, "Lee"), 0)
  test(getOrZero(m, "Kim"), 5)

  // hidden tests for `getOrZero`
  test(getOrZero(m, "Choi"), 0)
  test(getOrZero(m, ""), 0)
  test(getOrZero(Map("Zero" -> 0), "Zero"), 0)
  test(getOrZero(Map("Negative" -> -5), "Negative"), -5)
  test(getOrZero(Map(), "Absent"), 0)
  test(getOrZero(Map("1" -> 1, "2" -> 2, "3" -> 3), "2"), 2)
  test(getOrZero(Map("First" -> 100, "Last" -> 200), "Last"), 200)

  // tests for `setMinus`
  test(setMinus(Set(1, 2, 3), Set(2, 3, 4)), Set(1))
  test(setMinus(Set(1, 2, 3), Set(4, 5, 6)), Set(1, 2, 3))
  test(setMinus(Set(1, 2, 3), Set(1, 2, 3, 4)), Set())

  // hidden tests for `setMinus`

  test(setMinus(Set(10, 20, 30), Set(20)), Set(10, 30))
  test(setMinus(Set(), Set(1, 2, 3)), Set())
  test(setMinus(Set(1, 2, 3), Set()), Set(1, 2, 3))
  test(setMinus(Set(1, 2, 3), Set(1, 2, 3)), Set())
  test(setMinus(Set(1, 3, 5, 7, 9), Set(2, 4, 6, 8)), Set(1, 3, 5, 7, 9))
  test(setMinus(Set(-1, -2, -3), Set(-3, -4, -5)), Set(-1, -2))
  test(setMinus(Set(1, 2, 3, 4, 5), Set(1, 2, 3, 4, 5)), Set())

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  //  8
  val tree1: Tree = Leaf(8)

  //    4
  //   / \
  //  5   2
  //     / \
  //    8   3
  val tree2: Tree = Branch(Leaf(5), 4, Branch(Leaf(8), 2, Leaf(3)))

  //    7
  //   / \
  //  2   3
  //     / \
  //    5   1
  //   / \
  //  1   8
  val tree3: Tree =
    Branch(Leaf(2), 7, Branch(Branch(Leaf(1), 5, Leaf(8)), 3, Leaf(1)))

  // hideen trees

  //          10
  //         /  \
  //        5    15
  //       / \  /  \
  //      2  7 12  20
  //     / \
  //    1   3
  val tree4: Tree = Branch(
    Branch(
      Branch(Leaf(1), 2, Leaf(3)),
      5,
      Leaf(7)
    ),
    10,
    Branch(
      Leaf(12),
      15,
      Leaf(20)
    )
  )

  //    2
  //   / \
  //  1   4
  //     / \
  //    3   6
  //       / \
  //      5   8
  //         / \
  //        7   9

  val tree5: Tree = Branch(
    Leaf(1),
    2,
    Branch(
      Leaf(3),
      4,
      Branch(
        Leaf(5),
        6,
        Branch(
          Leaf(7),
          8,
          Leaf(9)
        )
      )
    )
  )

  // tests for `has`
  test(has(8)(tree1), true)
  test(has(7)(tree2), false)
  test(has(1)(tree3), true)

  // hidden tests for `has`
  test(has(10)(tree4), true)
  test(has(20)(tree4), true)
  test(has(6)(tree4), false)
  test(has(1)(tree5), true)
  test(has(9)(tree5), true)
  test(has(2)(tree5), true)
  test(has(11)(tree4), false)

  // tests for `maxDepthOf`
  test(maxDepthOf(8)(tree1), Some(0))
  test(maxDepthOf(7)(tree2), None)
  test(maxDepthOf(1)(tree3), Some(3))

  // hidden tests for `maxDepthOf`
  test(maxDepthOf(10)(tree4), Some(0))
  test(maxDepthOf(20)(tree4), Some(2))
  test(maxDepthOf(6)(tree4), None)
  test(maxDepthOf(11)(tree4), None)
  test(maxDepthOf(1)(tree5), Some(1))
  test(maxDepthOf(9)(tree5), Some(4))
  test(maxDepthOf(10)(tree5), None)

  // tests for `mul`
  test(mul(tree1), 8, weight = 2)
  test(mul(tree2), 960, weight = 2)
  test(mul(tree3), 1680, weight = 2)

  // hidden tests for `mul`
  test(mul(tree4), 7560000, weight = 2)
  test(mul(tree5), 362880, weight = 2)

  // tests for `countLeaves`
  test(countLeaves(tree1), 1, weight = 2)
  test(countLeaves(tree2), 3, weight = 2)
  test(countLeaves(tree3), 4, weight = 2)

  // hidden tests for `countLeaves`
  test(countLeaves(tree4), 5, weight = 2)
  test(countLeaves(tree5), 5, weight = 2)

  // tests for `postOrder`
  test(postOrder(tree1), List(8), weight = 2)
  test(postOrder(tree2), List(5, 8, 3, 2, 4), weight = 2)
  test(postOrder(tree3), List(2, 1, 8, 5, 1, 3, 7), weight = 2)

  // hidden tests for `postOrder`
  test(postOrder(tree4), List(1, 3, 2, 7, 5, 12, 20, 15, 10), weight = 2)
  test(postOrder(tree5), List(1, 3, 5, 7, 9, 8, 6, 4, 2), weight = 2)

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  // (true | false)
  val be1: BE = Or(True, False)

  // (!(true | false) & !(false | true))
  val be2: BE = And(Not(Or(True, False)), Not(Or(False, True)))

  // (!((false | !true) & false) & (true & !false))
  val be3: BE =
    And(Not(And(Or(False, Not(True)), False)), And(True, Not(False)))

  // hidden boolean expressions

  // ((((true | false) & !true) | false) & (!(true & false) | ((true | false) & !false)))
  val be4: BE = And(
    Or(And(Or(True, False), Not(True)), False),
    Or(Not(And(True, False)), And(Or(True, False), Not(False)))
  )

  // (((true | false) & !true) | ((true | false) & !false))
  val be5: BE =
    Or(And(Or(True, False), Not(True)), And(Or(True, False), Not(False)))

  // tests for `countLiterals`
  test(countLiterals(be1), 2, weight = 2)
  test(countLiterals(be2), 4, weight = 2)
  test(countLiterals(be3), 5, weight = 2)

  // hidden tests for `countLiterals`
  test(countLiterals(be4), 9, weight = 2)
  test(countLiterals(be5), 6, weight = 2)

  // tests for `countNots`
  test(countNots(be1), 0, weight = 2)
  test(countNots(be2), 2, weight = 2)
  test(countNots(be3), 3, weight = 2)

  // hidden tests for `countNots`
  test(countNots(be4), 3, weight = 2)
  test(countNots(be5), 2, weight = 2)

  // tests for `depth`
  test(depth(be1), 1, weight = 2)
  test(depth(be2), 3, weight = 2)
  test(depth(be3), 5, weight = 2)

  // hidden tests for `depth`
  test(depth(be4), 4, weight = 2)
  test(depth(be5), 3, weight = 2)

  // tests for `eval`
  test(eval(be1), true, weight = 2)
  test(eval(be2), false, weight = 2)
  test(eval(be3), true, weight = 2)

  // hidden tests for `eval`
  test(eval(be4), false, weight = 2)
  test(eval(be5), true, weight = 2)

  // tests for `getString`
  test(getString(be1), "(true | false)", weight = 2)
  test(getString(be2), "(!(true | false) & !(false | true))", weight = 2)
  test(
    getString(be3),
    "(!((false | !true) & false) & (true & !false))",
    weight = 2
  )

  // hidden tests for `getString`
  test(
    getString(be4),
    "((((true | false) & !true) | false) & (!(true & false) | ((true | false) & !false)))",
    weight = 2
  )
  test(
    getString(be5),
    "(((true | false) & !true) | ((true | false) & !false))",
    weight = 2
  )
}

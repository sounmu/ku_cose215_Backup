file://<WORKSPACE>/tm-examples/src/main/scala/kuplrg/Implementation.scala
### java.lang.OutOfMemoryError: Java heap space

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.3/scala3-library_3-3.3.3.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 4334
uri: file://<WORKSPACE>/tm-examples/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // You can check your implementation here.
    val accept = tm_an_bn_cn.accept
    println(s"tm_an_bn_cn.accept(\"abc\")    = ${accept("abc")}")
    println(s"tm_an_bn_cn.accept(\"aabbcc\") = ${accept("aabbcc")}")
    println(s"tm_an_bn_cn.accept(\"abcabc\") = ${accept("abcabc")}")

    println(s"tm_add.accept(\"1+1\") = ${tm_add.accept("1+1")}")
    println("--------------------------------------------------")
  }

  import HeadMove.*

  // TM accpeting L = { a^n b^n c^n | n ≥ 0 }
  val tm_an_bn_cn: TM = TM(
    states = Set(0, 1, 2, 3, 4, 5),
    symbols = Set('a', 'b', 'c'),
    tapeSymbols = Set('a', 'b', 'c', 'X', 'Y', 'Z', 'B'),
    trans = Map(
      (0, 'a') -> (1, 'X', R),
      (0, 'Y') -> (4, 'Y', R),
      (0, 'B') -> (5, 'B', L),
      (1, 'a') -> (1, 'a', R),
      (1, 'Y') -> (1, 'Y', R),
      (1, 'b') -> (2, 'Y', R),
      (2, 'b') -> (2, 'b', R),
      (2, 'Z') -> (2, 'Z', R),
      (2, 'c') -> (3, 'Z', L),
      (3, 'a') -> (3, 'a', L),
      (3, 'b') -> (3, 'b', L),
      (3, 'Y') -> (3, 'Y', L),
      (3, 'Z') -> (3, 'Z', L),
      (3, 'X') -> (0, 'X', R),
      (4, 'Y') -> (4, 'Y', R),
      (4, 'Z') -> (4, 'Z', R),
      (4, 'B') -> (5, 'B', L),
    ),
    initState = 0,
    blank = 'B',
    finalStates = Set(5),
  )

  // TM accepting L = { a^{n^2} | n ≥ 0 }
  def tm_square: TM = TM(
    states = Set(0, 1, 2, 3, 4, 5, 6, 7, 8),
    symbols = Set('a'),
    tapeSymbols = Set('a', 'X', 'Z', 'B'),
    trans = Map(
      (0, 'a') -> (1, 'Z', R),
      (0, 'B') -> (7, 'B', L),
      (1, 'B') -> (7, 'B', L),
      (1, 'a') -> (2, 'a', L),

      (2, 'X') -> (2, 'X', L),
      (2, 'Z') -> (2, 'Z', L),
      (2, 'B') -> (3, 'B', R),
// move
      (3, 'X') -> (4, 'B', R),
      (3, 'Z') -> (5, 'B', R), 
// carry
      (4, 'X') -> (4, 'X', R),
      (4, 'Z') -> (4, 'Z', R),
      (4, 'a') -> (2, 'X', L),
      (4, 'B') -> (8, 'B', L),

      (5, 'X') -> (5, 'X', R),
      (5, 'a') -> (6, 'X', R),
      (5, 'B') -> (8, 'B', L),
      
      (6, 'a') -> (0, 'X', R),
      (6, 'B') -> (8, 'B', L)
    ),
    initState = 0,
    blank = 'B',
    finalStates = Set(7),
  )

  // TM accepting L = { a^n | n is a fibonacci number }
  def tm_fib: TM = TM(
    states = Set(0, 1, 2, 3, 4, 5),
    symbols = Set('a'),
    tapeSymbols = Set('a', 'X', 'B', 'Y'),
    trans = Map(
      (0, 'a') -> (1, 'X', R),
      (0, 'B') -> (4, 'B', R),

      (1, 'a') -> (1, 'a', R),
      (1, 'B') -> (1, 'B', L),

      (2, 'a') -> (2, 'X', L),
      (2, 'X') -> (2, 'X', L),
      (2, 'B') -> (3, 'B', R),

      (3, 'X') -> (4, 'X', R),
      (3, 'a') -> (5, 'a', R),

      (4, 'X') -> (4, 'B', L), // 문자에 a가 없으면 q5로
      (4, 'Y') -> (4, 'B', L),
      (4, 'B') -> (5, 'B', R),
    ),
    initState = 0,
    blank = 'B',
    finalStates = Set(5),
  )

  // TM accepting L = { w \in {a, b, c}* | N_a(w) = N_b(w) = N_c(w) }
  def tm_eq_abc: TM = TM(
    states = Set(0, 1, 2, 3, 4, 5, 6, 7, 8),
    symbols = Set('a', 'b', 'c'),
    tapeSymbols = Set('a', 'X', 'B'),
    trans = Map(
      (0, 'X') -> (0, 'X', R),

      (0, 'a') -> (1, 'X', R), // a (bc || cb)
      (0, 'b') -> (2, 'X', R), // b (ac || ca)
      (0, 'c') -> (3, 'X', R), // c (ab || ba)

      (1, 'X') -> (1, 'X', R),
      (1, 'a') -> (1, 'a', R),
      (1, 'b') -> (4, 'X', R), // a b c
      (1, 'c') -> (5, 'X', R), // a c b

      (2, 'X') -> (2, 'X', R),
      (2, 'b') -> (2, 'b', R),
      (2, 'a') -> (4, 'X', R), // b a c
      (2, 'c') -> (6, 'X', R), // b c a
    
      (3, 'X') -> (3, 'X', R),
      (3, 'c') -> (3, 'c', R), 
      (3, 'a') -> (5, 'a', R), // c a b
      (3, 'b') -> (6, 'b', R), // c b a

      (4, 'X') -> (4, 'X', R), // 마지막으로 c 잡기
      (4, 'a') -> (4, 'a', R),
      (4, 'b') -> (4, 'b', R),
      (4, 'c') -> (, 'X', R),
      
      (5, 'X') -> (5, 'X', R), // 마지막으로 b 잡기
      (5, 'a') -> (5, 'a', R),
      (5, 'b') -> (, 'X', R),
      (5, 'c') -> (5, 'c', R),

      (6, 'X') -> (6, 'X', R), // 마지막으로 a 잡기
      (6, 'a') -> (, 'X', R),
      (6, 'b') -> (6, 'b', R),
      (6, 'c') -> (, '@@X', R),

    ),
    initState = 0,
    blank = 'B',
    finalStates = Set(5),
  )
  // TM for a function f(w ∈ {0, 1}*) = w' where w' = w - 1 if w starts with 1,
  // otherwise f(w) is not defined
  def tm_dec: TM = TM(// 첫 문자가 1인지 확인한 후에 끝으로 이동해서 끝 문자가 0일 때, 1일 때 나누기. 
  // 끝이 0일 때는 그거를 1로 바꾸고 왼쪽으로 이동해서 그게 또 1일 때 종료. 0이면 또 1로 바꾸고 왼쪽으로 이동.

    states = Set(0, 1, 2, 3, 4, 5, 6, 7, 8),
    symbols = Set('0', '1'),
    tapeSymbols = Set('0', '1', 'B'), 
    trans = Map(
      (0, '1') -> (1, '1', R), //첫 문자가 1인지 검증하는 단계
      (1, '1') -> (1, '1', R),
      (1, '0') -> (1, '0', R),
      (1, 'B') -> (2, 'B', L), // 이후 오른쪽 끝으로 이동.
      (2, '0') -> (3, '1', L), // 1|0
      (2, '1') -> (7, '0', L),  // 1|1 -> |10 ok, |1 -> |B0   포인터 옮겨야 함.
      
      (3, '0') -> (3, '1', L),
      (3, '1') -> (4, '0', L),  // 100000 - 1 = |B011111B , 101000 - 1 = 1|00111

      (4, 'B') -> (5, 'B', R), // 제일 앞이 바뀌 경우.
      (4, '1') -> (7, '1', L),
      (4, '0') -> (7, '0', L),

      (5, '0') -> (8, 'B', R),

      (7, '0') -> (7, '0', L),
      (7, '1') -> (7, '1', L),
      (7, 'B') -> (8, 'B', R),
    ),
    initState = 0,
    blank = 'B',
    finalStates = Set(8),
  )
  // TM for a function f(x+y) = z where x,y ∈ {0, 1}* start with 1 and z = x + y
  def tm_add: TM = TM(
    states = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), //처음에 입력 받을 때부터 검증 과정을 추가해야 하네...
    symbols = Set('0', '1', '+'),
    tapeSymbols = Set('0', '1', '+', 'B'),
    trans = Map(
      (0, '0') -> (0, '1', L),
      (0, '1') -> (1, '0', L),
      (0, '+') -> (4, 'B', R),

      (1, '+') -> (3, '+', L),
      (1, '0') -> (1, '0', L),
      (1, '1') -> (1, '1', L),

      (2, 'B') -> (0, 'B', L),
      (2, '0') -> (2, '0', R),
      (2, '1') -> (2, '1', R),
      (2, '+') -> (2, '+', R),

      (3, '1') -> (3, '0', L),
      (3, 'B') -> (2, '1', R),
      (3, '0') -> (2, '1', R),

      (4, '1') -> (4, 'B', R),
      (4, 'B') -> (5, 'B', L),

      (5, 'B') -> (5, 'B', L),
      (5, '1') -> (7, '1', L),
      (5, '0') -> (7, '0', L),

      (6, '1') -> (9, '1', R),

      (7, '1') -> (7, '1', L),
      (7, '0') -> (7, '0', L),
      (7, 'B') -> (8, 'B', R),

      (9, '0') -> (9, '0', R),
      (9, '1') -> (9, '1', R),
      (9, '+') -> (10, '+', R),
      (10, '1') -> (2, '1', R),
    ),
    initState = 6,
    blank = 'B',
    finalStates = Set(8),
  )
}

```



#### Error stacktrace:

```

```
#### Short summary: 

java.lang.OutOfMemoryError: Java heap space
file://<WORKSPACE>/src/main/scala/kuplrg/Implementation.scala
### java.lang.AssertionError: assertion failed: NoType

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<WORKSPACE>/.bloop/root/bloop-bsp-clients-classes/classes-Metals-r6yOKNezRSCVaTiNvriqzA== [exists ], <HOME>/Library/Caches/bloop/semanticdb/com.sourcegraph.semanticdb-javac.0.9.9/semanticdb-javac-0.9.9.jar [exists ], <WORKSPACE>/lib/warts.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.3/scala3-library_3-3.3.3.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12.jar [exists ]
Options:
-feature -deprecation -explain -explain-types -language:implicitConversions -Xsemanticdb -sourceroot <WORKSPACE>


action parameters:
offset: 567
uri: file://<WORKSPACE>/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

object Implementation extends Template {

  def sqsum(x: Int, y: Int): Int = (x * x) + (y * y)

  def concat(left: String, right: String): String = left + right

  def subN(n: Int): Int => Int = b => b - n

  def twice(f: Int => Int): Int => Int = x => f(f(x))

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  def sumOnlyOdd(l: List[Int]): Int = l.filter(_ % 2 == 1).sum

  def foldWith(f: (Int, Int) => Int): List[Int] => Int = l => l.foldLeft(0)(f)

  def toSet(l: List[Int], from: Int): Set[Int] = l.zipWithIndex.filter{ c@@}

  def getOrZero(map: Map[String, Int], key: String): Int = map.getOrElse(key, 0)

  def setMinus(s1: Set[Int], s2: Set[Int]): Set[Int] = s1.diff(s2)

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  def has(value: Int): Tree => Boolean = 
    case Tree.Leaf(v) => v == value
    case Tree.Branch(left, v, right) => v == value || has(value)(left) || has(value)(right)

  def maxDepthOf(value: Int): Tree => Option[Int] = ???

  def mul(t: Tree): Int = ???

  def countLeaves(t: Tree): Int = ???

  def postOrder(t: Tree): List[Int] = ???

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  def countLiterals(expr: BE): Int = ???

  def countNots(expr: BE): Int = ???

  def depth(expr: BE): Int = ???

  def getString(expr: BE): String = ???

  def eval(expr: BE): Boolean = ???
}

```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
	dotty.tools.dotc.core.Types$TypeBounds.<init>(Types.scala:5178)
	dotty.tools.dotc.core.Types$AliasingBounds.<init>(Types.scala:5257)
	dotty.tools.dotc.core.Types$TypeAlias.<init>(Types.scala:5279)
	dotty.tools.dotc.core.Types$TypeAlias$.apply(Types.scala:5316)
	dotty.tools.dotc.core.Types$Type.bounds(Types.scala:1756)
	scala.meta.internal.pc.completions.CaseKeywordCompletion$.contribute(MatchCaseCompletions.scala:156)
	scala.meta.internal.pc.completions.Completions.advancedCompletions(Completions.scala:443)
	scala.meta.internal.pc.completions.Completions.completions(Completions.scala:183)
	scala.meta.internal.pc.completions.CompletionProvider.completions(CompletionProvider.scala:86)
	scala.meta.internal.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:146)
```
#### Short summary: 

java.lang.AssertionError: assertion failed: NoType
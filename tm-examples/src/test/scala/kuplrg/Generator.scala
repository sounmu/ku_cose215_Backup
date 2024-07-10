package kuplrg

import scala.util.Random._
import scala.math._

object Generator {
  
  // L = { w \in {a, b, c}* | N_a(w) = N_b(w) = N_c(w) }
  def tm_eq_abc_accept(nMax: Int)(): Word = {
    shuffle(List('a', 'b', 'c').flatMap(c => List.fill(nextInt(nMax + 1))(c))).mkString
  }
  
}
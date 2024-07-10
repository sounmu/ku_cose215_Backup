package kuplrg

trait Template {
  def dfa_a_b_star: DFA
  def dfa_div_3_1: DFA
  def dfa_subseq_101: DFA
  def dfa_odd_0_1: DFA
  def nfa_two_0: NFA
  def nfa_substr_011: NFA
  def nfa_has_00_or_11: NFA
  def enfa_ab_plus: ENFA
  def enfa_same_digits: ENFA
  def enfa_complex: ENFA
}

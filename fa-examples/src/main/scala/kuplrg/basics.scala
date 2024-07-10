package kuplrg

// The type definitions of states and symbols
type State = Int

// The type definitions of states and symbols
type Symbol = Char

// The type definition of words
type Word = String

// The type definition of languages
type Lang = (Set[Symbol], Word => Boolean)

// A helper function to extract first symbol and rest of word
object `<|` { def unapply(w: Word) = w.headOption.map((_, w.drop(1))) }

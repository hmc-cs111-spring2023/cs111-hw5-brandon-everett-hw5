package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

trait RegularLanguage

case object Empty extends RegularLanguage

case object Epsilon extends RegularLanguage

case class Character(c: Char) extends RegularLanguage

case class Union(l1: RegularLanguage, l2: RegularLanguage) extends RegularLanguage

case class Concat(l1: RegularLanguage, l2: RegularLanguage) extends RegularLanguage

case class Star(l: RegularLanguage) extends RegularLanguage

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = lang match {
  case Concat(Epsilon, sublang) => simplify(sublang)
  case Concat(sublang, Epsilon) => simplify(sublang)
  case Concat(Empty, sublang) => simplify(Empty)
  case Concat(sublang, Empty) => simplify(Empty)
  case Concat(sublang1, sublang2) => Concat(simplify(sublang1), simplify(sublang2))
  case Union(Empty, sublang) => simplify(sublang)
  case Union(sublang, Empty) => simplify(sublang)
  case Union(sublang1, sublang2) => Union(simplify(sublang1), simplify(sublang2))
  case Star(Epsilon) => Epsilon
  case Star(Empty) => Empty
  case Star(sublang) => Star(simplify(sublang))
  case _ => lang
}


/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = lang match {
  case Empty => false
  case Epsilon => true
  case Character(_) => false
  case Union(l1, l2) => nullable(l1) || nullable(l2)
  case Concat(l1, l2) => nullable(l1) && nullable(l2)
  case Star(_) => true
}

/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = l match {
  case Empty | Epsilon => Empty
  case Character(ch) => if (c == ch) Epsilon else Empty
  case Union(l1, l2) => Union(derivative(l1)(c), derivative(l2)(c))
  case Concat(l1, l2) => 
    if (nullable(l1)) Union(Concat(derivative(l1)(c), l2), derivative(l2)(c))
    else Concat(derivative(l1)(c), l2)
  case Star(inner) => Concat(derivative(inner)(c), Star(inner))
}

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
package dfa // leave this line in the file

case class State(label: String):

case class Transition(from: State, symbol: Char, to: State):

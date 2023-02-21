package dfa // leave this line in the file

case class State(label: String)

case class Transition(from: State, to: State, symbol: Char)

class DFA(val states: Set[State], val transitions: Set[Transition], val start: State, val accept: Set[State]) {
  def accepts(input: String): Boolean = {
    var currentState = start
    for (symbol <- input) {
      val transitionOpt = transitions.find(t => t.from == currentState && t.symbol == symbol)
      transitionOpt match {
        case Some(transition) => currentState = transition.to
        case None => false
      }
    }
    accept.contains(currentState)
  }
}

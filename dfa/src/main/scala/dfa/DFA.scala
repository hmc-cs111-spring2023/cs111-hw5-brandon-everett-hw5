package dfa // leave this line in the file

case class State(label: String):
    def stateName = label
    override def toString(): String = lable

case class Transition(from: State, symbol: Char, to: State):
    def previosState = from
    def value = symbol
    def nextState = to

class DFA (val states: Set[State], val transitions: Set[Transition], val startState: State, val acceptingStates: Set[State]):
    
    def accepts (str: String): Boolean = {

    }


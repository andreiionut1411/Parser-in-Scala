import scala.annotation.tailrec
import scala.collection.mutable

class Dfa[A] (alphabet: Set[Char], states: Set[A], initialState: A, finalState: Set[A], transitions: Map[A, Map[Char, A]]){

  var dfaStateFromNfa: mutable.Map[Int, Set[Int]] = mutable.Map(0 -> Set(-1))
  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = new Dfa[B](alphabet, states.map(f), f(initialState), finalState.map(f), transitions.map{case (k, v) => (f(k), v.map{case (x, y) => (x, f(y))})}) // TODO implement map

  def next(state:A, c: Char): A = if(alphabet.contains(c)) transitions.apply(state).apply(c)
  else states.head // Daca avem un caracter care nu face parte din alfabet, atunci e trecut in sink state

  def accepts(str: String): Boolean = {
    // Verificam daca din starea curenta pe cuvantul ramas se ajunge in starea finala.
    @tailrec
    def verifyState(state: A, string: List[Char]): Boolean = {
      string match {
        case Nil => isFinal(state)
        case x::xs => if (alphabet.contains(x)) verifyState(next(state, x), xs) else false
        // Daca avem litere care nu fac parte din alfabetul automatului, atunci rejectam cuvantul
      }
    }

    verifyState(initialState, str.toList)
  }

  def getStates : Set[A] = states // TODO implement getStates

  def isFinal(state: A): Boolean = finalState.foldRight(false)((x:A, acc: Boolean) => acc || x.equals(state)) // TODO implement isFinal

  def getFinal: Set[A] = finalState

  def getInitial: A = initialState

  def setStates(states: mutable.Map[Int, Set[Int]]): Unit = {
    dfaStateFromNfa = states
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  val dfaStateFromNfa: mutable.Map[Int, Set[Int]] = mutable.Map(0 -> Set(-1))

  def fromPrenex(str: String): Dfa[Int] = {
    val nfa = Nfa.fromPrenex(str)
    dfaFromNfa(nfa)
  } // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa

  def dfaFromNfa(nfa: Nfa[Int]): Dfa[Int] = {
    val nfaStateToDfa: mutable.Map[Set[Int], Int] = mutable.Map((Set(-1), 0))
    val initialState: Set[Int] = epsilonClosure(nfa, 0)
    val fin: Set[Int] = if (dfaStateIsFinal(initialState, nfa.getFinal)) Set(1) else Set()

    // Adaugam in setul nostru de stari pe langa sink si starea initiala.
    // Vom avea 2 map-uri, unul care tine minte ce stare noua in DFA este formata
    // din combinarea mai multor stari din NFA, iar cel de-al doilea va fi complementar
    // si va porni de la starile din DFA si ne va da starile din NFA din care a
    // fost formata.
    nfaStateToDfa.addOne(initialState, 1)
    dfaStateFromNfa.addOne(1 -> initialState)

    val (transitions, finalStates) = createDfaFromState(initialState, nfa.getAlphabet, nfaStateToDfa, nfa, fin, Map())
    // Sink-ul va fi numerotat mereu ca si starea 0, iar starea initiala va fi starea 1, si tot asa.

    val dfa = new Dfa[Int](nfa.getAlphabet, List.range(0, nfaStateToDfa.size).toSet, 1, finalStates, transitions.concat(createSinkState(nfa.getAlphabet)))
    dfa.setStates(dfaStateFromNfa)
    dfa
  }

  def epsilonClosure(nfa: Nfa[Int], state: Int): Set[Int] = {
    def aux(nfa: Nfa[Int], state:Int, set:mutable.Set[Int]): mutable.Set[Int] = {
      val next = nfa.next(state, 0)

      if (set.contains(state)) set
      else {
        set += state

        for (elem <- next) {
          aux(nfa, elem, set)
        }

        set
      }
    }

    aux(nfa, state, mutable.Set()).toSet
  }

  // Verificam daca o stare a DFA-ului formata dintr-un set de stari din NFA este finala.
  def dfaStateIsFinal(states: Set[Int], finalState: Set[Int]): Boolean = {
    states.foldRight(false)((x, acc) => finalState.contains(x) | acc)
  }

  // Cream tranzitiile pentru sink state. Pe toate caracterele din alfabet ne intoarcem in aceiasi stare.
  def createSinkState(alphabet: Set[Char]): Map[Int, Map[Char, Int]] = {
    Map(0 -> alphabet.foldRight(Map[Char, Int]())((x, acc) => acc + (x -> 0)))
  }

  // Functia porneste de la o stare a DFA-ului (care este reprezentata ca un set
  // de stari din NFA), si se adauga tranzitii catre noi stari. Pentru a mapa
  // starile NFA-urilor la numere care sa fie stari pentru DFA-uri, avem un Map
  // care face legatura dintre Set si numar.
  def createDfaFromState(states: Set[Int], alphabet: Set[Char], nfaState: mutable.Map[Set[Int], Int],
                         nfa: Nfa[Int], finalStates: Set[Int],
                         transitions: Map[Int, Map[Char, Int]]): (Map[Int, Map[Char, Int]], Set[Int]) = {

    val dfaState = nfaState.apply(states)
    var dfaTransitions = Map[Char, Int]()
    var crtTransitions = transitions
    var crtFinalStates = finalStates

    for (character <- alphabet) {
      var nextState = Set[Int]() // Aici vom aduna starile urmatoare din NFA pentru a crea o stare in DFA
      nextState = states.foldRight(Set[Int]())((x, acc) => acc.concat(nfa.next(x, character))).foldRight(Set[Int]())((x, acc) => acc.concat(epsilonClosure(nfa, x)))

      // Daca nu avem in NFA o tranzitie, in DFA avem tranzitie catre sink.
      // Daca urmatoarea stare a fost deja gasita, atunci pur si simplu facem tranzitia
      // si nu adaugam aceasta stare noua in map.
      if (nextState.isEmpty) dfaTransitions = dfaTransitions + (character -> 0)
      else if (nfaState.contains(nextState)) dfaTransitions += (character -> nfaState.apply(nextState))
      else {
        dfaStateFromNfa.addOne(nfaState.size -> nextState)
        nfaState.addOne(nextState, nfaState.size)
        dfaTransitions += (character -> (nfaState.size - 1))

        // Daca una din starile din NFA e finala, atunci si starea din DFA este finala
        if (dfaStateIsFinal(nextState, nfa.getFinal)) {
          crtFinalStates = crtFinalStates + nfaState.apply(nextState)

        }

        // Construim DFA-ul mergand in "adancime" in NFA, daca l-am privi ca pe un arbore
        val (newTransitions, newFinalStates) = createDfaFromState(nextState, alphabet, nfaState, nfa, crtFinalStates, crtTransitions)

        crtTransitions = newTransitions
        crtFinalStates = newFinalStates
      }
    }

    (crtTransitions.concat(Map ((dfaState, dfaTransitions))), crtFinalStates)
  }
}

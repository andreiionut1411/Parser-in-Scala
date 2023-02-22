import scala.annotation.tailrec
import scala.collection.mutable

trait binaryTree{}
case object Empty extends binaryTree{}
case class Node(key:String, left:binaryTree, right:binaryTree) extends binaryTree {}

class Nfa[A](alphabet: Set[Char],states: Set[A], initialState:A, finalState:Set[A], transitions: Map[A, Map[Char, Set[A]]]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = new Nfa[B](alphabet, states.map(f), f(initialState), Set(f(finalState.head)), transitions.map{case (k:A, v:Map[Char, Set[A]]) => (f(k), v.map{case (x, y) => (x, y.map(f))}.withDefaultValue(Set()))}.withDefaultValue(Map().withDefaultValue(Set()))) // TODO implement map

  def next(state:A, c: Char): Set[A] = transitions.apply(state).apply(c) // TODO implement next

  def accepts(str: String): Boolean = {
    def verifyState(str:List[Char], state:A): Boolean = {
      str match {
        // Ne oprim daca starea in care am ajuns e finala, sau una din inchiderile epsilon ale acesteia este stare finala
        case Nil => if (isFinal(state) || next(state, 0).map(s => verifyState(Nil, s)).foldRight(false)(_ || _)) true else false

        // Acceptam un automat, daca consumand caracterul curent se ajunge la final intr-o stare finala, sau
        // se poate ajunge la o stare finala mergand prin tranzitii epsilon.
        case x::xs => next(state, x).map(s => verifyState(xs, s)).concat(next(state, 0).map(s => verifyState(x::xs, s))).foldRight(false)((elem, acc) => elem || acc)
      }
    }

    verifyState(str.toList, initialState)
  } // TODO implement accepts

  def getStates : Set[A] = states // TODO implement getStates

  def isFinal(state: A): Boolean = finalState.foldRight(false)((x, acc) => x.equals(state) | acc)  // TODO implement isFinal

  def getFinal: Set[A] = finalState

  def getInitial: A = initialState

  def getTransitions: Map[A, Map[Char, Set[A]]] = transitions

  def getAlphabet: Set[Char] = alphabet
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {

  // Functia ia un Prenex, il parseaza, si intoarce un binary tree bazat pe aceasta,
  // astfel incat daca punem cap la cap ramurile lui, vom obtine NFA-ul.
  def createAST(str:String):binaryTree = {
    val listOfTokens = parseString(str)

    // La final, in stack va ramane doar arborele construit.
    @tailrec
    def createOneNode(list:List[String], stack:mutable.Stack[binaryTree]):mutable.Stack[binaryTree] = {
      list match {
        case Nil => stack
        case "void":: _ => stack.push(Node("void", Empty, Empty))
        case "UNION"::xs => createOneNode(xs, stack.push(Node("UNION", stack.pop(), stack.pop())))
        case "STAR"::xs => createOneNode(xs, stack.push(Node("STAR", stack.pop(), Empty)))
        case "CONCAT"::xs => createOneNode(xs, stack.push(Node("CONCAT", stack.pop(), stack.pop())))
        case "PLUS"::xs => val x = stack.pop(); createOneNode(xs, stack.push(Node("CONCAT", x, Node ("STAR", x, Empty)))) // In tree punem xx*
        case "MAYBE"::xs => createOneNode(xs, stack.push(Node("UNION", stack.pop(), Node("eps", Empty, Empty)))) // In tree punem o reuniune de a si epsilon
        case x::xs => createOneNode(xs, stack.push(Node(x, Empty, Empty)))
      }
    }

    createOneNode(listOfTokens, new mutable.Stack[binaryTree]()).pop()
  }

  // Functia imparte string-ul primit in tokenuri. Acestea sunt separate de spatii si un token poate sa fie sau nu
  // intre 2 apostrofuri.
  def parseString(str: String): List[String] = {
    @tailrec
    def aux(string: List[Char], tokens: List[String], crtToken: List[Char], isApostrophe: Boolean): (List[String], List[Char]) = {
      string match {
        case Nil => if (crtToken.isEmpty) (tokens, crtToken) else (crtToken.mkString::tokens, crtToken)

        // Daca dam de un apostrof, il adaugam in cuvantul curent, in caz ca apstroful este efectiv un token,
        // nu doar un item de separare. Daca intr-adevar era doar de separare, atunci cand incepe token-ul
        // propriu zis, pur si simplu scoatem apostroful din lista.
        case '\''::xs =>  if (crtToken.isEmpty) aux(xs, tokens, List('\''), isApostrophe = true) else aux(xs, crtToken.mkString::tokens, List(), isApostrophe = false)
        case ' '::xs => if (isApostrophe) aux(xs, tokens, List(' '), isApostrophe = false) else if (crtToken.isEmpty) aux(xs, tokens, List(), isApostrophe = false) else aux(xs, crtToken.mkString::tokens, List(), isApostrophe = false)
        case x::xs => if (!isApostrophe) aux(xs, tokens, crtToken ++ (x::Nil), isApostrophe = false) else aux(xs, tokens, List(x), isApostrophe = false)
      }
    }

    val (parsedString, _) = aux(str.toList, List(), List(), isApostrophe = false)

    parsedString
  }

  // Din sirul de caractere pe care il primim de parsat extragem alfabetul NFA-ului
  def createAlphabet(str: String): Set[Char] = {
    val listOfTokens = parseString(str)
    listOfTokens.foldRight(mutable.Set[Char]())((x, acc) => if (x.length == 1) acc += x.head else acc).toSet[Char]
  }

  def fromPrenex(str: String): Nfa[Int] = {
    val alphabet = createAlphabet(str)
    def fromAst(tree: binaryTree): Nfa[Int] = {
      tree match {
        case Node ("void", Empty, Empty) => new Nfa[Int](alphabet, Set(0, 1), 0, Set(1), Map().withDefaultValue(Map().withDefaultValue(Set())))
        case Node ("eps", Empty, Empty) => new Nfa[Int](alphabet, Set(0), 0, Set(0), Map().withDefaultValue(Map().withDefaultValue(Set())))
        case Node (x, Empty, Empty) => nfaFromChar(x.head, alphabet)
        case Node ("CONCAT", left, right) => nfaFromConcat(fromAst(left), fromAst(right), alphabet)
        case Node ("UNION", left, right) => nfaFromUnion(fromAst(left), fromAst(right), alphabet)
        case Node ("STAR", left, Empty) => nfaFromStar(fromAst(left), alphabet)
      }
    }

    fromAst(createAST(str))
  }
  // TODO implement Prenex -> Nfa transformation.

  def nfaFromChar(c: Char, alphabet: Set[Char]): Nfa[Int] = new Nfa[Int](alphabet, Set(0, 1), 0, Set(1), Map((0, Map((c, Set(1))).withDefaultValue(Set()))).withDefaultValue(Map().withDefaultValue(Set())))

  def nfaFromConcat(x1: Nfa[Int], x2:Nfa[Int], alphabet:Set[Char]): Nfa[Int] = {
    val finalX1 = x1.getFinal.head

    // La toate starile din x2 se adauga numarul de stari de la x1, astfel daca x1 inainte
    // de concatenare avea starea finala 2, atunci starea initiala a lui x2 va fi 3.
    val newX2 = x2.map[Int](x => x + finalX1 + 1)

    new Nfa[Int](alphabet, x1.getStates.concat(newX2.getStates), 0, newX2.getFinal, x1.getTransitions.++(newX2.getTransitions).++(Map((finalX1, Map((0.toChar, Set(finalX1 + 1))).withDefaultValue(Set()))).withDefaultValue(Map().withDefaultValue(Set()))))
  }

  def nfaFromUnion(x1: Nfa[Int], x2: Nfa[Int], alphabet: Set[Char]): Nfa[Int] = {
    val numStateX1 = x1.getFinal.head + 1
    val numStateX2 = x2.getFinal.head + 1
    val finalState = Set(numStateX1 + numStateX2 + 1)
    // Avem + 1 pentru ca adaugam pe deasupra si o noua stare 0 ca fiind initiala

    val newX1 = x1.map(_ + 1)
    val newX2 = x2.map(_ + numStateX1 + 1)

    // Adaugam tranzitiile de la noua stare initiala la starile initiale ale celor 2 NFA-uri.
    val initalTransitions: Map[Int, Map[Char, Set[Int]]]= Map((0, Map((0.toChar, Set(newX1.getInitial, newX2.getInitial))).withDefaultValue(Set()))).withDefaultValue(Map().withDefaultValue(Set()))

    // Adaugam tranzitia de la starea finala x1 la noua stare finala.
    val finalX1Transition: Map[Int, Map[Char, Set[Int]]] = Map((newX1.getFinal.head, Map((0.toChar, Set(finalState.head))).withDefaultValue(Set()))).withDefaultValue(Map().withDefaultValue(Set()))

    // Adaugam tranzitia de la starea finala x2 la noua stare finala.
    val finalX2Transition: Map[Int, Map[Char, Set[Int]]] = Map((newX2.getFinal.head, Map((0.toChar, Set(finalState.head))).withDefaultValue(Set()))).withDefaultValue(Map().withDefaultValue(Set()))

    new Nfa[Int](alphabet, newX1.getStates.concat(newX2.getStates).concat(Set(0, finalState.head)), 0, finalState, newX1.getTransitions.++(newX2.getTransitions).++(initalTransitions).++(finalX1Transition).++(finalX2Transition))
  }

  def nfaFromStar(x: Nfa[Int], alphabet: Set[Char]): Nfa[Int] = {
    val finalState = x.getFinal.head + 2
    val newX = x.map(_ + 1)

    // Adaugam tranzitia initiala si tranzitia de la starea initiala la finala pentru a accepta epsilon
    val initialTransition: Map[Int, Map[Char, Set[Int]]] = Map((0, Map((0.toChar, Set(1, finalState))).withDefaultValue(Set()))).withDefaultValue(Map().withDefaultValue(Set()))

    // Adaugam tranzitia catre starea finala si tranzitia de intoarcere
    val finalTransition: Map[Int, Map[Char, Set[Int]]] = Map((newX.getFinal.head, Map((0.toChar, Set(finalState, 1))).withDefaultValue(Set()))).withDefaultValue(Map().withDefaultValue(Set()))

    new Nfa[Int](alphabet, newX.getStates.concat(Set(0, finalState)), 0, Set(finalState), newX.getTransitions.++(initialTransition).++(finalTransition))
  }

  // You can add more methods to this object
}
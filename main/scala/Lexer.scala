case class Lexer (spec: String) {

  // Este un map care are ca si cheie valoarea starii finale si ca valoare tokenul din
  // specificatie si pozitia in care se afla el in fisier
  var TokenMap: Map[Int, (String, Int)] = Map()

  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */
  def lex(word: String): Either[String,List[(String,String)]] = {
    val dfa = createLexer(spec)
    def aux(word: List[Char], acc: List[(String, String)], crtSequence: List[Char],
            prevSequence: List[Char], prevToken: String, prevWord: List[Char],
            dfa: Dfa[Int], state: Int, characterIndex: Int, prevChar: Int,
            line: Int, prevLine: Int): List[(String, String)] = {
      word match {
        case Nil => if (dfa.isFinal(state)) acc ++ ((crtSequence.mkString, currentToken(dfa, state))::Nil)
        else if (prevToken.equals("")) List(("", "e " + line.toString))
        else aux(prevWord, acc ++ ((prevSequence.mkString, prevToken)::Nil), Nil, Nil, "", prevWord, dfa, dfa.getInitial, characterIndex + 1, prevChar, prevLine, prevLine)
        case x::xs =>
          val nextState = dfa.next(state, x)
          val lineAux = if (x == '\n') line + 1 else line
          if (stateIsSink(nextState)) if (!prevToken.equals("")) aux(prevWord, acc ++ ((prevSequence.mkString, prevToken)::Nil), Nil, Nil, "", prevWord, dfa, dfa.getInitial, prevChar, prevChar, prevLine, prevLine)
          else List(("", characterIndex.toString + " " + line.toString))
          else if (dfa.isFinal(nextState)) aux(xs, acc, crtSequence ++ (x::Nil), crtSequence ++ (x::Nil), currentToken(dfa, nextState), xs, dfa, nextState, characterIndex + 1, characterIndex + 1, lineAux, lineAux)
          else aux(xs, acc, crtSequence ++ (x::Nil), prevSequence, prevToken, prevWord, dfa, nextState, characterIndex + 1, prevChar, lineAux, prevLine)
      }
    }

    val result: List[(String, String)] = aux(word.toList, Nil, Nil, Nil, "", Nil, dfa, dfa.getInitial, 0, 0, 0, 0)

    // Erorile le primim daca primul String este vid sau nu
    if (!result.head._1.equals("")) Right(result)
    else if (result.head._2.head == 'e') Left("No viable alternative at character EOF, line " + result.head._2.split("\\s").apply(1))
    else Left("No viable alternative at character " + result.head._2.split("\\s").apply(0) + ", line " + result.head._2.split("\\s").apply(1))
    // Dupa spatiu avem mereu numarul liniei
  }

  def splitSpecInLines(spec: String): List[String] = spec.split("\\n").toList

  // Daca folosim String-uri formate cu ajutorul """, atunci caracterele speciale cum ar fi \n nu
  // vor fi interpretate, asa ca, vom inlocui toate caracterele speciale cu caracterul propriu zis.
  // Aceste caractere speciale facem conventia ca vor fi mereu intre ghilimele.
  def createSpecialCharacters(str: String): String = str.replace("\'\\n\'", "\'\n\'")
    .replace("\'\\b\'", "\'\b\'").replace("\'\\t\'", "\'\t\'")
    .replace("\'\\f\'", "\'\f\'").replace("\'\\r\'", "\'\r\'")

  def createNfaFromLine(line: String): Nfa[Int] = {
    val tokens = line.split(":").toList

    // Ignoram primul caracter pentru ca urmeaza spatiu dupa :
    // Apoi, inversam regexul, si taiem ultimul caracter deoarece ';' nu face parte din regexul propriu zis
    val regularExpression = tokens.tail.head.toList.tail.reverse.tail.reverse.mkString
    Nfa.fromPrenex(Regex.toPrenex(createSpecialCharacters(regularExpression)))
  }

  // Functia creaza NFA-ul care uneste toate regex-urile primite din fisierul de specificatii
  def createLexerNfa(spec: List[String]): Nfa[Int] = {
    var initialStates: Set[Int] = Set() // Contine starile initiale din noul Nfa
    var prevStates = 1
    var counter = 0
    var nfaArray: List[Nfa[Int]] = List()
    var alphabet: Set[Char] = Set()
    var states: Set[Int] = Set()
    var finalStates: Set[Int] = Set()
    var transitions: Map[Int, Map[Char, Set[Int]]] = Map().withDefaultValue(Map().withDefaultValue(Set()))

    for (line <- spec) {
      val token = line.split(":").head
      var nfa = createNfaFromLine(line)
      val numberOfStates = nfa.getStates.size
      nfa = nfa.map(_ + prevStates)

      // Adaugam in map pentru starea finala curenta tokenul din spec
      TokenMap = TokenMap + (nfa.getFinal.head -> (token, counter))
      counter += 1

      // Cream tot ce ne trebuie ca sa construim nfa-ul mare
      prevStates += numberOfStates
      initialStates = initialStates + nfa.getInitial
      nfaArray = nfaArray ++ (nfa::Nil)
      alphabet = alphabet ++ nfa.getAlphabet
      states = states ++ nfa.getStates
      finalStates = finalStates ++ (nfa.getFinal.head::Nil)
      transitions = transitions ++ nfa.getTransitions
    }

    val initialTransition: Map[Int, Map[Char, Set[Int]]] = Map((0, Map((0.toChar, initialStates)).withDefaultValue(Set()))).withDefaultValue(Map().withDefaultValue(Set()))

    new Nfa[Int](alphabet, states, 0, finalStates, transitions ++ initialTransition)
  }

  // Functia creaza DFA-ul final pe care se va baza lexerul
  def createLexer(spec: String): Dfa[Int] = {
    val nfa = createLexerNfa(splitSpecInLines(spec))
    Dfa.dfaFromNfa(nfa)
  }

  // Functia primeste DFA-ul lexerului si starea curenta si intoarce numele tokenului
  // care a fost descoperit in starea respectiva, daca exista. Daca sunt mai multi tokeni,
  // atunci se intoarce token-ul cu prioritatea cea mai mare.
  def currentToken(dfa: Dfa[Int], state: Int): String = {
    val nfaStates: Set[Int] = dfa.dfaStateFromNfa.apply(state)
    var token = ""
    var bestPriority = TokenMap.size + 1

    for (s <- nfaStates) {
      if (TokenMap.contains(s)) {
        val value = TokenMap.apply(s)

        // Verificam daca starea curenta are prioritate mai buna ca ce am descoperit anterior.
        if (value._2 < bestPriority) {
          bestPriority = value._2
          token = value._1
        }
      }
    }

    token
  }

  def stateIsSink(state: Int):Boolean = state == 0
}
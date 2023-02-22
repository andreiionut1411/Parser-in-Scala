import scala.annotation.tailrec
import scala.collection.mutable

object Regex {
  // Dupa paranteza nu conteaza ce operator urmeaza, asa ca daca vrem sa mearga
  // algoritmul corect, punem ')' sa aiba precedenta minima. Pentru ca algoritmul
  // Shunting-Yard ne da in mod infixat ecuatia, pentru a o face in mod prefixat,
  // parantezele isi interschimba rolurile.
  val characterPrecedence: Map[String, Int] = Map("|" -> 1, "." -> 2, "*" -> 3, "?" -> 3, "+" ->3, ")" -> 0)
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  def preprocess(s:List[Char]): List[Either[Char,Char]] = ???

  def charIsOperator(x: String): Boolean = {
    if (x.equals(".") || x.equals("|") || x.equals("*") || x.equals("?") || x.equals("+")) return true

    false
  }

  // Concat nu are un semn, asa ca preprocesam mai intai stringul si acolo unde ar fi trebuit
  // sa fie o concatenare, atunci adaugam '.' pentru a fi mai usoara parsarea in viitor.
  // Concatul se adauga intre 2 non-operatori (am inclus aici si parantezele), sau se poate
  // face concat dupa un operator unar (*, ?, +) si un non-operator.
  def addConcat(str: String): List[String] = {
    @tailrec
    def aux(str: List[String], acc: List[String]): List[String] = {
      str match {
        case Nil => acc
        case x::Nil => acc ++ (x::Nil)
        case x::y::xs => if ((!charIsOperator(x) && !x.equals("(") && !charIsOperator(y) && !y.equals(")")) || (charIsOperator(x) && !x.equals("|") && !charIsOperator(y) && !y.equals(")"))) {
          aux(y :: xs, acc ++ (x :: "." :: Nil))
        } else {
          aux (y::xs, acc ++ (x::Nil))
        }
      }
    }

    aux(eliminateApostrophes(str.replaceAll("eps", "\u0000")), Nil)
  }

  def eliminateApostrophes(str: String): List[String] = {
    @tailrec
    def aux(str: List[Char], acc: List[String], openApostrophe: Boolean, element: List[Char]): List[String] = {
      // Daca un caracter este incadrat intre apostroafe, atunci il consideram un singur element,
      // altfel fiecare caracter este un element. Variabila element contine toate caracterele
      // dintre apostroafe, pentru ca spre exemplu caracterele speciale cum ar fi \n sunt de fapt
      // formate din mai multe caractere de sine statatoare. Daca dam de un apostrof trebuie
      // sa tinem minte daca este un apostrof care deschide sau inchide un element, deoarece va trebui
      // sa luam actiuni diferite in functie de fiecare situatie.
      str match {
        case Nil => acc
        case '\''::xs => if (openApostrophe) aux(xs, acc ++ (("\'" + element.mkString + "\'")::Nil), false, Nil)
        else aux(xs, acc, true, Nil)
        case x::xs => if (openApostrophe) aux(xs, acc, openApostrophe, element ++ (x::Nil))
        else aux(xs, acc ++ (x.toString::Nil), false, Nil)
      }
    }

    aux(str.toList, Nil, false, Nil)
  }

  def addSyntacticSugars(str: String): String = {
    var aux = str.replaceAll("\\[0-9]", "(0|1|2|3|4|5|6|7|8|9)")
    aux = aux.replaceAll("\\[a-z]", "(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)")
    aux = aux.replaceAll("\\[A-Z]", "(A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z)")
    aux
  }

  def preprocess(str: String): List[String] = addConcat(addSyntacticSugars(str))

  def toPrefix(str: List[String]): List[String] = {
    // Folosim algoritmul Shunting-Yard
    val outputStack = mutable.Stack[String]()
    val operatorStack = mutable.Stack[String]()

    @tailrec
    def aux(str: List[String]): List[String] = {
      str match {
        case Nil =>
          while (operatorStack.nonEmpty) {
            outputStack.push(operatorStack.pop()) // Scoatem toti operatorii care au ramas pe stiva
          }
          outputStack.toList
        case x::xs =>
          if (charIsOperator(x) && operatorStack.isEmpty) {
            operatorStack.push(x)
            aux(xs)
          } else if (charIsOperator(x) && characterPrecedence.apply(x) <= characterPrecedence.apply(operatorStack.top)) {
            var stop = false
            while (operatorStack.nonEmpty && !stop) {
              if (characterPrecedence.apply(x) <= characterPrecedence.apply(operatorStack.top)) {
                outputStack.push(operatorStack.pop())
              } else stop = true
            }
            operatorStack.push(x)
            aux(xs)
          } else if (charIsOperator(x)) {
            operatorStack.push(x)
            aux(xs)
          }else if (x.equals(")")) {
            operatorStack.push(x)
            aux(xs)
          } else if (x.equals("(")) {
            while (!operatorStack.top.equals(")")) {
              outputStack.push(operatorStack.pop())
            }
            operatorStack.pop()
            aux(xs)
          } else {
            outputStack.push(x)
            aux(xs)
          }
      }
    }

    aux(str)
  }

  def transformPrefix(prefix: List[String]): String = {
    @tailrec
    def aux(prefix: List[String], acc: String): String = {
      prefix match {
        case Nil => acc
        case "|"::xs => aux(xs, acc + "UNION ")
        case "."::xs => aux(xs, acc + "CONCAT ")
        case "*"::xs => aux(xs, acc + "STAR ")
        case "?"::xs => aux(xs, acc + "MAYBE ")
        case "+"::xs => aux(xs, acc + "PLUS ")
        case "\u0000"::xs => aux(xs, acc + "eps ")
        case x::xs => aux(xs, acc + x + " ")
      }
    }

    // Ultimul caracter e spatiu, asa ca il eliminam
    aux(prefix, "").reverse.tail.reverse
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = transformPrefix(toPrefix(preprocess(str).reverse))
}

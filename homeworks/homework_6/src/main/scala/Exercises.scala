object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = {
    var reversed_seq:List[T] = List()
    var given_seq = seq
    while (given_seq.nonEmpty) {
      reversed_seq = given_seq.head :: reversed_seq
      given_seq = given_seq.tail
    }
    reversed_seq
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = {
    idx match{
      case 0 => 0
      case 1 => 1
      case _ => fibonacci4Index(idx-1) + fibonacci4Index(idx-2)
    }
  }

  def fibonacci(idx: Int): Seq[Int] = {
    var result = scala.collection.mutable.ListBuffer[Int]()
    var count = idx
    while (count > 0){
      /* Надеюсь не запрещается пользоваться предыдущей функцией */
      result += fibonacci4Index(count)
      count = count - 1
    }
    result += 0
    result.toList.reverse
    }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = {
    val str = text.toUpperCase().toList
    var list_res = scala.collection.mutable.ListBuffer[String]()
    var str_res = ""
    for (x <- str){
      val in_morse = MORSE.contains(x.toString)
      in_morse match {
        case true => list_res += (MORSE.get(x.toString)).toString.replace("Some", "").replace("(", "").replace(")", "")
        case false => {
          list_res += x.toString
        }
      }
    }
    for (y <- list_res){
      val in_morse = MORSE.values.exists(_ == y)
      if (!in_morse){
        if (y != " ") str_res += y
        else str_res += "  "
      }
      else {
        str_res += " "
        str_res += y
      }
    }
    if (str_res(0).toString == " ") return str_res.replaceFirst(" ", "")
    str_res
  }



  def wordReverse(text: String): String = {
    val text_list = text.split(" ").toList
    var res = scala.collection.mutable.ListBuffer[String]()
    var word = ""
    var symb = ""
    var bool_up = false
    for (x <- text_list){
      for (y <- x) {
        if (y.isLetter){
          if (y.isUpper){
            bool_up = true
          }
          word += y
        }
        else {
          symb += y
        }
      }
      if (bool_up)
      {
        res += word.reverse.toLowerCase.capitalize + symb
      }
      else {
        res += word.reverse.toLowerCase + symb
      }
      symb = ""
      word = ""
      bool_up = false
    }
    res.mkString(" ")
  }

}

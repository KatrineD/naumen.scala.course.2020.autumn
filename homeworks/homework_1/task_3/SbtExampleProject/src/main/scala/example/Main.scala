package example

object Main extends App {
  var first = "He"
  var second = 110
  var third = List("S", "c", "a", "l", "a", "!")
  var fourth = List("Th1s", "1s")
  var name = "Katr1ne Dem1cheva."
  var all_str = (Array(first + second.toString, third.mkString(""), fourth.mkString(" "), name)).mkString(" ")
  var res_str = all_str.replace("11", "ll").replace("0", "o").replace("1", "i")
  println(res_str)

  def greetings(str: String):Tuple2[String, String] ={
    var str_became_list = str.split(" ").toList;
    val bonjour = str_became_list.updated(0, "Bonjour").mkString(" ");
    val saluton = str_became_list.updated(0, "Saluton").mkString(" ");
    return(bonjour, saluton)
  }

  greetings(res_str).productIterator.foreach(println)


  var greetings_list = (greetings(res_str)).productIterator.toList

  def reversed_name(str: String): String ={
    var str_rep = str.replace("Katrine Demicheva", "KatrineDemicheva")
    val reversed_name = name.reverse.replace("1", "i")
    var str_became_list = str_rep.split(" ").toList
    val rev_str = str_became_list.updated(4, reversed_name).mkString(" ").replace(".", "") + "."
    return(rev_str)
  }

  for (gr <- greetings_list){
    println(reversed_name(gr.toString))
  }
}

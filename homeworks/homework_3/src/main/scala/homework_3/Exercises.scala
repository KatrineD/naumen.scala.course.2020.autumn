package homework_3

object Exercises {


    /**
     * Задание №1
     * Реализуйте функцию, которая принимает любой тип и преобразует его в строку.
     * Для всех типов кроме Boolean достаточно воспользоваться стандартной функцией .toString.
     * Для типа Boolean сделайте особое преобразование: true -> "правда", false -> "ложь".
     *
     * Реализуйте функцию тремя разными способами, отличающимися тем, как определяется какой тип имеет значение переданное в аргументе. 
     * Определение типа необходимо для реализации специальной логики работы с Boolean значениями, которая описана в условии выше.
     */
    def prettyBooleanFormatter1(x: Any): String = {
      if (!x.isInstanceOf[Boolean]) {
        x.toString
      }
      else {
        x match{
          case true => "правда"
          case false => "ложь"
        }
      }
    }

    def prettyBooleanFormatter2(x: Any): String = {
      x match {
        case x: Boolean => x match {
          case true => "правда"
          case false => "ложь"
        }
        case _ => x.toString
      }
    }

    import scala.reflect.ClassTag
    def prettyBooleanFormatter3(x: Any): String = {
      var res_type = ""
      val type_of_x = ClassTag(x.getClass)
      if (type_of_x.toString != "java.lang.Boolean"){
        res_type += x.toString
      }
      else {
        if (x == true){
          res_type = "правда"
        }
        else {
          res_type = "ложь"
        }
      }
      res_type
    }


    /**
     * Задание №2
     * Реализуйте функцию нахождения максимального числа в переданной коллекции интов (можно использовать все методы стандартной библиотеки).
     *
     * Реализуйте функцию тремя разными способами, отличающимися тем как функция себя ведет на пустой коллекции. 
     * Обратите внимание на возвращаемые типы.
     */
    def max1(xs: Seq[Int]): Int = {
      xs.isEmpty match {
        case false => xs.max
        case true => 0
      }
    }

    def max2(xs: Seq[Int]): Seq[Int] = {
      if (xs.isEmpty) {
        xs
      } else {
        Seq(xs.max)
      }
    }

    def max3(xs: Seq[Int]): Option[Int] = {
      xs.isEmpty match {
        case false => Some(xs.max)
        case true => None
      }
    }    

    /**
     * Задание №3
     * Допустим дана функция sumIntegers, которая умеет суммировать числа.
     */
    def sumIntegers[CollectionType <: Iterable[Int]](xs: CollectionType): Int = xs.sum

    /**
     * Реализуйте на основе нее 3 варианта суммирования 2х чисел, отличающиеся способом передачи этих 2х чисел в функцию sumIntegers.
     * Как минимум одна из реализаций должна использовать тип данных (класс) написанный вами самостоятельно.
     */ 
    def sum1(x: Int, y: Int): Int = sumIntegers(Seq(x, y))
    
    /* Класс для второй ф-ии: */
    class sequence_creator {
      def seq_maker(a: Int, b: Int)= {
        Array(a, b).toIterable
      }
    }
    /* И экземпляр этого класса: */
    val seq: sequence_creator = new sequence_creator()
    def sum2(x: Int, y: Int): Int = sumIntegers(seq.seq_maker(x, y))
    
    /* С анонимной функцией: */
    var convert_to_Iterable = (x: Int, y: Int) => List(x, y)
    def sum3(x: Int, y: Int): Int = sumIntegers(convert_to_Iterable(x, y))
}

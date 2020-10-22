package homework_3

import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_example - {
            val trueStr = "правда"
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
            val false_str = "ложь"
            assert(Exercises.prettyBooleanFormatter1(false) == false_str)
            assert(Exercises.prettyBooleanFormatter1(123456789) == "123456789")
            assert(Exercises.prettyBooleanFormatter1(List(123456789)) == "List(123456789)")
            assert(Exercises.prettyBooleanFormatter1(156.78) == "156.78")
            assert(Exercises.prettyBooleanFormatter1((123, 456, 789)) == "(123,456,789)")
        }

        'test_prettyBooleanFormatter2 - {
            val trueStr = "правда"
            assert(Exercises.prettyBooleanFormatter2(true) == trueStr)
            val false_str = "ложь"
            assert(Exercises.prettyBooleanFormatter2(false) == false_str)
            assert(Exercises.prettyBooleanFormatter2(987654321) == "987654321")
            assert(Exercises.prettyBooleanFormatter2(List(6, 6, 6)) == "List(6, 6, 6)")
            assert(Exercises.prettyBooleanFormatter2(104.45) == "104.45")
            assert(Exercises.prettyBooleanFormatter2(("Hello", "world", "!")) == "(Hello,world,!)")
        }

        'test_prettyBooleanFormatter3 - {
            val trueStr = "правда"
            assert(Exercises.prettyBooleanFormatter3(true) == trueStr)
            val false_str = "ложь"
            assert(Exercises.prettyBooleanFormatter3(false) == false_str)
            assert(Exercises.prettyBooleanFormatter3(999) == "999")
            assert(Exercises.prettyBooleanFormatter3(List("01", "02")) == "List(01, 02)")
            assert(Exercises.prettyBooleanFormatter3(104.45) == "104.45")
            assert(Exercises.prettyBooleanFormatter3((12.3, 45.6, 78.9)) == "(12.3,45.6,78.9)")
        }

        'test_max1 - {
            assert(Exercises.max1(List(400, 250, 800, 30)) == 800)
            assert(Exercises.max1(List()) == 0)
            assert(Exercises.max1(Seq()) == 0)
            assert(Exercises.max1(Seq(5, 5, 5)) == 5)
        }

        'test_max2 - {
            assert(Exercises.max2(List(400, 250, 800, 30)) == List(800))
            assert(Exercises.max2(Seq()) == Seq())
            assert(Exercises.max2(List()) == List())
            assert(Exercises.max2(Seq(5, 5, 5)) == Seq(5))
        }

        'test_max3 - {
            assert(Exercises.max3(List(400, 250, 800, 30)) == Some(800))
            assert(Exercises.max3(Seq()) == None)
            assert(Exercises.max3(List()) == None)
            assert(Exercises.max3(Seq(5, 5, 5)) == Some(5))
        }

        'test_sum1 - {
            assert(Exercises.sum1(999, 1) == 1000)
            assert(Exercises.sum1(0, 0) == 0)
            assert(Exercises.sum1("45".toInt, "45".toInt) == 90)
            val Array(x, y) = "10-2020".split("-").map(_.toInt)
            assert(Exercises.sum1(x, y) == 2030)
        }

        'test_sum2 - {
            assert(Exercises.sum2(1, 999) == 1000)
            assert(Exercises.sum2(0, 567) == 567)
            assert(Exercises.sum2("79".toInt, "21".toInt) == 100)
            val Array(x, y) = "10-2020".split("-").map(_.toInt)
            assert(Exercises.sum2(x, y) == 2030)
        }

        'test_sum3 - {
            assert(Exercises.sum3(555, 555) == 1110)
            assert(Exercises.sum3(7000, 0) == 7000)
            assert(Exercises.sum3("0".toInt, "146".toInt) == 146)
            val Array(x, y) = "10-2020".split("-").map(_.toInt)
            assert(Exercises.sum3(x, y) == 2030)
        }
    }
}

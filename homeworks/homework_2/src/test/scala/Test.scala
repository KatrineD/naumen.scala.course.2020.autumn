import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(3, 30) == 225)
            assert(Exercises.sumOfDivBy3Or5(0, 1) == 0)
            assert(Exercises.sumOfDivBy3Or5(1, 99) == 2318)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(88) == Seq(2, 11))
            assert(Exercises.primeFactor(127) == Seq(127))
            assert(Exercises.primeFactor(30030) == Seq(2, 3, 5, 7, 11, 13))
            assert(Exercises.primeFactor(96721) == Seq(311))
        }

        'test_sumScalars - {
            val vec1 = new Exercises.Vector2D(1.0, 5.0)
            val vec2 = new Exercises.Vector2D(-2.0, 8.0)
            val vec3 = new Exercises.Vector2D(-1.0, -5.0)
            val vec4 = new Exercises.Vector2D(2.0, -8.0)
            assert(Exercises.sumScalars(vec1, vec2, vec3, vec4) == 76.0)

            val vec5 = new Exercises.Vector2D(0.0, -100.0)
            val vec6 = new Exercises.Vector2D(0.0, 100.0)
            val vec7 = new Exercises.Vector2D(-50.0, 50.0)
            val vec8 = new Exercises.Vector2D(50.0, -50.0)
            assert(Exercises.sumScalars(vec5, vec6, vec7, vec8) == -15000.0)
        }

        'test_sumCosines - {
            val vec9 = new Exercises.Vector2D(0.0, 10.0)
            val vec10 = new Exercises.Vector2D(0.0, -10.0)
            val vec11 = new Exercises.Vector2D(0.0, 5.0)
            val vec12 = new Exercises.Vector2D(5.0, 0.0)
            assert(Exercises.sumCosines(vec9, vec10, vec11, vec12) == -1)

            val vec13 = new Exercises.Vector2D(-7.0, 7.0)
            val vec14 = new Exercises.Vector2D(7.0, -7.0)
            val vec15 = new Exercises.Vector2D(3.0, 3.0)
            val vec16 = new Exercises.Vector2D(-3.0, -3.0)
            assert(Exercises.sumCosines(vec13, vec14, vec15, vec16) == -2)
        }

        'test_sortByHeavyweight - {
            val alkaline_balls: Map[String, (Int, Double)] =
                Map(
                    "Sodium" ->    (18,   0.971), "Lithium" ->  (4,  0.534), "Potassium" -> (1,  0.862), "Francium" -> (3,   2.3),
                    "Rubidium" ->   (4,   19.32), "Cesium"  ->  (4, 1.873)
                )

            assert(Exercises.sortByHeavyweight(alkaline_balls) == Seq("Potassium", "Lithium", "Francium", "Cesium", "Rubidium", "Sodium"))

            val radioactive_balls: Map[String, (Int, Double)] =
                Map(
                    "Technetium" ->    (5,   11.5), "Uranium" ->   (8,   19.04), "Plutonium" -> (20,   19.25), "Polonium" -> (3,   9.136),
                    "Neptunium" ->   (6,   20.4), "Promethium"  ->  (12, 7.22)
                )

            assert(Exercises.sortByHeavyweight(radioactive_balls) == Seq("Polonium", "Technetium", "Neptunium", "Uranium", "Promethium", "Plutonium"))
        }
    }
}


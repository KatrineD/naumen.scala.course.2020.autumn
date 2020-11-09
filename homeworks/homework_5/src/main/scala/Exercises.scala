object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal



  case class Shelter[+T <: Animal](animal_list: List[T]) {
    def +[B >: T <: Animal](animal: B): Shelter[B] = {
      val new_shelter = new Shelter[B](animal :: animal_list)
      new_shelter
    }

    def ++[B >: T <: Animal](another_shelter: Shelter[B]): Shelter[B] = {
      val new_combined_shelter = new Shelter[B](another_shelter.animal_list ::: animal_list)
      new_combined_shelter
    }

    def getNames: List[String] = {
      val animals_names = for (animal <- animal_list) yield animal.name
      animals_names
    }

    def feed(meal: Food[T]): List[String] = {
      val feed_animals = for (animal <- animal_list) yield meal.feed(animal)
      feed_animals
    }
  }


  trait Food [-T <: Animal]{
    val meal: String
    def feed(animal: T): String ={
      (animal.name).concat(" eats ").concat(meal.toString.toLowerCase)
    }
  }

  case object Meat extends Food[Animal] {
    override val meal: String = "meat"
  }

  case object Milk extends Food[Cat] {
    override val meal: String = "milk"
  }

  case object Bread extends Food[Dog] {
    override val meal: String = "bread"
  }


}

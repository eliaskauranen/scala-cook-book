import java.io.{BufferedReader, FileReader}
import scala.collection.mutable.Buffer

object Testing2 extends App {

  val storeX = new Store()
  val userSettings = new Settings
  val happyUser = new User("Happy", storeX, userSettings)
  val recipeBook = new CookBook(happyUser)

    List(
    ("Aleppo pepper", 0.41),
    ("baking powder", 0.97),
    ("basil", 0.1),
    ("butter", 0.9),
    ("cinnamon", 0.53),
    ("cumin", 0.41),
    ("curry", 0.43),
    ("chili powder", 0.54),
    ("cilantro", 1.04),
    ("coriander", 1.04),
    ("corned beef", 1.0),
    ("flour", 0.593),
    ("garlic powder", 0.66),
    ("ginger", 0.41),
    ("nutmeg", 0.47),
    ("onions", 0.95),
    ("oregano", 0.2),
    ("paprika powder", 0.46),
    ("pepper", 0.47),
    ("raw cashews", 0.51),
    ("rosemary", 0.11),
    ("salt", 2.17),
    ("sugar",0.85),
    ("thyme", 0.16)
    ).foreach(UnitConverter.densityList += _)


  //Test1

  val fReader = new FileReader("VeganSourCream.txt")
  val bReader = new BufferedReader(fReader)
  val res: Seq[String] = RecipeReader.fullRecipe(bReader).filter(_.nonEmpty)
  fReader.close()
  bReader.close()

  val resultBuffer = recipeBook.readIngredients(res.dropWhile(!_.contains("Ingredients")))._1
  val resultTest1 = resultBuffer
  val expectedTest1 = Buffer(
    ("raw cashews", 1.0, "cup"),
    ("water", 0.5, "cup"),
    ("lemon juice", 1.0, "tablespoon"),
    ("apple cider vinegar", 1.0, "teaspoon"),
    ("salt", 0.25, "teaspoon"),
    ("dijon mustard", 0.25, "teaspoon"))
  val testPassed1 = resultTest1 == expectedTest1
  println("TEST 1:")
  println(resultTest1)
  println(expectedTest1)
  println("Was expected: " + testPassed1)

  //Test2

  val resultTest2 = UnitConverter.convert(resultBuffer.toList)
  //Test1 units should be converted right
  val expectedTest2 = List(
    ("raw cashews", 0.51*1.0*100.0*2.4, "grams"),
    ("water", 0.5*2.4, "deciliters"),
    ("lemon juice", 1.0*0.148, "deciliters"),
    ("apple cider vinegar", 1.0*0.0493, "deciliters"),
    ("salt", 2.6745249999999996, "grams"),
    ("dijon mustard", 0.25*0.0493, "deciliters"))
  val testPassed2 = resultTest2 == expectedTest2
  println("TEST 2:")
  println(resultTest2)
  println(expectedTest2)
  println("Was expected: " + testPassed2)

  //Test3

  recipeBook.addRecipe(RecipeReader.readFile("BlueberryMuffins.txt"))
  recipeBook.addRecipe(RecipeReader.readFile("Coleslaw.txt"))
  recipeBook.addRecipe(RecipeReader.readFile("CreamedCabbage.txt"))
  recipeBook.addRecipe(RecipeReader.readFile("IrishTacos.txt"))
  recipeBook.addRecipe(RecipeReader.readFile("Shakshuka.txt"))
  recipeBook.addRecipe(RecipeReader.readFile("BroccoliCurry.txt"))
  recipeBook.addRecipe(RecipeReader.readFile("MushroomLasagna.txt"))
  recipeBook.addRecipe(RecipeReader.readFile("MushroomSoup.txt"))
  recipeBook.addRecipe(RecipeReader.readFile("PeaSoup.txt"))
  recipeBook.addRecipe(RecipeReader.readFile("VeganSourCream.txt"))
  recipeBook.addRecipe(RecipeReader.readFile("VeganTaquitos.txt"))

  val expectedTest3 = Buffer(
    "raw cashews",
    "water",
    "lemon juice",
    "apple cider vinegar",
    "salt",
    "dijon mustard").sorted
  val resultTest3 = storeX.ingredients.map(_.name).filter(expectedTest3.contains(_)).sorted
  val testPassed3 = resultTest3 == expectedTest3
  println("TEST 3:")
  println(resultTest3)
  println(expectedTest3)
  println("Was expected: " + testPassed3)

  //Test4

  val resultTest4 = recipeBook.recipesTest.map(a => (a._1, a._2.map(_.name))).filter(b => b._1 == "lunch" || b._1 == "dinner" || b._1 == "vegan" || b._1 == "vegetarian")
  val expectedTest4 = Map(
    "lunch" -> Buffer("Creamed Cabbage", "Shakshuka", "Broccoli Curry", "Mushroom Lasagna", "Mushroom Soup", "Pea Soup", "Vegan Taquitos"),
    "vegetarian" -> Buffer("Broccoli Curry", "Mushroom Lasagna", "Mushroom Soup", "Pea Soup", "Sour Cream", "Vegan Taquitos"),
    "dinner" -> Buffer("Creamed Cabbage", "Shakshuka", "Broccoli Curry", "Mushroom Lasagna", "Mushroom Soup", "Pea Soup", "Vegan Taquitos"),
    "vegan" -> Buffer("Broccoli Curry", "Mushroom Soup", "Pea Soup", "Sour Cream", "Vegan Taquitos"))
  val testPassed4 = resultTest4 == expectedTest4
  println("TEST 4:")
  println(resultTest4)
  println(expectedTest4)
  println("Was expected: " + testPassed4)

  //Test5

  val inStore = storeX.ingredients.map(_.name)
  //All ingredients should be in store only once. For example there should not be two or more ingredients named potato
  val testPassed5 = inStore.forall(a => inStore.indexOf(a) == (inStore.size -1 -inStore.reverse.indexOf(a)))
  println("TEST 5:")
  println("Was expected: " + testPassed5)

  //Test6

  List(
    ("raw cashews", 0.51*1.0*100.0*2.4, "grams"),
    ("water", 0.5*2.4, "deciliters"),
    ("lemon juice", 1.0*0.148, "deciliters"),
    ("apple cider vinegar", 1.0*0.0493, "deciliters"),
    ("salt", 2.17*0.25*0.0493*100.0, "grams"),
    ("dijon mustard", 0.25*0.0493, "deciliters")
  ).foreach(a => storeX.increaseAmount(a._1, 10.0 * a._2))

  val resultTest6 = recipeBook.recipesTest.get("sour cream").head.filter(_.name == "Sour Cream").head.canBeCooked(recipeBook, 2.0)
  //Store should have enough ingredients for 10 servings of Vegan Sour Cream, can be cooked 5 times for 2 servings
  val expectedTest6 = 5
  val testPassed6 = resultTest6 == expectedTest6
  println("TEST 6:")
  println(resultTest6)
  println(expectedTest6)
  println("Was expected: " + testPassed6)

  //Test 7

  storeX.decreaseAmount("raw cashews", 4*0.51*1.0*100.0*2.4)

  val resultTest7 = recipeBook.recipesTest.get("sour cream").head.filter(_.name == "Sour Cream").head.canBeCooked(recipeBook, 2.0)
  //Store should have raw cashews enough for 6 servings of Vegan Sour Cream, other ingredients enough for 10 servings, can be cooked 3 times for 2 servings
  val expectedTest7 = 3
  val testPassed7 = resultTest7 == expectedTest7
  println("TEST 7:")
  println(resultTest7)
  println(expectedTest7)
  println("Was expected: " + testPassed7)


  //Test 8

  storeX.ingredients.foreach(_.amount = 0.0) //Empty store
  happyUser.settings.sort("duration") //Sort recipes by duration

  val resultTest8 = recipeBook.search(List("tortillas", "mushroom"), None, 2.0, "Wide search").map(a => (a._1.name, a._2, a._3))
  //Should return all tortilla and mushroom recipes sort by duration
  val expectedTest8 = List(("Irish Tacos", 25, 0), ("Vegan Taquitos", 35, 0), ("Mushroom Soup", 105, 0), ("Mushroom Lasagna", 125, 0))
  val testPassed8 = resultTest8 == expectedTest8
  println("TEST 8:")
  println(resultTest8)
  println(expectedTest8)
  println("Was expected: " + testPassed8)

  //Test 9

  val resultTest9 = recipeBook.search(List("tortillas", "mushroom", "vegan"), None, 2.0, "Wide search").map(a => (a._1.name, a._2, a._3))
  //Should return all VEGAN tortilla and mushroom recipes sorted by duration
  val expectedTest9 = List(("Vegan Taquitos", 35, 0), ("Mushroom Soup", 105, 0))
  val testPassed9 = resultTest9 == expectedTest9
  println("TEST 9:")
  println(resultTest9)
  println(expectedTest9)
  println("Was expected: " + testPassed9)

  //Test 10

  this.userSettings.addFilter(Buffer("peas"))

  val resultTest10 = recipeBook.search(List("dinner", "vegan"), Some(List("mushrooms")), 2.0, "Wide search").map(a => (a._1.name, a._2, a._3))
  //Should return a list with vegan recipes for dinner without mushrooms and peas, sorted by duration
  val expectedTest10 = List(("Vegan Taquitos", 35, 0), ("Broccoli Curry", 40, 0))
  val testPassed10 = resultTest10 == expectedTest10
  println("TEST 10:")
  println(resultTest10)
  println(expectedTest10)
  println("Was expected: " + testPassed10)

  //Test 11

  this.userSettings.selectedFilters = Buffer() //Clean settings

  //Let's test the accurate search
  //Should return all lunch recipes
  val resultTest11A = recipeBook.search(List("lunch"), None, 2.0, "Accurate search").size
  val expectedTest11A = 7
  //Should return only lunch recipes for soups
  val resultTest11B = recipeBook.search(List("lunch", "soup"), None, 2.0, "Accurate search").size
  val expectedTest11B = 2
  //Should return only lunch recipes for soups which contain mushrooms
  val resultTest11C = recipeBook.search(List("lunch", "soup", "mushrooms"), None, 2.0, "Accurate search").size
  val expectedTest11C = 1

  val testPassed11 = (resultTest11A == expectedTest11A) && (resultTest11B == expectedTest11B) && (resultTest11C == expectedTest11C)

  println("TEST 11:")
  println("result A: " + resultTest11A)
  println("expected A: " + expectedTest11A)
  println("result B: " + resultTest11B)
  println("expected B: " + expectedTest11B)
  println("result C: " + resultTest11C)
  println("expected C: " + expectedTest11C)
  println("Was expected: " + testPassed11)

  //Test 12

  //Let's test the accurate search
  //Should return all lactose free recipes
  val resultTest12A = recipeBook.search(List("lactose free"), None, 2.0, "Accurate search").size
  val expectedTest12A = 6
  //Should return all lactose free recipes which contain olive oil
  val resultTest12B = recipeBook.search(List("lactose free", "olive oil"), None, 2.0, "Accurate search").size
  val expectedTest12B = 3
  //Should return all lactose free recipes which contain both olive oil and baby spinach
  val resultTest12C = recipeBook.search(List("lactose free", "olive oil", "baby spinach"), None, 2.0, "Accurate search").size
  val expectedTest12C = 1

  val testPassed12 = (resultTest12A == expectedTest12A) && (resultTest12B == expectedTest12B) && (resultTest12C == expectedTest12C)

  println("TEST 12:")
  println("result A: " + resultTest12A)
  println("expected A: " + expectedTest12A)
  println("result B: " + resultTest12B)
  println("expected B: " + expectedTest12B)
  println("result C: " + resultTest12C)
  println("expected C: " + expectedTest12C)
  println("Was expected: " + testPassed12)

  //Test 13

  //Returns all recipes
  val resultTest13 = recipeBook.allRecipes.map(_.name).sorted
  val expectedTest13 = Buffer(
    "Blueberry Muffins",
    "Broccoli Curry",
    "Coleslaw",
    "Creamed Cabbage",
    "Irish Tacos",
    "Mushroom Lasagna",
    "Mushroom Soup",
    "Pea Soup",
    "Shakshuka",
    "Sour Cream",
    "Vegan Taquitos"
  )
  val testPassed13 = resultTest13 == expectedTest13
  println("TEST 13:")
  println(resultTest13)
  println(expectedTest13)
  println("Was expected: " + testPassed13)


  //Test 14

  //Returns all recipes which don't contain the allergen nut
  val resultTest14 = recipeBook.search(List(), Some(List("nut")), 2.0, "Wide search").map(_._1.name).sorted
  val expectedTest14 = List(
    "Blueberry Muffins",
    "Coleslaw",
    "Irish Tacos",
    "Mushroom Lasagna",
    "Mushroom Soup",
    "Shakshuka"
  )
  val testPassed14 = resultTest14 == expectedTest14
  println("TEST 14:")
  println(resultTest14)
  println(expectedTest14)
  println("Was expected: " + testPassed14)

  //Test 15
  //Show ingredients with their allergens if they have some
  val resultTest15 = storeX.ingredients.filter(_.allergens.nonEmpty).map(a => (a.name, a.allergens.map(_.name)))
  println("TEST 15:")
  println(resultTest15)

  val allTestsPassed =
    testPassed1 && testPassed2 && testPassed3 && testPassed4 && testPassed5 && testPassed6 && testPassed7 && testPassed8 && testPassed9 && testPassed10 && testPassed11 && testPassed12 && testPassed13 && testPassed14

  println("ALL TESTS PASSED: " + allTestsPassed)
}

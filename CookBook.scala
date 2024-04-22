
import scala.collection.mutable.Buffer

class CookBook(val user: User) {

  private var recipes = Map[String, Buffer[Recipe]]() //Store here every category and all recipes under that category.

  val allRecipes = Buffer[Recipe]() //Store here every recipe of this cookbook.

  def recipesTest = this.recipes //Used for testing

  def readIngredients(res: Seq[String]) = {

    //Get every ingredient with the amount needed in this recipe and the amount's unit and store it here.
    val ingredients: Buffer[(String, Double, String)] = Buffer()
    //If an ingredient has an allergen, store the name of the ingredient with all its allergens as a pair here.
    val allergensOfIngredient: Buffer[(String, List[String])] = Buffer()

    val numbersAsChar = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    val unitList = List[CharSequence]("spoon", "cup", "ounce", "pound", "pint", "gallon", "quart", "gram", "liter", "piece")

    //The ingredients should be listed each one on its own line between the lines "Ingredients" and "Phases".
    //Start at line 1 and do until the line "Phases".
    var integer = 1
    //The sequence has to start with header Ingredients. Otherwise throw exception.
    if(res.head.contains("Ingredients")) {
      while(!(res(integer).contains("Phases"))) {
        var amountNeeded: Double = 0
        val newAmount = res(integer).dropWhile(_ == ' ').split(' ').filter(number => numbersAsChar.exists(number.contains(_)))
        val allergens = res(integer).dropWhile(_ != '(').tail.takeWhile(_ != ')').split(',').map(str => str.dropWhile(_ == ' ')).toList
        val newIngredient = res(integer).takeWhile(_ != '(').split(' ').filterNot(number => numbersAsChar.exists(number.contains(_))).map(_.dropWhile(_ == ' '))
        //newIngredient = Array("unit", "ingredient", "name", "max", "6", "words", "long").length == 7
        if(newAmount.isEmpty || newIngredient.isEmpty || newIngredient.length > 7) {
          throw new Exception("Error reading ingredients1")
        } else if(newIngredient.nonEmpty) {
          if(newAmount.length == 2) { // for example 1 1/4
            amountNeeded = (newAmount(0).toDouble * newAmount(1).split('/')(1).toDouble + newAmount(1).split('/').head.toDouble) / newAmount(1).split('/')(1).toDouble
          } else if(newAmount.length == 1 && newAmount.head.contains('/')) { // for example 1/2
            amountNeeded = newAmount(0).split('/').head.toDouble / newAmount(0).split('/')(1).toDouble
          } else { // for example 1 or 1.25
            amountNeeded = newAmount(0).toDouble
          }
          var unit = "unitless"
          var ingredientName = "nameless"
          if(newIngredient.length == 1) {
            unit = "piece"
            ingredientName = newIngredient.head
          } else if(!(unitList.exists(newIngredient(0).contains(_)))) {
            unit = "piece"
            ingredientName = newIngredient.mkString(" ")
          } else{
            unit = newIngredient.head
            ingredientName = newIngredient.tail.mkString(" ")
          }
          //Add the information.
          //Get rid of possible white space in front of the name and make sure the name is completely in lower case.
          ingredients += ((ingredientName.dropWhile(_ == ' ').toLowerCase, amountNeeded, unit))
          allergensOfIngredient += ((ingredientName.dropWhile(_ == ' ').toLowerCase, allergens))
        } else {
          throw new Exception("Error reading ingredients2")
        }
        //Go to the next line.
        integer += 1
      }
    } else {
      throw new Exception("Error reading ingredients3")
    }
    (ingredients, allergensOfIngredient)
  }



  def addRecipe(recipeText: Seq[String]) = {
    val res = recipeText
    val numbersAsChar = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    //These information will be collected from the file to create a new Recipe.
    var ingredients: Buffer[(String, Double, String)] = Buffer()
    var recipeName = "Nameless"
    var recipeCategories = Buffer[String]()
    var recipeTime: Int = 0
    var servings: Int = 0
    //The amount the ready food equals approximately for one serving and the unit. Needed for subrecipes.
    //For example you make a smoothie for 5 portions and then you have let's say 5 deciliters of smoothie. Then one serving of the recipe equals 1 deciliters.
    var oneServing: Option[(Double, String)] = None
    //Let's make sure the text file has a correct structure. If not, an exception is thrown.
    val terms = List[Boolean](
      res(0).contains("Name"),
      res(2).contains("Categories"),
      res(4).contains("Time"),
      res(6).contains("Servings"),
      res(8).contains("Ingredients"),
      res.contains("Phases:") && (res.indexOf("Phases:") > (res.indexOf("Ingredients") +1)))
    if(!terms.forall(_ == true)){
      throw new Exception("The structure of the text file is incorrect")
    }
    //Gets the name and the categories.
    recipeName = res(1)
    res(3).split(',').map(_.dropWhile(_==' ')).map(_.toLowerCase).foreach(recipeCategories += _)
    //Gets time or throws exception.
    if(res(5).filter(number => numbersAsChar.contains(number)).nonEmpty) {
      recipeTime = res(5).filter(number => numbersAsChar.contains(number) || number == '.').toInt
    } else {
      throw new Exception("Time not found")
    }
    //Gets servings and the amount of one serving or throws exception.
    if(res(7).takeWhile(_ != '(').filter(number => numbersAsChar.contains(number)).nonEmpty) {
      servings = res(7).takeWhile(_ != '(').filter(number => numbersAsChar.contains(number) || number == '.').toInt
      val optionRes = res(7).dropWhile(_ !='(').tail.takeWhile(_ != ')')
      if(optionRes.filter(number => numbersAsChar.contains(number)).nonEmpty) {
        //the amount this recipe creates / servings = amount of one serving
        val optionDoub = optionRes.filter(number => numbersAsChar.contains(number) || number == '.').toDouble / servings.toDouble
        val optionUnit = optionRes.filterNot(number => numbersAsChar.contains(number) || number == '.' || number == ' ')
        //Convert the amount so that the unit is grams, desiliters or piece.
        val onePortionAmount = UnitConverter.convert(List(("order66", optionDoub, optionUnit)))
        if(onePortionAmount.head._3 != "grams" && onePortionAmount.head._3 != "deciliters" && onePortionAmount.head._3 != "piece") {
          throw new Exception("Incorrect structure inside the brackets after servings")
        } else {
          oneServing = Some((onePortionAmount.head._2, onePortionAmount.head._3))
        }
      }
    } else {
      throw new Exception("Servings not found")
    }
    //Gets the ingredients or throws exception.
    val ingredientText = res.dropWhile(!_.contains("Ingredients"))
    ingredients = this.readIngredients(ingredientText)._1
    val validUnits = UnitConverter.convert(ingredients.toList)
    //Gets the allergens.
    val allergens = this.readIngredients(ingredientText)._2.toMap
    //Creates a new recipe...
    for(ing <- validUnits) {
      //Creates a place in the store for an ingredient if there isn't one yet.
      if(!(this.user.store.ingredients.map(_.name).contains(ing._1))) {
            this.user.store.addIngredient(ing._1, 0.0, ing._3)
      }
      this.user.store.ingredients.find(_.name == ing._1) match {
        case Some(i) => {
          //If there's a new allergen for an ingredient, it's added to the ingredient's val allergens.
          for(allergen <- allergens(ing._1)) {
            if(!i.allergens.map(_.name).contains(allergen) && allergen != "") {
              i.allergens += new Allergen(allergen)
            }
          }
        }
        case None =>
      }
    }
    val listForRecipe = validUnits.map(a => (this.user.store.ingredients.dropWhile(_.name != a._1).head, a._2, a._3))
    val happyRecipe = new Recipe(listForRecipe, recipeName, recipeCategories.toList, servings, res, recipeTime, oneServing)
    //...and adds it to val allRecipes and var recipes if there is not a recipe with the same name.
    if(allRecipes.map(_.name).contains(recipeName)) {
      throw new Exception("Recipe exists," + recipeName)
    } else {
      allRecipes += happyRecipe
      for(category <- recipeCategories){
        if(!recipes.contains(category)) {
          recipes += category -> Buffer(happyRecipe)
        } else {
          recipes.map(pair => if(pair._1 == category) pair._1 -> (pair._2 += happyRecipe) else pair)
        }
      }
    }

  }

  def deleteRecipe(name: String) = {
    if(allRecipes.map(_.name).contains(name)) {
      allRecipes -= allRecipes.find(_.name == name).head
      recipes = recipes.map(r => (r._1, r._2.filterNot(_.name == name))).toMap
    }

  }


  def search(keywords: List[String], filter: Option[List[String]], portions: Double, searchType: String): List[(Recipe, Int, Int)] = {
    require(portions >= 1 && portions <= 12 && (searchType == "Wide search" || searchType == "Accurate search"))

    var recipesIn = this.recipes

    val allCategories = this.recipes.keys.toList

    val specialDiets = List("vegan", "vegetarian", "lactose free", "gluten free")

    //recipesIn should contain only recipes of chosen special diets
    for(diet <- keywords.filter(specialDiets.contains(_))) {
      recipesIn = Diet.filter(diet, recipesIn)
    }

    val categoryKeys: List[String] = keywords.filter(recipesIn.contains(_)).filterNot(specialDiets.contains(_))

    val ingredientKeys: List[String] = keywords.filterNot(k => recipesIn.contains(k) || specialDiets.contains(k))

    var recipesOut: List[(Recipe, Int, Int)] = List()

    filter match {
      case Some(avoid) => {
        val avoidThese = avoid.filterNot(this.user.settings.selectedFilters.contains(_)) ++ this.user.settings.selectedFilters.toList
        if(keywords.isEmpty || keywords.forall(specialDiets.contains(_))) {
          recipesIn.flatMap(_._2).foreach(recipe =>
            //List("a", "b").diff(List("c", "d")) = List("a", "b") and List("a", "b").diff(List("b", "c")) = List("a")
            //If the recipe has neither ingredients nor allegens which are mentioned in val avoidThese, avoidThese.diff(ingredients + allergens) = avoidThese so the recipe is good.
            if(avoidThese.diff((recipe.ingredients.map(_._1.name) ++ recipe.ingredients.map(_._1.name).flatMap(_.split(" ")) ++ recipe.ingredients.flatMap(_._1.allergens).map(_.name))) == avoidThese && !recipesOut.map(_._1.name).contains(recipe.name))
              recipesOut = recipesOut :+ (recipe, recipe.duration, recipe.canBeCooked(this, portions)))
        } else if(categoryKeys.nonEmpty) {
          //for every category in keywords
          for(key <- categoryKeys) {
            recipesIn.get(key).foreach(recipesListed => recipesListed.foreach(recipe =>
              if(avoidThese.diff((recipe.ingredients.map(_._1.name) ++ recipe.ingredients.map(_._1.name).flatMap(_.split(" ")) ++ recipe.ingredients.flatMap(_._1.allergens).map(_.name))) == avoidThese && !recipesOut.map(_._1.name).contains(recipe.name))
                recipesOut = recipesOut :+ (recipe, recipe.duration, recipe.canBeCooked(this, portions))))
          }
          //for every ingredient or recipe name in keywords
          //Don't do the categories again, connect the recipes of other categories and get the recipes that has one keyword as ingredient or name and doesn't conflickt with avoided ones.
          recipesIn.filterNot(categoryKeys.contains(_)).flatMap(_._2).filter(r => keywords.exists(k => r.ingredients.map(_._1.name).contains(k) || k == r.name.toLowerCase)).foreach(recipe =>
              if(avoidThese.diff((recipe.ingredients.map(_._1.name) ++ recipe.ingredients.map(_._1.name).flatMap(_.split(" ")) ++ recipe.ingredients.flatMap(_._1.allergens).map(_.name))) == avoidThese && !recipesOut.map(_._1.name).contains(recipe.name))
                recipesOut = recipesOut :+ (recipe, recipe.duration, recipe.canBeCooked(this, portions)))
        } else {
          recipesIn.filterNot(categoryKeys.contains(_)).flatMap(_._2).filter(r => keywords.exists(k => r.ingredients.map(_._1.name).contains(k) || k == r.name.toLowerCase)).foreach(recipe =>
              if(avoidThese.diff((recipe.ingredients.map(_._1.name) ++ recipe.ingredients.map(_._1.name).flatMap(_.split(" ")) ++ recipe.ingredients.flatMap(_._1.allergens).map(_.name))) == avoidThese && !recipesOut.map(_._1.name).contains(recipe.name))
                recipesOut = recipesOut :+ (recipe, recipe.duration, recipe.canBeCooked(this, portions)))
        }

        //If avoidThese contains a category name, recipesOut shouldn't contain any recipe under that category.
        for(cat <- avoidThese.filter(allCategories.contains(_))) {
          this.recipes.get(cat) match {
            case Some(buf) => recipesOut = recipesOut.filterNot(rec => buf.map(_.name).contains(rec._1.name))
            case None =>
          }
        }

        if(searchType == "Accurate search") {
          recipesOut = recipesOut.filter(hasAll =>
            categoryKeys.forall(recipesIn.get(_).head.contains(hasAll._1)) &&
            ingredientKeys.forall((hasAll._1.ingredients.map(_._1.name) ++ hasAll._1.ingredients.map(_._1.name).flatMap(_.split(" "))).contains(_))
          )
        }

        if(this.user.settings.sortBy == "duration") {
          recipesOut.sortBy(_._2)
        } else {
          recipesOut.sortBy(_._3).reverse
        }
      }
      case None => {
        val avoidThese = this.user.settings.selectedFilters.toList
        if(keywords.isEmpty || keywords.forall(specialDiets.contains(_))) {
          recipesIn.flatMap(_._2).foreach(recipe =>
            if(avoidThese.diff((recipe.ingredients.map(_._1.name) ++ recipe.ingredients.map(_._1.name).flatMap(_.split(" ")) ++ recipe.ingredients.flatMap(_._1.allergens).map(_.name))) == avoidThese && !recipesOut.map(_._1.name).contains(recipe.name))
              recipesOut = recipesOut :+ (recipe, recipe.duration, recipe.canBeCooked(this, portions)))
        } else if(categoryKeys.nonEmpty) {
          for(key <- categoryKeys) {
            recipesIn.get(key).foreach(recipesListed => recipesListed.foreach(recipe =>
              if(avoidThese.diff((recipe.ingredients.map(_._1.name) ++ recipe.ingredients.map(_._1.name).flatMap(_.split(" ")) ++ recipe.ingredients.flatMap(_._1.allergens).map(_.name))) == avoidThese && !recipesOut.map(_._1.name).contains(recipe.name))
                recipesOut = recipesOut :+ (recipe, recipe.duration, recipe.canBeCooked(this, portions))))
          }
          recipesIn.filterNot(categoryKeys.contains(_)).flatMap(_._2).filter(r => keywords.exists(k => r.ingredients.map(_._1.name).contains(k) || k == r.name.toLowerCase)).foreach(recipe =>
              if(avoidThese.diff((recipe.ingredients.map(_._1.name) ++ recipe.ingredients.map(_._1.name).flatMap(_.split(" ")) ++ recipe.ingredients.flatMap(_._1.allergens).map(_.name))) == avoidThese && !recipesOut.map(_._1.name).contains(recipe.name))
                recipesOut = recipesOut :+ (recipe, recipe.duration, recipe.canBeCooked(this, portions)))
        } else {
          recipesIn.filterNot(categoryKeys.contains(_)).flatMap(_._2).filter(r => keywords.exists(k => r.ingredients.map(_._1.name).contains(k) || k == r.name.toLowerCase)).foreach(recipe =>
              if(avoidThese.diff((recipe.ingredients.map(_._1.name) ++ recipe.ingredients.map(_._1.name).flatMap(_.split(" ")) ++ recipe.ingredients.flatMap(_._1.allergens).map(_.name))) == avoidThese && !recipesOut.map(_._1.name).contains(recipe.name))
                recipesOut = recipesOut :+ (recipe, recipe.duration, recipe.canBeCooked(this, portions)))
        }

        for(cat <- avoidThese.filter(allCategories.contains(_))) {
          this.recipes.get(cat) match {
            case Some(buf) => recipesOut = recipesOut.filterNot(rec => buf.map(_.name).contains(rec._1.name))
            case None =>
          }
        }

        if(searchType == "Accurate search") {
          recipesOut = recipesOut.filter(hasAll =>
            categoryKeys.forall(recipesIn.get(_).head.contains(hasAll._1)) &&
            ingredientKeys.forall((hasAll._1.ingredients.map(_._1.name) ++ hasAll._1.ingredients.map(_._1.name).flatMap(_.split(" "))).contains(_))
          )
        }

        if(this.user.settings.sortBy == "duration") {
          recipesOut.sortBy(_._2)
        } else {
          recipesOut.sortBy(_._3).reverse
        }
      }
    }
  }


}

import scala.collection.mutable.Buffer

//An ingredient has a name, the amount of that ingredient in store and one of three basic units used for storing
class Ingredient(val name: String, var amount: Double, val unit: String) {
  require(unit == "piece" || unit == "grams" || unit == "deciliters")
  val allergens = Buffer[Allergen]()
}

class Allergen(val name: String)



class Recipe(val ingredients: List[(Ingredient, Double, String)], val name: String, var categories: List[String], servings: Int, recipe: Seq[String], val duration: Int, val onePortion: Option[(Double, String)]) {

  //Some ingredients can be made of their own recipes.
  //Those subrecipes are only used in a main recipe if there is not enough of that ingredient in store but the subrecipe can be made.
  private var subRecipes = Buffer[(Recipe, Double)]()

  //How many times can this recipe be cooked for a chosen amount of portions?
  def canBeCooked(book: CookBook, portions: Double): Int = {
    this.subRecipes = Buffer[(Recipe, Double)]()
    //val ingredients store the amount needed for the servings of the original recipe, so one portion needs amount/servings of one ingredient
    val missingIngredients = ingredients.filter(m => m._1.amount - (m._2 * portions / servings.toDouble) < 0)
    //Does the missing ingredients have a subrecipe which could be used and how much do we need to cook it?
    val hasOwnRecipe = book.allRecipes.filter(a => missingIngredients.map(_._1.name.toLowerCase).contains(a.name.toLowerCase)).map(
      rec => (rec, ingredients.find(_._1.name.toLowerCase == rec.name.toLowerCase).head._2, ingredients.find(_._1.name.toLowerCase == rec.name.toLowerCase).head._3))
    if(hasOwnRecipe.nonEmpty) {
      var updated: List[(Ingredient, Double, String)] = ingredients
      for(rec <- hasOwnRecipe) {
        //How much do we have of the missing ingredient? We use everything we have of that and make more with a subrecipe only the amount that's missing if that's even possible.
        val exists = ingredients.filter(_._1.name.toLowerCase == rec._1.name.toLowerCase).head._1.amount
        if(rec._3 == "unit" && rec._1.canBeCooked(book, rec._2 - exists) > 0) {
          updated = updated.filterNot(_._1.name.toLowerCase == rec._1.name.toLowerCase) ++ rec._1.ingredients
          //the subrecipes used and its amount needed will be stored so that method showRecipe is able to show the subrecipe as part of the main recipe
          this.subRecipes += ((rec._1, rec._2 - exists))
        } else if(rec._1.onePortion.nonEmpty && rec._3 == rec._1.onePortion.head._2 && rec._1.canBeCooked(book, ((rec._2 * portions / servings - exists) / rec._1.onePortion.head._1)) > 0) {
          updated = updated.filterNot(_._1.name.toLowerCase == rec._1.name.toLowerCase) ++ rec._1.ingredients
          this.subRecipes += ((rec._1, (rec._2 * portions / servings - exists) / rec._1.onePortion.head._1))
        }
      }
      //Inside the map: Ingredient in store / Ingredient needed for chosen amount of portions = How many times this recipe can be cooked because of this ingredient
      updated.map(t => t._1.amount * servings.toDouble / (t._2 * portions)).min.toInt
    } else {
      ingredients.map(t => t._1.amount * servings.toDouble / (t._2 * portions)).min.toInt
    }
  }

  //Foreach ingredients not enough in store for x portions, tell how much is missing
  def howMuchMissing(portions: Double): String = {
    //Is missing if amount in store - amount needed is negative
    val info = ingredients.filter(m => m._1.amount - (m._2 * portions / servings.toDouble) < 0).map(m => (m._1, m._2, if(m._3 == "piece") "" else m._3)).map(n =>

        (n._1, ((n._2 * portions / servings.toDouble - n._1.amount) * 1000.0).round / 1000.0, n._3)
    )
    //Show how much missing rounded to 3 decimal places ((x * 1000.0).round / 1000.0), unit if it's not piece and the name of the missing ingredient.
    "Missing: " + info.map(i => (i._2 + " " + i._3 + " " + i._1.name)).mkString(", ") + "."
  }

  //Decrease the amount of ingredients of a cooked recipe by the amount used in that recipe
  def cookRecipe(store: Store, portions: Double): Unit = {
    this.subRecipes.foreach(rec => rec._1.cookRecipe(store, rec._2))
    for(ing <- ingredients) {
     val amountUsed = ing._2 * portions / servings.toDouble
     store.decreaseAmount(ing._1.name, amountUsed)
    }

  }


  //Shows the recipe for a chosen amount of portions
  def showRecipe(book: CookBook, portions: Double): String = {
    //Don't show possible extra information inside brackets before phases
    val rezipe = recipe.takeWhile(!_.contains("Phases")).map(_.takeWhile(_!='(')) ++ recipe.dropWhile(!_.contains("Phases"))
    //Do we use subrecipes?
    if(canBeCooked(book, portions) > 0 && this.subRecipes.nonEmpty) {
      //Update servings of the original recipe
      val part1 = rezipe.reverse.dropWhile(!_.contains("Servings")).reverse.mkString("\n") + "\n" + portions.toInt + "\n" + "Ingredients:"
      //Use class CookBook's method readIngredients to show the same units of the original recipe...
      val ingText = recipe.dropWhile(!_.contains("Ingredients"))
      val part2 = book.readIngredients(ingText)._1.map(m => (m._1, m._2, if(m._3 == "piece") "" else m._3)).map(ing =>
        //...but update the amount needed. Calculates the right amount for the chosen amount of portions rounded to 3 decimal places.
        (ing._2 * 1000.0 * portions / servings).round / 1000.0+ " " + ing._3 + " " + ing._1).mkString("\n")
      //Calls method showRecipe for the subrecipes and takes the ingredient part
      val part3 = this.subRecipes.map(rec => (rec._1.name , rec._1.showRecipe(book, rec._2).split("\n").dropWhile(!_.contains("Ingredients")).takeWhile(!_.contains("Phases")).toBuffer))
      part3.foreach(str => str._2(0) = str._2.head + " (" + str._1 + ")")
      //Calls method showRecipe for the subrecipes and takes the phases part
      val part4 = this.subRecipes.map(rec => (rec._1.name , rec._1.showRecipe(book, rec._2).split("\n").dropWhile(!_.contains("Phases")).toBuffer))
      part4.foreach(str => str._2(0) = str._2.head + " (" + str._1 + ")")
      //The recipe ends with the phases of the main recipe
      val part5 = recipe.dropWhile(!_.contains("Phases")).toBuffer
      part5(0) = part5.head + " (" + name + ")"

      part1 + "\n" + part2 + "\n" + part3.map(_._2.mkString("\n")).mkString("\n") + "\n" + part4.map(_._2.mkString("\n")).mkString("\n") + "\n" + part5.mkString("\n")
    //The original recipe is fine if no subrecipes are used and portions equals servings.
    } else if(portions == servings.toDouble) {
      rezipe.mkString("\n")
    //No subrecipes. Calculates only how much ingredients are needed for a chosen amount of portions.
    } else {
      val part1 = rezipe.reverse.dropWhile(!_.contains("Servings")).reverse.mkString("\n") + "\n" + portions.toInt + "\n" + "Ingredients:"
      val ingText = recipe.dropWhile(!_.contains("Ingredients"))
      val part2 = book.readIngredients(ingText)._1.map(m => (m._1, m._2, if(m._3 == "piece") "" else m._3)).map(ing =>
        (ing._2 * 1000.0 * portions / servings).round / 1000.0+ " " + ing._3 + " " + ing._1).mkString("\n")
      val part3 = recipe.dropWhile(!_.contains("Phases")).mkString("\n")
      part1 + "\n" + part2 + "\n" + part3
    }
  }


}

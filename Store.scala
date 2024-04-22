import scala.collection.mutable.Buffer

class Store() {

  val ingredients: Buffer[Ingredient] = Buffer()

  def addIngredient(name: String, amount: Double, unit: String) = {
    val newIngredient = new Ingredient(name, amount, unit)
    this.ingredients += newIngredient
  }

  def decreaseAmount(ingredientName: String, by: Double) = {
    if(ingredients.map(_.name).contains(ingredientName)) {
      val getIndex = ingredients.map(_.name).indexOf(ingredientName)
      if(ingredients(getIndex).amount < by) {
        ingredients(getIndex).amount = 0.0
      } else {
        ingredients(getIndex).amount -= by
      }
    }
  }

  def increaseAmount(ingredientName: String, by: Double) = {
    val getIndex = ingredients.map(_.name).indexOf(ingredientName)
    ingredients(getIndex).amount += by
  }

  def store(): String = {
    var situation = Buffer[String]()

    this.ingredients.sortBy(_.name).map(ing => (ing.name + " " + ing.amount + " " + ing.unit)).foreach(str => situation += str)

    if(situation.isEmpty) {
      situation += "Store is Empty"
    }

    situation.mkString("\n")

  }

}

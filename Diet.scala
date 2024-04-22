import scala.collection.mutable.Buffer

object Diet {

  def filter(diet: String, recipes: Map[String, Buffer[Recipe]]): Map[String, Buffer[Recipe]] = {
    require(diet == "vegan" || diet == "vegetarian" || diet == "lactose free" || diet == "gluten free")
    //Gets all the recipes under the special diets category
    var sd = recipes.get(diet)
    sd match {
      //For all categories remove the recipes this diet doesn't contain.
      case Some(d) => recipes.toList.map(x => (x._1, x._2.filter(y => d.contains(y)))).toMap
      case None => recipes
    }
  }

}

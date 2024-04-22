import scala.collection.mutable.Buffer

object UnitConverter {

 /**  1 tablespoon = 3 teaspoons
      1 cup = 240 mL = 2.4 dL
      1 tablespoon = 0.148 dL
      1 teaspoon = 4.93 mL = 0.0493 dL
      1 pint = 473 mL = 4.73 dL
      1 quart = 9.46 dL
      1 gallon = 3.785 L = 37.850 dL
      1 milliliter = 0.01 dL
      1 centiliter = 0.1 dL
      1 liter = 10.0 dL
      1 ounce = 28.35 g
      1 pound = 453.6 g
      1 milligram = 0.001 g
      1 kilogram = 1000.0 g
 */
  //converts to dL or gram which are the used with piece as the three amount units in class Store
  //CharSequence because of plurals
  val relationToDL = List[(Double, CharSequence)](
    (2.4, "cup"),
    (0.148, "tablespoon"),
    (0.0493, "teaspoon"),
    (4.73, "pint"),
    (9.46, "quart"),
    (37.85, "gallon"),
    (0.01, "milliliter"),
    (0.1, "centiliter"),
    (10.0, "liter"),
  )
  val relationToG = List[(Double, CharSequence)](
    (28.35, "ounce"),
    (453.6, "pound"),
    (0.001, "milligram"),
    (1000.0, "kilogram")
  )
  val relation = relationToDL ++ relationToG

  //I think I will list only those I use in the example recipes, the user can add more in the program
  //According to www.aqua-calc.com
  val densityList = Buffer[(String, Double)]()


  def convert(rightUnits: List[(String, Double, String)]): List[(String, Double, String)] = {
    rightUnits.map(input =>
      //the name remains the the same
      (input._1,
      //If there is a relation...
      if(relation.map(_._2).exists(input._3.contains(_)) && input._3 != "desiliters") {
        var newValue = 0.0
        for(u <- relation) {
          if(input._3.contains(u._2)){
            //...convert
            newValue = input._2 * u._1
          }
        }
        newValue
      } else {
        input._2
      }
    ,
        //Gets the right unit
        if(relationToDL.map(_._2).exists(input._3.contains(_))) {
          "deciliters"
        } else if(relationToG.map(_._2).exists(input._3.contains(_)) || input._3 == "gram") {
          "grams"
        } else if(input._3 == "pieces") {
          "piece"
        } else {
          input._3
        }
      )).map(grams =>
      //Convert desiliters to grams with the help of density for those ingredients which density is stored in the density list
      (grams._1,
      if(densityList.map(_._1).exists(grams._1.contains(_)) && grams._3 != "grams"){
        var newValue = 0.0
        for(d <- densityList) {
          if(grams._1.toLowerCase.contains(d._1.toLowerCase)){
            //Density in g/cm^3  -> 100 cm^3 = dL -> x g/cm^3 * 100 y dL = z g
            newValue = d._2 * 100.0 * grams._2
          }
        }
        newValue
      } else {
        grams._2
      },
      if(densityList.map(_._1).exists(grams._1.contains(_)) && grams._3 != "grams") {
        "grams"
      } else {
        grams._3
      }
      ))
  }


}

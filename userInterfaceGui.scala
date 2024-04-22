import scala.util.{Try, Success, Failure}
import scala.collection.mutable.Buffer
import scala.swing._

class UI extends MainFrame {
  def restrictHeight(s: Component) {
    s.maximumSize = new Dimension(Short.MaxValue, s.preferredSize.height)
  }

  title = "Smart recipe book"

  private val numbersAsChar = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

  private val densities = RecipeReader.readFile("Densities.txt").map(_.split(";")).filter(seq => seq.length == 2 && seq(1).exists(numbersAsChar.contains(_))).map(toPair => (toPair(0), toPair(1).toDouble))

  private val recipeFiles = RecipeReader.readFile("RecipeFileNames.txt").toBuffer

  private val storeSituation = RecipeReader.readFile("StoreSituation.txt").map(_.split(";")).filter(seq => seq.length == 3 && seq(1).exists(numbersAsChar.contains(_))).map(toTriplet => (toTriplet(0), toTriplet(1).toDouble, toTriplet(2)))

  private val filterAway = RecipeReader.readFile("AvoidedOnes.txt")

  val store = new Store()

  val settings = new Settings()

  val user = new User("Elias", store, settings)

  val recipeBook = new CookBook(user)

  //Loads the density list for the UnitConverter
  densities.foreach(d => UnitConverter.densityList += d)
  //Loads the recipes
  recipeFiles.foreach(fn => recipeBook.addRecipe(RecipeReader.readFile(fn)))
  //Adds all ingredients which are not part of a recipe of this recipe book
  storeSituation.filterNot(in => store.ingredients.map(_.name).contains(in._1)).foreach(ing => store.addIngredient(ing._1, 0.0, ing._3))
  //Loads the current amounts for the ingredients
  storeSituation.foreach(ia => store.increaseAmount(ia._1, ia._2))
  //Loads the settings
  user.settings.addFilter(filterAway.toBuffer)

  //Contents

  val welcome        = new Label("Welcome to your personal recipe book!")
  val want           = new Label("Please list keywords for wanted recipes here:")
  val wanted         = new TextField { columns = 30 }
  val avoid          = new Label("Please list here everything you want to avoid:")
  val avoided        = new TextField { columns = 30 }
  val diet           = new Label("Choose diets if you want:")
  val chooseDiet1    = new ComboBox(List("None", "Vegan", "Vegetarian", "Lactose free", "Gluten free"))
  val chooseDiet2    = new ComboBox(List("None", "Vegan", "Vegetarian", "Lactose free", "Gluten free"))
  val chooseDiet3    = new ComboBox(List("None", "Vegan", "Vegetarian", "Lactose free", "Gluten free"))
  val searchButton   = Button("Search") {
                         searchRecipes(wanted.text, avoided.text)
                       }

  val searchType     = new ComboBox(List("Wide search", "Accurate search"))
  val servings       = new Label("Servings:")
  val portions       = new ComboBox(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  val sortResults    = new Label("Sort results by:")
  val timeOrServings = new ComboBox(List("inStore", "duration"))
  val settingsButton = Button("My settings") {
                         changeSettings()
                       }

  val storeButton    = Button("Store") {
                         showStore()
                       }

  val updateButton   = Button("Update Store") {
                         update()
                       }

  val densityButton  = Button("Densities") {
                         densityUpdate()
                       }

  val turnOutput     = new TextArea(rows=21, columns=80) {
                           editable = false
                           wordWrap = true
                           lineWrap = true
                       }

  val outputArea     = new ScrollPane(turnOutput)
  var lastOutput     = turnOutput.text
  val pick           = new Label("Write a recipe name here:")
  val show           = new TextField { columns = 30 }
  val showButton     = Button("Show Recipe") {
                         showRecipe(show.text)
                       }

  val cookButton     = Button("Cook this recipe") {
                         cookRecipe(show.text)
                       }

  val editButton     = Button("Edit this recipe") {
                         edit(turnOutput.text)
                       }

  val createButton   = Button("Create a recipe") {
                         createRecipe()
                       }

  val addButton      = Button("Add recipe") {
                         newRecipe(turnOutput.text)
                       }

  val deleteButton   = Button("Delete this recipe") {
                         removeRecipe(show.text)
                       }

  val backButton     = Button("Back") {
                         turnOutput.text = lastOutput
                       }

  val closeButton    = Button("Close") {
                         closeWindow()
                       }



  contents = new BoxPanel(Orientation.Vertical) {
    contents += welcome
    contents += Swing.VStrut(10)
    contents += want
    contents += wanted
    contents += Swing.VStrut(5)
    contents += avoid
    contents += avoided
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += diet
      contents += Swing.HStrut(5)
      contents += chooseDiet1
      contents += Swing.HStrut(5)
      contents += chooseDiet2
      contents += Swing.HStrut(5)
      contents += chooseDiet3
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += searchButton
      contents += Swing.HStrut(5)
      contents += searchType
      contents += Swing.HStrut(5)
      contents += servings
      contents += Swing.HStrut(5)
      contents += portions
      contents += Swing.HStrut(5)
      contents += sortResults
      contents += Swing.HStrut(5)
      contents += timeOrServings
      contents += Swing.HStrut(5)
      contents += settingsButton
      contents += Swing.HStrut(5)
      contents += storeButton
      contents += Swing.HStrut(5)
      contents += updateButton
      contents += Swing.HStrut(5)
      contents += densityButton
    }
    contents += Swing.VStrut(5)
    contents += outputArea
    contents += Swing.VStrut(5)
    contents += pick
    contents += Swing.VStrut(5)
    contents += show
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += showButton
      contents += Swing.HStrut(5)
      contents += cookButton
      contents += Swing.HStrut(5)
      contents += editButton
      contents += Swing.HStrut(5)
      contents += createButton
      contents += Swing.HStrut(5)
      contents += addButton
      contents += Swing.HStrut(5)
      contents += deleteButton
      contents += Swing.HStrut(5)
      contents += backButton
    }
    contents += Swing.VStrut(5)
    contents += closeButton

    for (con <- contents)
      con.xLayoutAlignment = 0.0
    border = Swing.EmptyBorder(10, 10, 10, 10)

  }


  def showStore() = {
    turnOutput.editable = false
    lastOutput = turnOutput.text
    turnOutput.text = store.store()
  }


  def changeSettings() {
    val change = Dialog.showInput(contents.head, "List here everything you want to avoid separated with a comma", initial = settings.selectedFilters.sorted.mkString(", "))
    val settingsBefore = settings.selectedFilters
    change match {
      case Some(c) => {
        settings.selectedFilters = c.split(",").map(str => str.dropWhile(_ == ' ')).toBuffer
        if(settingsBefore != settings.selectedFilters) {
          RecipeReader.writeToFile("AvoidedOnes.txt", settings.selectedFilters.sorted.toSeq)
        }
      }
      case None =>
    }
  }


  def searchRecipes(keywords: String, filter: String) = {
    turnOutput.editable = false
    settings.sort(timeOrServings.selection.item)
    val first = ((keywords.split(",").map(_.dropWhile(_==' ')).toList ++ List(chooseDiet1.selection.item, chooseDiet2.selection.item, chooseDiet3.selection.item)).filterNot(k => k.isEmpty || k == "None")).map(_.toLowerCase)
    var second: Option[List[String]] = None
    if(filter.nonEmpty) {
      second = Some((filter.split(",").map(_.dropWhile(_==' ')).toList.filterNot(_.isEmpty)).map(_.toLowerCase))
    }
    val third = portions.selection.item.toDouble

    val fourth =searchType.selection.item

    lastOutput = turnOutput.text

    turnOutput.text = recipeBook.search(first, second, third, fourth).map(re =>
      if(re._3 == 0) (re._1, re._2, re._3, re._1.howMuchMissing(third)) else (re._1, re._2, re._3, "")).map(r =>
      r._1.name + " : Can be cooked " + r._3 + " times for " + third.toInt + " servings, duration " + r._2 + " minutes." + "\n" + r._4
    ).mkString("\n\n")

  }


  def update() = {
    val update = Dialog.showInput(
                   contents.head,
                   "List every update separated with a comma. Write the name of the ingredient plus or minus the amount to be added or decreased followed by the unit.",
                    initial="For example butter+100grams, milk-2.0liters, apple + 3 pieces"
                 )
    var returnText = "No update!"
    val numbersAsChar = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    val unitList = List[CharSequence]("spoon", "cup", "ounce", "pound", "pint", "gallon", "quart", "gram", "liter", "piece")
    update match {
      case Some(updateText) => {
       val u = updateText.toLowerCase
       val plus = u.split(",").map(str => str.dropWhile(_ == ' ')).toBuffer.filter(str => str.contains("+"))
       val minus = u.split(",").map(str => str.dropWhile(_ == ' ')).toBuffer.filter(str => str.contains("-") && !str.contains("+"))
       if(u.split(",").size == plus.size + minus.size) {
         for(p <- plus) {
          val namePart = p.takeWhile(_ != '+').dropWhile(_ ==' ').reverse.dropWhile(_ ==' ').reverse
          val amountString = p.filter(char => numbersAsChar.contains(char) || char == '.')
          val unitPart = p.reverse.dropWhile(_ == ' ').takeWhile(cha => !numbersAsChar.contains(cha)).reverse.dropWhile(_ == ' ')
          if(namePart != "" && amountString != "" && amountString != "." && amountString.count(_ == '.') < 2 && unitList.exists(u => unitPart.contains(u))) {
            val amountPart = amountString.toDouble
            val converted = UnitConverter.convert(List((namePart, amountPart, unitPart)))
            if(!store.ingredients.map(_.name).contains(namePart)) {
              store.addIngredient(converted.head._1, converted.head._2, converted.head._3)
            } else {
              store.increaseAmount(converted.head._1, converted.head._2)
            }
            returnText = "Store updated succesfully!"
          } else {
            returnText = "Updating the store failed, please check you wrote everything right!"
          }
         }
         for(m <- minus) {
          val namePart = m.reverse.dropWhile(_ != '-').tail.dropWhile(_==' ').reverse.dropWhile(_==' ')
          val amountString = m.filter(char => numbersAsChar.contains(char) || char == '.')
          val unitPart = m.reverse.takeWhile(cha => !numbersAsChar.contains(cha)).reverse
          if(namePart != "" && amountString != "" && amountString != "." && amountString.count(_ == '.') < 2 && unitList.exists(u => unitPart.contains(u))) {
            val amountPart = amountString.toDouble
            val converted = UnitConverter.convert(List((namePart, amountPart, unitPart)))
            store.decreaseAmount(converted.head._1, converted.head._2)
            returnText = "Store updated succesfully!"
          } else {
            returnText = "Updating the store failed, please check you wrote everything right!"
          }
         }
       } else {
         returnText = "Updating the store failed, please check you wrote everything right!"
       }
      }

      case None =>
    }
    //Update StoreSituation.txt and if was a success.
    if(returnText == "Store updated succesfully!") {
      RecipeReader.writeToFile("StoreSituation.txt", store.ingredients.map(ing => ing.name + ";" + ing.amount + ";" + ing.unit).sorted.toSeq)
    }
    if(returnText != "No update!") {
      turnOutput.text = store.store()
      turnOutput.editable = false
      Dialog.showMessage(contents.head, returnText, title="Store Update")
    }
  }


  def densityUpdate() = {
    val density = Dialog.showInput(
                    contents.head,
                    "Add a new density if you want. List only one ingredient and its density in g/cm^3. To delete a density write delete and the name of the ingredient. Please write only one input at a time.",
                    initial="For example sugar 0.85"
                  )
    density match {
      case Some(den) => {
        val inputText = den.toLowerCase
        if(inputText.exists(numbersAsChar.contains(_)) && !inputText.contains("delete")) {
          val ingName = inputText.takeWhile(!numbersAsChar.contains(_)).dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse
          //If the input has more than one doubles the .toDouble method throws an exception.
          //It's clearly said to list only one ingredient with density at a time but you never know about the user.
          var dens: Double = 0.0
          Try {
            dens = inputText.filter(no => numbersAsChar.contains(no) || no == '.').toDouble
          } match {
            case Failure(exception) => Dialog.showMessage(contents.head, "Invalid input. Make sure you wrote everything right!", title="Error")
            case Success(value) => {
              UnitConverter.densityList.find(_._1 == ingName) match {
                //Delete the old value because one ingredient cannot have more than one density.
                case Some(old) => {
                  UnitConverter.densityList -= old
                  UnitConverter.densityList += ((ingName, dens))
                }
                case None => UnitConverter.densityList += ((ingName, dens))
              }
              //Update Densities.txt.
              RecipeReader.writeToFile("Densities.txt", UnitConverter.densityList.sorted.map(den => den._1 + ";" + den._2.toString).toSeq)
              Dialog.showMessage(contents.head, "New density added successfully!", title="Success")
            }
          }
        } else if(inputText.contains("delete")) {
          val deleteThis = inputText.split("delete").map(_.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse)
          if(deleteThis.length == 2 && UnitConverter.densityList.map(_._1).contains(deleteThis(1))) {
            UnitConverter.densityList -= UnitConverter.densityList.find(_._1 == deleteThis(1)).head
            //Update Densities.txt
            RecipeReader.writeToFile("Densities.txt", UnitConverter.densityList.sorted.map(den => den._1 + ";" + den._2.toString).toSeq)
            Dialog.showMessage(contents.head, "Density deleted successfully!", title="Success")
          } else {
            Dialog.showMessage(contents.head, "Invalid input. Make sure you wrote everything right!", title="Error")
          }
        } else {
          Dialog.showMessage(contents.head, "Invalid input. Make sure you wrote everything right!", title="Error")
        }
      }
      case None =>
    }
    lastOutput = turnOutput.text
    turnOutput.text = "Densities in g/cm^3 :" + "\n" + UnitConverter.densityList.sorted.map(x => x._1 + " " + x._2).mkString("\n")
  }


  def showRecipe(name: String) = {
    lastOutput = turnOutput.text
    val eaters = portions.selection.item.toDouble
    val chosenOne = recipeBook.allRecipes.filter(_.name == name)
    if(chosenOne.isEmpty) {
      Dialog.showMessage(contents.head, "Invalid recipe name!", title="Error")
    } else {
      turnOutput.text = chosenOne.head.showRecipe(recipeBook, eaters)
      turnOutput.editable = false
    }
  }


  def cookRecipe(name: String) = {
    val eaters = portions.selection.item.toDouble
    val menu = recipeBook.allRecipes.filter(_.name == name)
    if(menu.isEmpty) {
      Dialog.showMessage(contents.head, "Invalid recipe name!", title="Error")
    } else if(menu.head.canBeCooked(recipeBook, eaters) == 0) {
      Dialog.showMessage(contents.head, "You don't have enough ingredients!", title="Error")
    } else {
      menu.head.cookRecipe(store, eaters)
      //Update StoreSituation.txt
      RecipeReader.writeToFile("StoreSituation.txt", store.ingredients.map(ing => ing.name + ";" + ing.amount + ";" + ing.unit).sorted.toSeq)
      Dialog.showMessage(contents.head, "The amount of ingredients were decreased successfully. Happy cooking!", title="Happy cooking!")
      showRecipe(name)
      turnOutput.editable = false
    }
  }


  def edit(text: String) = {
    if(text.contains("Name") && text.contains("Ingredients") && text.contains("Phases")) {
      turnOutput.editable = true
    } else {
      Dialog.showMessage(contents.head, "Open a recipe first!", title="Error")
    }
  }


  private val instructions = List(
        "Name:",
        "Write a recipe name here",
        "Categories:",
        "Write categories here separated with a comma",
        "Time:",
        "X min" + "\n" +
          "Replace X with an integer (How many minutes this recipe takes time?)",
        "Servings:",
        "X (Y Z)" + "\n" +
        "Replace X with an integer (How many servings do you get with this recipe?). Replace Y with the amount you get cooking this recipe and Z with a unit" +
          "For example a smoothie recipe for 5 (=X) servings equals 1 (=Y) liter (=Z) smoothie. This is optional and only needed if an ingredient has its own recipe" +
          "Clear the brackets if you don't replace Y and Z. Clear also this instructions at the end.",
        "Ingredients:",
        "X Y Z" + "\n" +
          "Replace X with an amount, Y with an unit and Z with a ingredient's name. Do this for each ingredient on a new row." +
          "Possible allergens should be listed inside brackets after the ingredient's name, for example 4 ounces feta cheese (lactose)." +
          "Remove this instructions at the end.",
        "Phases:",
        "1. Write phase 1 of this recipe here. Begin a new phase on a new line, will you." + "\n" +
          "Clear all instructions and make sure the recipe is correct. Click Add Recipe button to add this recipe to the recipe book!"
      )

  def createRecipe() = {
    turnOutput.editable = true
    lastOutput = turnOutput.text
    turnOutput.text = this.instructions.mkString("\n")
  }


  def newRecipe(text: String): Unit = {
    if(turnOutput.editable) {
      if(this.instructions.zipWithIndex.filter(_._2 % 2 != 0).map(_._1).exists(text.contains(_))) {
        Dialog.showMessage(contents.head, "Please clean all instructions first!", title="Error")
      } else {
        val recipeText = text.split("\n").toSeq
        val storeBefore = store.ingredients
        Try {
          recipeBook.addRecipe(recipeText)
        } match {
          case Success(value) => {
            val fileName = recipeText(1).filter(_ != ' ') + ".txt"
            //Create a new file or replace an existing one
            RecipeReader.writeToFile(fileName, recipeText)
            //Add the name of the file to RecipeFileNames.txt if not already there.
            if(!recipeFiles.contains(fileName)) {
              recipeFiles += fileName
              RecipeReader.writeToFile("RecipeFileNames.txt", recipeFiles.toSeq)
            }
            //Update StoreSituation.txt if necessary.
            if(store.ingredients.map(_.name) != storeBefore.map(_.name)) {
              RecipeReader.writeToFile("StoreSituation.txt", store.ingredients.map(ing => ing.name + ";" + ing.amount + ";" + ing.unit).toSeq)
            }

            Dialog.showMessage(contents.head, "Recipe added successfully!", title="Success")
            turnOutput.editable = false
          }
          case Failure(exception) => {
            if(exception.getMessage.takeWhile(_ != (',')) == "Recipe exists") {
              val sameName = exception.getMessage.dropWhile(_ != (',')).tail
              val replace = Dialog.showConfirmation(
                contents.head,
				        "A recipe with with name " + sameName + " already exists! Do you want to replace and delete it?",
				        optionType=Dialog.Options.YesNo,
				        title=title)
              if (replace == Dialog.Result.Ok) {
                delete(sameName)
                newRecipe(text)
              }
            } else {
              Dialog.showMessage(contents.head, exception, title="Error")
            }
          }
        }
      }

    } else {
      Dialog.showMessage(contents.head, "You have to create a recipe first!", title="Error")
    }
  }


  def removeRecipe(name: String) = {
    val remove = Dialog.showConfirmation(
      contents.head,
		  "Are you sure you want to delete recipe " + name + "?",
			optionType=Dialog.Options.YesNo,
		  title=title)
    if (remove == Dialog.Result.Ok) {
      delete(name)
    }
  }


  def delete(name: String) = {
    if(recipeBook.allRecipes.map(_.name).contains(name)) {
      val fileName = name.filter(_ != ' ') + ".txt"
      recipeBook.deleteRecipe(name)
      recipeFiles -= fileName
      RecipeReader.writeToFile("RecipeFileNames.txt", recipeFiles.toSeq)
      Dialog.showMessage(contents.head, "Recipe named " + name + " deleted successfully!", title="Success")
    } else {
      Dialog.showMessage(contents.head, "Recipe named " + name + " doesn't exists!", title="Error")
    }
  }


  def closeWindow() = {
    val res = Dialog.showConfirmation(
      contents.head,
		  "Do you really want to quit?",
			optionType=Dialog.Options.YesNo,
			title=title)
    if (res == Dialog.Result.Ok) {
      sys.exit(0)
    }
  }


}

object GuiProgram {
  def main(args: Array[String]) {
    val ui = new UI
    ui.visible = true
  }
}
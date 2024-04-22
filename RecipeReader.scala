import java.io.{BufferedReader, BufferedWriter, FileNotFoundException, FileReader, FileWriter, IOException}



object RecipeReader {

  def alphabet = "abcdefghijklmnopqrstuvxyzåäöABCDEFGHIJKLMNOPQRSTUVXYZÅÄÖ\""


  def fullRecipe(lineReader: BufferedReader) = {

    var resList = Seq[String]()

    var oneLine: String = null

    try {

      while ({oneLine = lineReader.readLine(); oneLine != null}) {

        resList = resList :+ oneLine

     }
    } catch {

      case notFound: FileNotFoundException => println("File not found")

      case e: IOException => println("Reading finished with error")
    }

    resList

  }

  def readFile(textfile: String) = {
    val fullPath = "Textfiles/" + textfile
    val fReader = new FileReader(fullPath)
    val bReader = new BufferedReader(fReader)
    //reads the complete text file
    val res: Seq[String] = RecipeReader.fullRecipe(bReader).filter(_.nonEmpty)
    fReader.close()
    bReader.close()
    res
  }


  def writeToFile(fileName: String, seq: Seq[String]) = {

    try {

      val fullPath = "Textfiles/" + fileName

      val fw = new FileWriter(fullPath);

      val bw = new BufferedWriter(fw)

      try {

        for(line <- seq) {

          bw.write(line)

          bw.newLine()

        }

      } finally {

        bw.close()

      }

    } catch {

      case e: FileNotFoundException => println("File not found"); Seq()

      case ioe: IOException => println("Problems with IO."); Seq()

      case _: Throwable => println("An unexpected exception"); Seq()

    }

  }


}

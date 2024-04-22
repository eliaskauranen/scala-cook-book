import scala.collection.mutable.Buffer

class User(name: String, val store: Store, val settings: Settings) {

}

class Settings() {

  var selectedFilters: Buffer[String] = Buffer[String]()

  var sortBy = "inStore"

  def addFilter(newFilters: Buffer[String]) = {
    for(filter <- newFilters) {
      if(!selectedFilters.contains(filter)){
        selectedFilters += filter
      }
    }
  }

  def sort(by: String) = {
    require(by == "duration" || by == "inStore")
    sortBy = by
  }

}

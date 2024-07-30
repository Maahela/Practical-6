import scala.io.StdIn.readLine

object InventoryManagement2 {

  // Define a case class for Product
  case class Product(name: String, quantity: Int, price: Double)

  // Define the initial empty inventories
  var inventory1: Map[Int, Product] = Map()
  var inventory2: Map[Int, Product] = Map()

  // I. Retrieve all product names from inventory1
  def getProductNames(inventory: Map[Int, Product]): List[String] = {
    inventory.values.map(_.name).toList
  }

  // II. Calculate the total value of all products in inventory1
  def calculateTotalValue(inventory: Map[Int, Product]): Double = {
    inventory.values.map(product => product.quantity * product.price).sum
  }

  // III. Check if inventory1 is empty
  def isInventoryEmpty(inventory: Map[Int, Product]): Boolean = {
    inventory.isEmpty
  }

  // IV. Merge inventory1 and inventory2, updating quantities and retaining the highest price
  def mergeInventories(inventory1: Map[Int, Product], inventory2: Map[Int, Product]): Map[Int, Product] = {
    (inventory1.toSeq ++ inventory2.toSeq)
      .groupBy(_._1)
      .map { case (id, products) =>
        val mergedProduct = products.map(_._2).reduce { (p1, p2) =>
          Product(p1.name, p1.quantity + p2.quantity, Math.max(p1.price, p2.price))
        }
        id -> mergedProduct
      }
  }

  // V. Check if a product with a specific ID exists and print its details
  def getProductById(inventory: Map[Int, Product], productId: Int): Option[Product] = {
    inventory.get(productId)
  }

  // Function to read a product from user input
  def readProduct(): Product = {
    println("Enter product name:")
    val name = readLine()
    println("Enter product quantity:")
    val quantity = readLine().toInt
    println("Enter product price:")
    val price = readLine().toDouble
    Product(name, quantity, price)
  }

  // Function to add a product to an inventory
  def addProductToInventory(inventory: Map[Int, Product]): Map[Int, Product] = {
    println("Enter product ID:")
    val id = readLine().toInt
    val product = readProduct()
    inventory + (id -> product)
  }

  // Function to display the main menu
  def displayMenu(): Unit = {
    println("Choose an option:")
    println("1. Add product to Inventory 1")
    println("2. Add product to Inventory 2")
    println("3. Retrieve all product names from Inventory 1")
    println("4. Calculate total value of all products in Inventory 1")
    println("5. Check if Inventory 1 is empty")
    println("6. Merge Inventory 1 and Inventory 2")
    println("7. Check if a product with a specific ID exists in Inventory 1")
    println("8. Exit")
  }

  // Main method to test the functions
  def main(args: Array[String]): Unit = {
    var continue = true

    while (continue) {
      displayMenu()
      val choice = readLine().toInt

      choice match {
        case 1 =>
          inventory1 = addProductToInventory(inventory1)
          println("Product added to Inventory 1.")

        case 2 =>
          inventory2 = addProductToInventory(inventory2)
          println("Product added to Inventory 2.")

        case 3 =>
          val productNames = getProductNames(inventory1)
          println(s"Product names in Inventory 1: $productNames")

        case 4 =>
          val totalValue = calculateTotalValue(inventory1)
          println(s"Total value of all products in Inventory 1: $$${totalValue}")

        case 5 =>
          val isEmpty = isInventoryEmpty(inventory1)
          println(s"Is Inventory 1 empty? $isEmpty")

        case 6 =>
          val mergedInventory = mergeInventories(inventory1, inventory2)
          println(s"Merged inventory: $mergedInventory")

        case 7 =>
          println("Enter the product ID to search for:")
          val productId = readLine().toInt
          getProductById(inventory1, productId) match {
            case Some(product) => println(s"Product with ID $productId: $product")
            case None => println(s"Product with ID $productId not found in Inventory 1")
          }

        case 8 =>
          continue = false
          println("Exiting...")

        case _ =>
          println("Invalid option. Please choose again.")
      }
    }
  }
}


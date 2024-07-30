import scala.io.StdIn.readLine

object StudentManagement {

  // Function to calculate percentage and grade based on marks
  def calculateGrade(marks: Int, totalMarks: Int): (Double, Char) = {
    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
    (percentage, grade)
  }

  // Function to validate user input (name, marks, total possible marks)
  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.trim.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0 || totalMarks < 0) {
      (false, Some("Marks and total marks must be positive integers."))
    } else if (marks > totalMarks) {
      (false, Some("Marks cannot exceed total marks."))
    } else {
      (true, None)
    }
  }

  // Function to get student info
  def getStudentInfo(): (String, Int, Int, Double, Char) = {
    println("Enter student name:")
    val name = readLine()
    println("Enter marks obtained:")
    val marks = readLine().toInt
    println("Enter total possible marks:")
    val totalMarks = readLine().toInt

    val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
    if (!isValid) {
      println(s"Error: ${errorMessage.getOrElse("Unknown error")}")
      getStudentInfo()
    } else {
      val (percentage, grade) = calculateGrade(marks, totalMarks)
      (name, marks, totalMarks, percentage, grade)
    }
  }

  // Function to print student record
  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
  }

  // Function to get student info with retry until valid data is provided
  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var valid = false
    var result: (String, Int, Int, Double, Char) = null
    while (!valid) {
      val name = readLine("Enter student name: ")
      val marks = readLine("Enter marks obtained: ").toInt
      val totalMarks = readLine("Enter total possible marks: ").toInt
      val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
      if (isValid) {
        val (percentage, grade) = calculateGrade(marks, totalMarks)
        result = (name, marks, totalMarks, percentage, grade)
        valid = true
      } else {
        println(s"Invalid input: ${errorMessage.getOrElse("Unknown error")}")
      }
    }
    result
  }

  // Function to display the main menu
  def displayMenu(): Unit = {
    println("Choose an option:")
    println("1. Add student info")
    println("2. Print student record")
    println("3. Exit")
  }

  // Main method to test the functions
  def main(args: Array[String]): Unit = {
    var continue = true
    var studentRecord: Option[(String, Int, Int, Double, Char)] = None

    while (continue) {
      displayMenu()
      val choice = readLine().toInt

      choice match {
        case 1 =>
          studentRecord = Some(getStudentInfoWithRetry())
          println("Student info added.")

        case 2 =>
          studentRecord match {
            case Some(record) => printStudentRecord(record)
            case None => println("No student record available. Please add student info first.")
          }

        case 3 =>
          continue = false
          println("Exiting...")

        case _ =>
          println("Invalid option. Please choose again.")
      }
    }
  }
}



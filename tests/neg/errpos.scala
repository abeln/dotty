object Test {
  val x =     // error: expression expected
  val y = 2   // error: ';' expected

  val z =     // error: expression expected

  // ...
  val a = 3   // error: ';' expected

  val b = type // error: expression expected (on "type")

  // Compare with null error gone because of changes to the 
  // parser.
  1 match {
    case            // error: pattern expected 
    case 2 => ""
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = 42
    val Y = "42"

    // This used to have twice as many error messages,
    // but changes to the error term in the parser
    // mean that the null-related errors are now gone.
    x match { case { 42 }           => () } // error 
    x match { case { "42".toInt }   => () } // error
    x match { case { "42" }.toInt   => () } // error
    x match { case { "42".toInt }   => () } // error
    x match { case { Y.toInt }      => () } // error
    x match { case { Y }.toInt      => () } // error
  }
}

object ManoInocente {
  def main(args: Array[String]): Unit = {
    // Distingue entre solteros y parejas
    val p = """([\p{L} ]*) y ([\p{L} ]*)""".r
    for (n <- args) {
      n match {
        case p(a, b) => ???
        case s => ???
      }
    }
  }
}
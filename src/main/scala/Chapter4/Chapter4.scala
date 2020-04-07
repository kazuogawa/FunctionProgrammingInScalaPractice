package Chapter4

object Chapter4 {

  def main(args: Array[String]): Unit = {
    def failingFn(i: Int): Int = {
      //これだとエラーになる
      //val y:Int = throw new Exception("fail")
      try {
        val x = 42 + 5
        //x + y
        x + ((throw new Exception("fail")): Int)
      }
      catch {
        case _: Exception => 43
      }
    }
    println(failingFn(12))
  }
}

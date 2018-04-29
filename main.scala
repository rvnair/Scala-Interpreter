import scala.io.Source

object Interpreter {
    def main(args: Array[String]): Unit = {
        Source.fromFile(args(0)).foreach{
        	print
        }
        println(666);
    }
}

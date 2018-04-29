import scala.io.Source

object Kind extends Enumeration {
	val ELSE = Value("ELSE")
	val END = Value("END")
	val EQ = Value("EQ")
	val EQEQ = Value("EQEQ")
	val ID = Value("ID")
	val IF = Value("IF")
	val INT = Value("INT")
	val LBRACE = Value("LBRACE")
	val LEFT = Value("LEFT")
	val MUL = Value("MUL")
	val NONE = Value("NONE")
	val PLUS = Value("PLUS")
	val PRINT = Value("PRINT")
	val RBRACE = Value("RBRACE")
	val RIGHT = Value("RIGHT")
	val SEMI = Value("SEMI")
	val WHILE = Value("WHILE")
	val FUN = Value("FUN")
	val FUNPAR = Value("FUNPAR")
}

object Interpreter {
    def main(args: Array[String]): Unit = {
        Source.fromFile(args(0)).foreach{
        	print
        }
        println(666);
    }
}

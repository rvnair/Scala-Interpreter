import scala.io.Source
import java.nio.file.Files
import java.nio.file.Paths

object Interpreter {

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
    }

    import Kind._

    def main(args: Array[String]): Unit = {
        val progText: String = new String(Files.readAllBytes(Paths.get(args(0))))
//       val k: Value = Kind.MUL.Value
        tokenize(progText, true)
    }

    def tokenize(progText: String, debug: Boolean): Unit = {
        var pos = 0;
        if(debug){
            println(progText)
        }
        while(pos <= progText.length()) {
            if(pos == progText.length()) {
                pos += 1
            }
            else if(progText(pos).isWhitespace){
                pos += 1
            }
            else if(progText(pos) == '{') {
                pos += 1
                if(debug) {
                    println("(LBRACE)")
                }
            }
            else if(progText(pos) == '(') {
                pos += 1
                if(debug) {
                    println("(LEFT)")
                }
            }
            else if(progText(pos) == '*') {
                pos += 1
                if(debug) {
                    println("(MUL)")
                }
            }
            else if(progText(pos) == '+') {
                pos += 1
                if(debug) {
                    println("(PLUS)")
                }
            }
            else if(progText(pos) == '}') {
                pos += 1
                if(debug) {
                    println("(RBRACE)")
                }
            }
            else if(progText(pos) == ')') {
                pos += 1
                if(debug) {
                    println("(RIGHT)")
                }
            }
            else if(progText(pos) == '=') {
                if(progText(pos + 1) == '=') {
                    pos += 2
                    if(debug) {
                        println("(EQEQ)")
                    }
                }
                else{
                    pos += 1
                    if(debug) {
                        println("(EQ)")
                    }
                }
            }
            else if(pos + 2 < progText.length() && (progText(pos) == 'i' && progText(pos + 1) == 'f'
                    && (progText(pos + 2).isWhitespace || progText(pos + 2) == '('))) {
                pos += 2
                if(debug) {
                    println("(IF)")
                }
            }
            else if(pos + 3 < progText.length() && (progText(pos) == 'f' && progText(pos + 1) == 'u' && progText(pos + 2) == 'n'
                    && (progText(pos + 3).isWhitespace || progText(pos + 3) == '{'))) {
                pos += 3
                if(debug) {
                    println("(FUN)")
                }
            }
            else if(pos + 4 < progText.length() && (progText(pos) == 'e' && progText(pos + 1) == 'l' && progText(pos + 2) == 's' && progText(pos + 3) == 'e'
                    && (progText(pos + 4).isWhitespace || progText(pos + 4) == '(' || progText(pos + 4) == '{'))) {
                pos += 4
                if(debug) {
                    println("(ELSE)")
                }
            }
            else if(pos + 5 < progText.length() && (progText(pos) == 'p' && progText(pos + 1) == 'r' && progText(pos + 2) == 'i' && progText(pos + 3) == 'n' && progText(pos + 4) == 't'
                    && (progText(pos + 5).isWhitespace || progText(pos + 5) == '('))) {
                pos += 5
                if(debug) {
                    println("(PRINT)")
                }
            }
            else if(pos + 5 < progText.length() && (progText(pos) == 'w' && progText(pos + 1) == 'h' && progText(pos + 2) == 'i' && progText(pos + 3) == 'l' && progText(pos + 4) == 'e'
                    && (progText(pos + 5).isWhitespace || progText(pos + 5) == '('))) {
                pos += 5
                if(debug) {
                    println("(WHILE)")
                }
            }
            else if(progText(pos).isDigit){
                var num: Long = 0
                while(pos < progText.length() && (progText(pos).isDigit || progText(pos) == '_')) {
                    if(progText(pos).isDigit) {
                        num *= 10
                        var dig = progText(pos).asDigit
                        //num = num.wrapping_add(dig.unwrap() as u64)
                        num += dig
                    }
                    pos += 1
                }
                if(debug) {
                    println("(INT: " + num + ")")
                }
            }
            else if(progText(pos).isLower){
                var s: StringBuilder = new scala.collection.mutable.StringBuilder()
                while(pos < progText.length() && (progText(pos).isDigit || progText(pos).isLower)) {
                    s.append(progText(pos))
                    pos += 1
                }
                if(debug) {
                    println("(ID: " + s + ")")
                }
            }
        }
    }
}

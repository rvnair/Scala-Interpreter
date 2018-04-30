import scala.io.Source
import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable.ListBuffer

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
    class Token(k: Kind.Value, v: Long, id_in: String){
        val kind: Kind.Value = k
        val value: Long  = v
        val id: String = id_in

        override def toString(): String =
            "(" + k + ": " + v + ", " + id + ")"
    }


    def main(args: Array[String]): Unit = {
        val progText: String = new String(Files.readAllBytes(Paths.get(args(0))))
        val tokList: List[Token] = tokenize(progText, false)
        tokList.foreach(println)
    }

    def tokenize(progText: String, debug: Boolean) = {
        var pos = 0;
        var tokList = new ListBuffer[Token]()
        while(pos <= progText.length()) {
            if(pos == progText.length()) {
                pos += 1
            }
            else if(progText(pos).isWhitespace){
                pos += 1
            }
            else if(progText(pos) == '{') {
                pos += 1
                tokList += new Token(Kind.LBRACE, 0, "")
            }
            else if(progText(pos) == '(') {
                pos += 1
                tokList += new Token(Kind.LEFT, 0, "")
            }
            else if(progText(pos) == '*') {
                pos += 1
                tokList += new Token(Kind.MUL, 0, "")
            }
            else if(progText(pos) == '+') {
                pos += 1
                tokList += new Token(Kind.PLUS, 0, "")
            }
            else if(progText(pos) == '}') {
                pos += 1
                tokList += new Token(Kind.RBRACE, 0, "")
            }
            else if(progText(pos) == ')') {
                pos += 1
                tokList += new Token(Kind.RIGHT, 0, "")
            }
            else if(progText(pos) == '=') {
                if(progText(pos + 1) == '=') {
                    pos += 2
                    tokList += new Token(Kind.EQEQ, 0, "")
                }
                else{
                    pos += 1
                    tokList += new Token(Kind.EQ, 0, "")
                }
            }
            else if(pos + 2 < progText.length() && (progText(pos) == 'i' && progText(pos + 1) == 'f'
                    && (progText(pos + 2).isWhitespace || progText(pos + 2) == '('))) {
                pos += 2
                tokList += new Token(Kind.IF, 0, "")
            }
            else if(pos + 3 < progText.length() && (progText(pos) == 'f' && progText(pos + 1) == 'u' && progText(pos + 2) == 'n'
                    && (progText(pos + 3).isWhitespace || progText(pos + 3) == '{'))) {
                pos += 3
                tokList += new Token(Kind.FUN, 0, "")
            }
            else if(pos + 4 < progText.length() && (progText(pos) == 'e' && progText(pos + 1) == 'l' && progText(pos + 2) == 's' && progText(pos + 3) == 'e'
                    && (progText(pos + 4).isWhitespace || progText(pos + 4) == '(' || progText(pos + 4) == '{'))) {
                pos += 4
                tokList += new Token(Kind.ELSE, 0, "")
            }
            else if(pos + 5 < progText.length() && (progText(pos) == 'p' && progText(pos + 1) == 'r' && progText(pos + 2) == 'i' && progText(pos + 3) == 'n' && progText(pos + 4) == 't'
                    && (progText(pos + 5).isWhitespace || progText(pos + 5) == '('))) {
                pos += 5
                tokList += new Token(Kind.PRINT, 0, "")
            }
            else if(pos + 5 < progText.length() && (progText(pos) == 'w' && progText(pos + 1) == 'h' && progText(pos + 2) == 'i' && progText(pos + 3) == 'l' && progText(pos + 4) == 'e'
                    && (progText(pos + 5).isWhitespace || progText(pos + 5) == '('))) {
                pos += 5
                tokList += new Token(Kind.WHILE, 0, "")
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
                tokList += new Token(Kind.INT, num, "")
            }
            else if(progText(pos).isLower){
                var s: StringBuilder = new scala.collection.mutable.StringBuilder()
                while(pos < progText.length() && (progText(pos).isDigit || progText(pos).isLower)) {
                    s.append(progText(pos))
                    pos += 1
                }
                tokList += new Token(Kind.ID, 0, s.toString())
            }
        }
        val retTokList = tokList.toList
        if(debug){
            println(progText)
            retTokList.foreach(println)
        }
        retTokList
    }
}

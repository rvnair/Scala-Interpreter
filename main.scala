import scala.io.Source
import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.math.pow

object Interpreter {

    var tokList: List[Token] = null
    var tokInd: Integer = 0
    var symTab: HashMap[String, Long] = new HashMap()

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
    	val SUB = Value("SUB")
    	val DIV = Value("DIV")
        val EXP = Value("EXP")
    	val NONE = Value("NONE")
    	val PLUS = Value("PLUS")
    	val PRINT = Value("PRINT")
    	val RBRACE = Value("RBRACE")
    	val RIGHT = Value("RIGHT")
    	val SEMI = Value("SEMI")
    	val WHILE = Value("WHILE")
    	val FUN = Value("FUN")
        val FOR = Value("FOR")
        val NOT = Value("NOT")
        val AND = Value("AND")
        val OR = Value("OR")
        val STRID = Value("STRID")
        val STRING = Value("STRING")
    }

    import Kind._
    class Token(k: Kind.Value, v: Long, id_in: String){
        val kind: Kind.Value = k
        val value: Long  = v
        val id: String = id_in

        override def toString(): String =
            "(" + k + ": " + v + ", " + id + ")"
    }

    def e1(): Long = {
        tokList(tokInd).kind match {
            case Kind.LEFT => {
                tokInd += 1
                val value = expression()
                tokInd += 1
                value
            }
            case Kind.FUN => {
                val funInd: Long = tokInd.toLong
                tokInd += 1
                statement(false)
                funInd
            }
            case Kind.INT => {
                val value = tokList(tokInd).value
                tokInd += 1
                value
            }
            case Kind.ID => {
                val id = tokList(tokInd).id
                tokInd += 1
                var value: Long = symTab.getOrElse(id, 0)
                value
            }
            case Kind.NOT => {
                tokInd += 1
                var value = e1()
                if(value != 0){
                    value = 0
                }
                else{
                    value = 1
                }
                value
            }
            case _ => {
                0
            }
        }
    }
    def e2(): Long = {
        var value = e1()
        while(tokList(tokInd).kind == Kind.EXP) {
            tokInd += 1
            value = pow(value.toDouble, e1().toDouble).toLong
        }
        value
    }
    def e3(): Long = {
        var value = e2()
        while((tokList(tokInd).kind == Kind.MUL) || (tokList(tokInd).kind == Kind.DIV)) {
            if(tokList(tokInd).kind == Kind.MUL) {
                tokInd += 1
                value *= e2()
            }
            else {
                tokInd += 1
                value /= e2()
            }
        }
        value
    }

    def e4(): Long = {
        var value = e3()
        while((tokList(tokInd).kind == Kind.PLUS) || (tokList(tokInd).kind == Kind.SUB)) {
            if(tokList(tokInd).kind == Kind.PLUS) {
                tokInd += 1
                value += e3()
            }
            else {
                tokInd += 1
                value -= e3()
            }
        }
        value
    }

    def e5(): Long = {
        var value = e4()
        while(tokList(tokInd).kind == Kind.EQEQ || tokList(tokInd).kind == Kind.AND || tokList(tokInd).kind == Kind.OR) {
            if(tokList(tokInd).kind == Kind.EQEQ){
            	tokInd += 1
            	if (value == e4()){
            		value = 1
            	}
            	else {
            		value = 0
            	}
            }
            
			if(tokList(tokInd).kind == Kind.AND){
            	tokInd += 1
            	if (value != 0 && e4() != 0){
            		value = 1
            	}
            	else {
            		value = 0
            	}
            }

            if(tokList(tokInd).kind == Kind.OR){
            	tokInd += 1
            	if (value != 0 || e4() != 0){
            		value = 1
            	}
            	else {
            		value = 0
            	}
            }
        }
        value
    }

    def expression(): Long = {
        e5()
    }

    def statement(doit: Boolean): Boolean = {
        tokList(tokInd).kind match {
            case Kind.ID => {
                val id: String = tokList(tokInd).id
                tokInd += 1
                if(tokList(tokInd).kind == Kind.LEFT) {
                    if(doit) {
                        tokInd += 1
                        val tempTokInd = tokInd
                        var v: Long = symTab.getOrElse(id, 0)
                        tokInd = v.toInt
                        tokInd += 1
                        statement(doit)
                        tokInd = tempTokInd
                        tokInd += 1
                    }
                    else {
                        tokInd += 2
                    }
                }
                else {
                    tokInd += 1
                    val value = expression()
                    if(doit){
                        symTab.put(id, value)
                    }
                }
                true
            }
            case Kind.LBRACE => {
                tokInd += 1
                seq(doit)
                tokInd += 1
                true
            }
            case Kind.IF => {
                tokInd += 1
                if(expression() > 0){
                    statement(doit)
                    if(tokList(tokInd).kind == Kind.ELSE) {
                        tokInd += 1
                        statement(false)
                    }
                }
                else {
                    statement(false)
                    if(tokList(tokInd).kind == Kind.ELSE) {
                        tokInd += 1
                        statement(doit)
                    }
                }
                true
            }
            case Kind.WHILE => {
                val tempInd = tokInd
                tokInd += 1
                var eval = expression()
                if(eval > 0 && doit) {
                    while(eval > 0) {
                        statement(doit)
                        tokInd = tempInd
                        tokInd += 1
                        eval = expression()
                    }
                    statement(false)
                }
                else {
                    statement(false)
                }
                true
            }
            case Kind.FOR => {
                //TODO: implement For loops in this form : for({statement};(expression);{statement}) {statement: loop body }
                tokInd += 2
                statement(doit)
                val tempInd = tokInd
                var eval = expression()
                val endStatement = tokInd
                statement(false)
                tokInd += 1
                val forStart = tokInd
                if(eval != 0 && doit){
                	while(eval > 0) {
                		statement(doit)
                		tokInd = endStatement
                		statement(doit)
                		tokInd = tempInd
                		eval = expression()
                		tokInd = forStart
                	}
                	statement(false)
                }
                else{
                	statement(false)
                }
                false
            }
            case Kind.PRINT => {
                tokInd += 1
                if(doit) {
                    println(expression())
                }
                else {
                    expression()
                }
                true
            }
            case _ => {
                false
            }
        }
    }

    def seq(doit: Boolean) = {
        while(statement(doit)){}
    }

    def program() = {
        seq(true)
    }

    def main(args: Array[String]): Unit = {
        val progText: String = new String(Files.readAllBytes(Paths.get(args(0))))
        tokList = tokenize(progText, false)
        //tokList.foreach(println)
        program()
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
            else if(progText(pos) == '^') {
                pos += 1
                tokList += new Token(Kind.EXP, 0, "")
            }
            else if(progText(pos) == '*') {
                pos += 1
                tokList += new Token(Kind.MUL, 0, "")
            }
            else if(progText(pos) == '/') {
                pos += 1
                tokList += new Token(Kind.DIV, 0, "")
            }
            else if(progText(pos) == '-') {
                pos += 1
                tokList += new Token(Kind.SUB, 0, "")
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
            else if(progText(pos) == '!') {
                pos += 1
                tokList += new Token(Kind.NOT, 0, "")
            }
            else if(progText(pos) == '&') {
            	pos += 1
            	tokList += new Token(Kind.AND, 0, "")
            }
            else if(progText(pos) == '|') {
            	pos += 1
            	tokList += new Token(Kind.OR, 0, "")
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
            else if(pos + 3 < progText.length() && (progText(pos) == 'f' && progText(pos + 1) == 'o' && progText(pos + 2) == 'r'
                    && (progText(pos + 3).isWhitespace || progText(pos + 3) == '('))) {
                pos += 3
                tokList += new Token(Kind.FOR, 0, "")
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
        tokList += new Token(Kind.END, 0, "")
        val retTokList = tokList.toList
        if(debug){
            println(progText)
            retTokList.foreach(println)
        }
        retTokList
    }
}

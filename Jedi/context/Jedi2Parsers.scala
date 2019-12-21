package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {
  
  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]] = "(" ~ opt(identifier ~ rep("," ~> identifier)) ~ ")" ^^ {
    case "(" ~ None ~ ")" => Nil
    case "(" ~ Some(id ~ more) ~ ")" => id :: more
  }
  
  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda] = "lambda" ~ params ~ expression ^^ {
    case "lambda" ~ p ~ e => Lambda(p, e)
  }
  
  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Block] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ e ~ Nil ~ "}" => Block(List[Expression](e))
    case "{" ~ e ~ more ~ "}"=> Block(e :: more)
  }
  
  // Thunk Parser
  def thunk: Parser[MakeThunk] = "freeze" ~ "(" ~ expression ~ ")" ^^ {
    case "freeze" ~ "(" ~ e ~ ")" => MakeThunk(e)
  }
  
  // Text Parser
  def text: Parser[MakeText] = "delay" ~ "(" ~ expression ~ ")" ^^ {
    case "delay" ~ "(" ~ e ~ ")" => MakeText(e)
  }
  
  
  // override of term parser
  //override def term: Parser[Expression]  = lambda | funCall | block | literal | "("~>expression<~")"
  override def term: Parser[Expression]  = lambda | thunk | text | funCall | block | literal | "("~>expression<~")"
}

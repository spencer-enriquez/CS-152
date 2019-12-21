package context

import scala.util.parsing.combinator._
import expression.Identifier

class JediException(val gripe: String = "Jedi error ") extends Exception(gripe)
class UndefinedException(val name: Identifier) extends JediException("Undefined identifier: " + name) 
class TypeException(val gripe1: String = "Type Error") extends JediException(gripe1)
class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error")
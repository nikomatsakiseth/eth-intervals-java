package inter.compiler

import scala.collection.mutable.Map

class SymbolTable
{
    val classes = Map[Name.Qual, Symbol.Class]()
}
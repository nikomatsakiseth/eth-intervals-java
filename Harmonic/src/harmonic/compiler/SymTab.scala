package harmonic.compiler

object SymTab {
    sealed abstract class Entry {
        def name: Name.Var
    }
    sealed case class Field(name: Name.MemberVar) extends Entry
    sealed case class Type(name: Name.MemberVar) extends Entry
    sealed case class Local(name: Name.LocalVar) extends Entry
    
    type Map = scala.collection.immutable.Map[String, Entry]
    
    val empty: Map = scala.collection.immutable.Map()
    
    case class ExtendedMap(m: Map) {
        def +(entry: SymTab.Entry): Map = m + (entry.name.text -> entry)
    }
    implicit def extendedMap(m: Map) = ExtendedMap(m)
}

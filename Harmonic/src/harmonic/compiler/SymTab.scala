package harmonic.compiler

object SymTab {
    sealed abstract class Entry {
        def name: Name.Var
        def asMemberEntryMatching(uName: Name.UnloweredMemberVar): Option[MemberEntry]
    }
    sealed abstract class MemberEntry extends Entry {
        def name: Name.MemberVar
        def asMemberEntryMatching(uName: Name.UnloweredMemberVar) = {
            if(name.matches(uName)) Some(this)
            else None
        }
        def isConstrainableInPathArg: Boolean = false
        def isConstrainableInTypeArg: Boolean = false
    }
    sealed case class InstanceField(name: Name.MemberVar) extends MemberEntry {
        def isConstrainableInPathArg = true
    }
    sealed case class StaticField(name: Name.MemberVar) extends MemberEntry
    sealed case class Type(name: Name.MemberVar) extends MemberEntry {
        def isConstrainableInTypeArg = true
    }
    sealed case class Ghost(name: Name.MemberVar) extends MemberEntry {
        def isConstrainableInPathArg = true        
    }
    sealed case class Local(name: Name.LocalVar) extends Entry {
        def asMemberEntryMatching(uName: UnloweredMemberVar) = None        
    }
    
    type Map = scala.collection.immutable.Map[String, Entry]
    
    val empty: Map = scala.collection.immutable.Map()
    
    case class ExtendedMap(m: Map) {
        def +(entry: SymTab.Entry): Map = m + (entry.name.text -> entry)
    }
    implicit def extendedMap(m: Map) = ExtendedMap(m)
}

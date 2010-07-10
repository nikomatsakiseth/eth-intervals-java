package harmonic.compiler

import com.smallcultfollowing.lathos._
import com.smallcultfollowing.lathos.Lathos

import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition

import Util._

object VarSymbol {
    type Any = VarSymbol[Name.Var]
    
    class Field(
        val pos: Position,
        val modifiers: Modifier.Set,
        val name: Name.Member,
        val ty: Type,
        val kind: FieldKind
    ) extends VarSymbol[Name.Member]
    
    def errorField(name: Name.Member, optExpTy: Option[Type]) = {
        val ty = optExpTy.getOrElse(Type.Null)
        new Field(NoPosition, Modifier.Set.empty, name, ty, FieldKind.Harmonic) {
            override def isError = true
        }
    }
    
    class Local(
        val pos: Position,
        val modifiers: Modifier.Set,
        val name: Name.LocalVar,
        val ty: Type
    ) extends VarSymbol[Name.LocalVar] {
        def toPath = Path.Local(name)
        def toTypedPath = Path.TypedLocal(this)
    }
    
    def errorLocal(name: Name.LocalVar, optExpTy: Option[Type]) = {
        val ty = optExpTy.getOrElse(Type.Null)
        new Local(NoPosition, Modifier.Set.empty, name, ty) {
            override def isError = true
        }
    }
}

abstract class VarSymbol[+N <: Name.Var] extends Symbol with Page {
    val modifiers: Modifier.Set
    val name: N
    val ty: Type
    
    override def toString = "%s(%s, %x)".format(
        getClass.getSimpleName,
        name, 
        System.identityHashCode(this)
    )
    
    def isNamed(aName: Name.Var) = (name == aName)    
    
    // ___ Page interface ___________________________________________________
    
    override def getId = "VarSymbol[%s]".format(System.identityHashCode(this))
    
    override def getParent = null
    
    override def addContent(content: PageContent) = throw new UnsupportedOperationException()
    
    override def renderInLine(out: Output): Unit = {
        Lathos.renderInLine(this, out)
    }
    
    override def renderInPage(out: Output): Unit = {
        out.startPage(this)
        
        out.startTable
        
        out.row("name", name)
        out.row("class", getClass.getSimpleName)
        out.row("ty", ty)
        out.row("pos", pos)
        
        out.endTable
        
        out.endPage(this)
    }
    
}
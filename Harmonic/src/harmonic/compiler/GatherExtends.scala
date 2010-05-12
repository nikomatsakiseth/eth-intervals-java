package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable

import scala.util.parsing.input.Position
import Ast.{Lower => in}
import Util._

/** Given a class C, creates a flattened list
  * ExtendsDecl listing the supertypes and the
  * arguments provided to them.  All uses of a
  * given class must provide equivalent arguments
  * up to equality.  The order in which those
  * constructors are executed will be the MRO
  * order. 
  *
  * Requires: 
  * - lower for csym and its supertypes has completed */
case class GatherExtends(global: Global) {
    
    class Data(pos: Position) {
        
        // Extends declarations for (transitive) supertypes of `csym`.
        // All substituted so as to be in terms of the parameters of `csym`.        
        val result = new mutable.HashMap[Name.Class, (Name.Class, in.ExtendsArg)]()
        
        // Returns None if pair are equivalent, else returns `Some(pos)` where
        // pos is the position of the item in the left which caused an error.
        def notEquatable(env: Env)(pair: (in.ExtendsArg, in.ExtendsArg)): Option[(in.ExtendsArg, in.ExtendsArg)] = {
            pair match {
                case (in.TupleExtendsArg(List(left)), right) =>
                    notEquatable(env)((left, right))
                case (left, in.TupleExtendsArg(List(right))) =>
                    notEquatable(env)((left, right))
                case (in.TupleExtendsArg(lefts), in.TupleExtendsArg(rights)) if sameLength(lefts, rights) =>
                    lefts.zip(rights).firstSome(notEquatable(env))
                case (in.PathExtendsArg(left), in.PathExtendsArg(right)) 
                if env.pathsAreEquatable(left.path.toPath, right.path.toPath) =>
                    None
                case (left, right) => 
                    Some((left, right))
            }
        }
        
        def addExtendsDecl(
            env: Env, 
            fromClass: Name.Class,
            subst: TypedSubst
        )(
            extendsDecl: in.ExtendsDecl
        ) = {
            val className = extendsDecl.className.name
            val arg = subst.extendsArg(extendsDecl.arg)
            
            global.csym(className) match {
                // Harmonic classes may be extended multiple times,
                // but the constructor arguments in all cases must be
                // identical.
                case csym: ClassFromSource => {
                    result.get(className) match {
                        case None => {
                            result(className) = (fromClass, arg)                            
                        }
                    
                        case Some((rightClass, rightArg)) => {
                            notEquatable(env)(arg, rightArg) match {
                                case None => // Arg is equivalent to otherArg.
                                case Some((left, right)) => {
                                    Error.ExtendsNotEquiv(fromClass, left, rightClass, right).report(global, pos)
                                }
                            }
                        }
                    }
                    
                    
                }
            
                // Non-harmonic classes are currently only Java interfaces,
                // so no worries there. 
                // TODO Extend to support a single Java class
                case _ => {}
            }
        }
    
        def addFor(subst: TypedSubst)(csym: ClassFromSource) = {
            val env = csym.classEnv
            csym.loweredSource.extendsDecls.foreach(addExtendsDecl(env, csym.name, subst))                    
        }
        
    }
    
    def forSym(csym: ClassFromSource) = {
        val data = new Data(csym.pos)
        data.addFor(TypedSubst.empty)(csym)
    }
    
}
package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable

import com.smallcultfollowing.lathos.Lathos

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
    val log = Lathos.context
    
    class Data(pos: Position) {
        
        // Extends declarations for (transitive) supertypes of `csym`.
        // All substituted so as to be in terms of the parameters of `csym`.        
        val result = new mutable.HashMap[Name.Class, (Name.Class, in.ExtendsDecl, List[SPath[Reified]])]()
        
        // Returns None if pair are equivalent, else returns `Some(pos)` where
        // pos is the position of the item in the left which caused an error.
        def notEquatable(env: Env)(pair: (SPath[Reified], SPath[Reified])): Boolean = {
            val (left, right) = pair
            !env.pathsAreEquatable(left.toPath, right.toPath)
        }
        
        def addExtendsDecl(
            env: Env, 
            fromClass: Name.Class,
            subst: TypedSubst
        )(
            extendsDecl: in.ExtendsDecl
        ): Unit = {
            val className = extendsDecl.className.name
            val args = extendsDecl.args.map(n => n.path.subst(subst))

            global.csym(className) match {
                // Harmonic classes may be extended multiple times,
                // but the constructor arguments in all cases must be
                // identical.
                case csym: ClassFromSource => {
                    result.get(className) match {
                        case None => {
                            result(className) = (fromClass, extendsDecl, args)                            
                        }
                    
                        case Some((rightClass, _, rightArgs)) => {
                            args.zip(rightArgs).find(notEquatable(env)) match {
                                case None => // All are equatable.
                                case Some((left, right)) => {
                                    Error.ExtendsNotEquiv(
                                        csym.name,
                                        fromClass, left, 
                                        rightClass, right).report(global, pos)
                                }
                            }
                        }
                    }
                    
                    // Recursively process each of the extends declarations from `csym`:
                    //    To do so, must create subst from ctor params of `csym` to the arguments.
                    val nextSubst = csym.typedSubstForFlatArgs(args)
                    addFor(nextSubst)(csym)
                }
            
                // Non-harmonic classes are currently only Java interfaces,
                // so no worries there. 
                // TODO Extend to support a single Java class
                case _ => ()
            }
        }
    
        def addFor(subst: TypedSubst)(csym: ClassFromSource): Unit = {
            val env = csym.classEnv
            csym.loweredSource.extendsDecls.foreach(addExtendsDecl(env, csym.name, subst))                    
        }
        
    }
    
    def forSym(csym: ClassFromSource) = log.indent("GatherExtends(", csym, ")") {
        val data = new Data(csym.pos)
        data.addFor(TypedSubst.empty)(csym)
        csym.ExtendedClasses.v = csym.mro.tail.reverse.flatMap { 
            case mroCsym: ClassFromSource => {
                val (_, decl, args) = data.result(mroCsym.name) 
                Some((decl, args))
            }
            case _ => None
        }
    }
    
}
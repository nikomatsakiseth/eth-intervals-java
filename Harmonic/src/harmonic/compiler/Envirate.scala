package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.util.parsing.input.Position

import com.smallcultfollowing.lathos.Lathos

import Ast.{Lower => in}
import Ast.Lower.Extensions._
import Util._

/** Computes the class environment that is then used by type checking
  * and other phases. */
case class Envirate(global: Global) {
    
    // General plan:
    // - Add the requirements listed in the primary constructor to the
    //   environment
    // - For each extends declaration:
    //   - Prepare a substition from pairs of formal arguments and paths.
    //   - Apply substitution to the exported environment from the superclass
    //     constructor.
    //   - Filter out relations of non-final paths.
    //   - Add to environment.
    // - Add relations from class body if both sides are final.
    
    def addAstReq(env: Env, req: in.Requirement) = env.plusFact(req.toFact)
    
    def forClassFromSource(csym: ClassFromSource) = {
        
        val log = Lathos.context
        
        val cdecl = csym.loweredSource
        
        // Add the this variable:
        var env = Env.empty(global).plusThis(csym.toType, cdecl.thisSym)
        
        // Add requirements from primary ctor to env:
        val classReqs: List[in.Requirement] = Nil // TODO
        env = classReqs.foldLeft(env)(addAstReq)

        val method = env.symPath(Path.ThisInit)
        
        // Add inherited environments from extends clauses:
        cdecl.extendsDecls.foreach { 
            case in.ExtendsDecl(name, args, (msym, msig)) => {
                val argPaths = args.map(_.path)

                val vsubst = msym.substForFlatSPaths(argPaths)
                val ensures = msym.ensures.view.map(vsubst.fact)
                val finalEnsures = ensures.filter(env.factIsFinalBy(_, method))
                env = env.plusFacts(finalEnsures)

                val supCsym = global.csym(name.name)
                env = env.plusFactSet(supCsym.checkEnv.factSet)
            }
        }
        
        // Add from class body:
        val newFacts = cdecl.members.flatMap {
            case in.RelDecl(_, in.TypedPath(left), rel, in.TypedPath(right)) => {
                log.embeddedIndent("Considering ", left, rel, right) {
                    (
                        env.pathIsFinalBy(left, method) &&
                        env.pathIsFinalBy(right, method)
                    ).toOption(rel.toFact(left.toPath, right.toPath))
                }
            }
                
            case _ => None
        }
        env = env.plusFacts(newFacts)
        
        // Store final computed class environment:
        csym.CheckEnv.v = env
        
    }
    
}
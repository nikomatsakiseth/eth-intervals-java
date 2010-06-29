package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.util.parsing.input.Position

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
    
    def addAstReq(env: Env, req: in.Requirement) = {
        req match {
            case in.PathRequirement(in.TypedPath(left), rel, in.TypedPath(right)) => {
                env.plusPathRel(Req.P(left.toPath, rel, right.toPath))
            }
            case in.TypeRequirement(in.TypeRef(left), rel, in.TypeRef(right)) => {
                env.plusTypeRel(Req.P(left.toPath, rel, right.toPath))                
            }
        }
    }
    
    def forClassFromSource(csym: ClassFromSource) = {
        
        val cdecl = csym.loweredSource

        // Add requirements from primary ctor to env:
        val classReqs: List[in.Requirement] = Nil // TODO
        var env = classReqs.foldLeft(Env.empty(global))(addAstReq)

        // Add inherited environments from extends clauses:
        cdecl.extendsDecls.foreach { 
            case in.ExtendsDecl(name, args, (msym, msig)) => {
                val argPaths = args.map(_.path)

                val vsubst = msym.substForFlatArgs(argPaths)
                val ensures = msym.ensures.map(vsubst.req)
                env = env.plusRels(ensures)

                val supCsym = global.csym(name.name)
                val fsubst = supCsym.typedSubstForFlatArgs(Path.This)(argPaths)
                val substdRels = supCsym.checkEnv.allRels.map(fsubst.req)
                val finalRels = substdRels.filter(env.relIsFinalBy(_, Path.ThisInit))
                env = env.plusRels(finalRels)
            }
        }
        
        // Add from class body:
        cdecl.members.foreach {
            case in.RelDecl(_, in.TypedPath(left), rel, in.TypedPath(right)) => {
                if(
                    env.pathIsFinalBy(left, Path.ThisInit) &&
                    env.pathIsFinalBy(right, Path.ThisInit)
                ) {
                    env = env.plusRel(Rel.P(left.toPath, rel, right.toPath))
                }
            }
            case _ => ()
        }
        
        // Store final computed class environment:
        csym.CheckEnv.v = env
        
    }
    
}
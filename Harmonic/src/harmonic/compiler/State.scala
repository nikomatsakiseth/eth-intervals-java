package harmonic.compiler

import ch.ethz.intervals

object State {
    def createSymbols(
        global: Global,
        master: Interval,
        compUnit: Ast.Parse.CompUnit
    ) = {
        Ast.Parse.definedClasses(compUnit).foreach { case (className, cdecl) =>
            global.optCsym(className) match {
                case Some(_) => {
                    global.reporter.report(cdecl.pos, "class.already.defined", List(className.toString))
                }
                
                case None => {
                    val csym = addCsym(new Symbol.ClassFromSource(className, this))
                    
                    // For a class being loaded from source, this is the structure:
                    //
                    // header -> body -> | lower --------------------- | -> byteCode
                    //                    \                           /
                    //                     create -> members -> merge 
                    //
                    // header: determines the list of members, names of superclasses.
                    //
                    // body: resolves members in the body to absolute class names, etc.
                    //
                    // lower: summary interval for the job of reducing IR to the simpler form
                    // - create: creates intervals for each member and the merge interval
                    // - members: summary interval for the intervals that lower each indiv. member
                    // - merge: merges all the lowered members into a lowered class declaration
                    // 
                    // byteCode: emits the byte code from the lowered form

                    val header = csym.addInterval(ClassSymbol.Header) {
                        master.subinterval() { header =>
                            val state = State(global, master, header, csym)
                            ResolveHeader(state, compUnit).resolveClassHeader(csym, cdecl)                        
                        }
                    }
                    
                    val body = csym.addInterval(ClassSymbol.Body) {
                        master.subinterval(after = List(header)) { body =>
                            val state = State(global, master, body, csym)
                            ResolveBody(state, compUnit).resolveClassBody(csym, cdecl)
                        }
                    }

                    val lower = csym.addInterval(ClassSymbol.Lower) {
                        master.subinterval(after = List(body)) { lower => 
                            () // Lower is really just a placeholder.
                        }
                    }
                    
                    val create = csym.addInterval(ClassSymbol.Create) {
                        master.subinterval(during = List(lower)) { create => 
                            val state = State(global, master, create, csym)
                            Create(state).createMemberIntervals()
                        }
                    }

                    csym.addInterval(ClassSymbol.ByteCode) {
                        master.subinterval(after = List(lower)) { byteCode => 
                            val state = State(global, master, byteCode, csym)
                            GatherOverrides(state).forSym(csym)
                            ByteCode(state).writeClassSymbol(csym)
                        }
                    }
                }
            }
        }        
    }
}

case class State(
    /** Data shared by the entire compilation. */
    global: Global,
    
    /** The interval representing the compilation as a whole. */
    master: Interval,
    
    /** The interval currently executing.  Always a subinterval
      * of master. */
    current: Interval,
    
    /** The class symbol being currently processed, if any. */
    curCsym: Symbol.ClassFromSource
) {
    def withCurrent(inter: Interval): AnyState
    def withCurCsym(csym: Symbol.ClassFromSource) = new State(global, master, current, csym)
    
    def csym(name: Name.Class) = global.csym(name)
    
    /** Reports an error.  Also flags the current class
      * as having failed compilation. */
    def report(pos: Position, msg: String, args: String*) {
        curCsym.addErrorReport()
        global.reporter.report(pos, msg, args.toList)        
    }
}
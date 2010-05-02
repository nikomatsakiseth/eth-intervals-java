package harmonic.compiler

import java.io.File
import scala.util.parsing.input.Position
import scala.collection.mutable

class CompilationState(
    val config: Config,
    val reporter: Reporter
) {
    /** Maps a class name to its symbol. */
    val classes = new mutable.HashMap[Name.Qual, Symbol.Class]()
    
    /** Compilation units containing classes whose body has not yet been resolved. */
    val toBeResolved = new mutable.Queue[Ast.Parse.CompUnit]()
    
    /** Symbols parsed and resolved but not yet lowered. */
    val toBeLowered = new mutable.Queue[Symbol.ClassFromSource]()
    
    /** Symbols lowered but for which we have not yet emitted byte code. */
    val toBeBytecoded = new mutable.Queue[Symbol.ClassFromSource]()
    
    // ___ Private Data For Other Classes ___________________________________
    //
    // Various passes and helpers, such as Lower or MethodResolutionOrder,
    // require state that should persist within a particular compilation.
    // They store their data in the `CompilationState` using the data() method.
    
    private[this] val privateData = new mutable.HashMap[java.lang.Class[_], Any]()
    
    def data[C](cls: java.lang.Class[C]): C = {
        privateData.get(cls) match {
            case Some(v) => cls.cast(v)
            case None => {
                val v = cls.getConstructor().newInstance()
                privateData(cls) = v
                v
            }
        }
    }
    
    // ___ Scheduler ________________________________________________________
    
    private[this] val activePasses = new mutable.HashSet[Pass]()
    var currentPass: Pass = null
    
    def sched(before: List[Pass] = List(), after: List[Pass] = List())(func: => Unit): Pass = {
        val pass = new Pass(() => func, after)
        assert(before.forall(activePasses))
        before.foreach(_.addDependency(pass))
        activePasses += pass
        pass
    }
    
    def drain() = {
        while(!activePasses.isEmpty) {
            activePasses.find(_.isEligible(activePasses)) match {
                case Some(pass) => {
                    activePasses -= pass
                    currentPass = pass
                    pass.func()
                    currentPass = null                                    
                }
                
                case None => throw new RuntimeException("Deadlock!")
            }
        }
    }
    
    def compile() = drain()
    
    // ___ Intrinsics _______________________________________________________
    
    val intrinsics = new mutable.HashMap[(Name.Qual, Name.Method), List[Symbol.Method]]()
    
    def addIntrinsic(msym: Symbol.Method) {
        val key = (msym.clsName, msym.name)
        intrinsics(key) = msym :: intrinsics.get(key).getOrElse(Nil)
    }
    
    /** Checks for an intrinsic method --- i.e., one that is built-in to the compiler ---
      * defined on the type `rcvrTy` with the name `name`. */
    def lookupIntrinsic(className: Name.Qual, methodName: Name.Method): Option[List[Symbol.Method]] = {
        intrinsics.get((className, methodName))
    }
    
    // ___ Loading and resolving class symbols ______________________________
    
    private[this] def createSymbols(compUnits: List[Ast.Parse.CompUnit]) {
        compUnits.foreach { compUnit =>
            Ast.Parse.definedClasses(compUnit).foreach { case (className, cdecl) =>
                if(classes.isDefinedAt(className))
                    reporter.report(cdecl.pos, "class.already.defined", className.toString)
                else {
                    val csym = new Symbol.ClassFromSource(className)
                    classes(className) = csym
                    
                    csym.resolveHeader = List(sched() {
                        ResolveHeader(this, compUnit).resolveClassHeader(csym, cdecl)
                    })
                    
                    csym.resolveHeaderClosure = List(sched(after = csym.resolveHeader) {
                        // Just a placeholder: resolveHeader will add incoming dependencies
                        // from the supertypes of `csym` that prevent this task from executing
                        // until the headers of all supertypes have been resolved.
                    })
                    
                    csym.resolveBody = List(sched(after = csym.resolveHeaderClosure) {
                        ResolveBody(this, compUnit).resolveClassBody(csym, cdecl)
                    })
                    
                    csym.lower = List(sched(after = csym.resolveBody) {
                        csym.loweredSource = Lower(this).lowerClass(csym)
                        if(config.dumpLoweredTrees)
                            csym.loweredSource.println(PrettyPrinter.stdout)
                    })
                    
                    csym.byteCode = List(sched(after = csym.lower) {
                        val csym = toBeBytecoded.dequeue()
                        GatherOverrides(this).forSym(csym)
                        ByteCode(this).writeClassSymbol(csym)
                    })
                }                
            }
        }
    }
    
    def loadInitialSources(files: List[java.io.File]) {
        val compUnits = files.flatMap(Parse(this, _))
        createSymbols(compUnits)
    }
    
    /** True if a class with the name `className` has been
      * loaded or we can find source for it (in which case
      * that source is loaded). */
    def loadedOrLoadable(className: Name.Class) = {
        classes.isDefinedAt(className) || locateSource(className)
    }
    
    /** Loads a class named `className`.  If it fails, reports an 
      * error and inserts a dummy into the symbol table. */
    def requireLoadedOrLoadable(pos: Position, className: Name.Class) = {
        val result = loadedOrLoadable(className)
        if(!result) {
            reporter.report(pos, "cannot.find.class", className.toString)
            classes(className) = new Symbol.ClassFromErroroneousSource(className)
        }
        result
    }
    
    /** Tries to locate a source for `className`.  If a source
      * is found, instantiates a corresponding symbol and
      * returns true.  Otherwise returns false. */
    def locateSource(className: Name.Class) = {        
        
        /** When we find a reference to a source file, we parse it and load
          * the resulting classes into the symbol table.  For each class,
          * we also resolve any references it may have to other classes. */
        def loadSourceFile(file: java.io.File) {
            Parse(this, file) match {
                case None => { 
                    // Parse error:
                    classes(className) = new Symbol.ClassFromErroroneousSource(className)
                }

                case Some(compUnit) => {
                    // Check that we got (at least) the class we expected to find:
                    Ast.Parse.definedClasses(compUnit).find(_._1 == className) match {
                        case None => {
                            reporter.report(
                                InterPosition.forFile(file),
                                "expected.to.find.class", 
                                className.toString
                            )
                            classes(className) = new Symbol.ClassFromErroroneousSource(className)                            
                        }
                        
                        case Some(_) =>
                    }
                    
                    // Process the loaded classes.
                    createSymbols(List(compUnit)) 
                }
            }
        }

        val sourceFiles = config.sourceFiles(className)
        val classFiles = config.classFiles(className)
        val reflClasses = config.reflectiveClasses(className)
        (sourceFiles, classFiles, reflClasses) match {
            case (List(), List(), None) => false
            case (List(), List(), Some(reflClass)) => {
                classes(className) = new Symbol.ClassFromReflection(className, reflClass)
                true
            }
            case (sourceFile :: _, List(), _) => {
                loadSourceFile(sourceFile)
                true
            }
            case (List(), classFile :: _, _) => {
                classes(className) = new Symbol.ClassFromClassFile(className, classFile)
                true
            }
            case (sourceFile :: _, classFile :: _, _) => {
                if (sourceFile.lastModified > classFile.lastModified) {
                    loadSourceFile(sourceFile)
                    true
                } else {
                    classes(className) = new Symbol.ClassFromClassFile(className, classFile)
                    true
                }
            }
        }                
    }
    
    // ___ Freshness ________________________________________________________
    
    private[this] var freshCounter = 0
    
    def freshInteger() = {
        val result = freshCounter
        freshCounter += 1
        result
    }
    
}
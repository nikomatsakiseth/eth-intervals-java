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
    
    private[this] def createSymbolsAndResolveHeader(compUnits: List[Ast.Parse.CompUnit]) {
        val newSyms = mutable.HashMap[
            Ast.Parse.CompUnit, 
            List[(Symbol.ClassFromSource, Ast.Parse.ClassDecl)]
        ]()
        
        // Create symbols for each class:
        //    We have to do this first so as to resolve 
        //    cyclic references between classes.
        compUnits.foreach { compUnit =>
            Ast.Parse.definedClasses(compUnit).foreach { case (className, cdecl) =>
                if(classes.isDefinedAt(className))
                    reporter.report(cdecl.pos, "class.already.defined", className.toString)
                else {
                    csym = new Symbol.ClassFromSource(className)
                    newSyms(compUnit) = (csym, cdecl) :: newSyms.getOrElse(compUnit, Nil)
                    classes(className) = csym
                }                
            }
        }
        
        // Resolve header for each class and enqueue
        // compilation unit for full resolution:
        newSyms.foreach { case (compUnit, classes) =>
            val resolve = Resolve(this, compUnit)
            classes.foreach { case (csym, cdecl) =>
                resolve.resolveClassHeader(csym, cdecl)
            }
            toBeResolved += compUnit
        }
    }
    
    def loadInitialSources(files: List[java.io.File]) {
        val compUnits = files.flatMap(Parse(this, _))
        createSymbolsAndResolveHeader(compUnits)
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
                    
                    // Process the loaded classes.  May trigger recursive calls to locateSource().
                    createSymbolsAndResolveHeader(List(compUnit)) 
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

    // ___ Main compile loop ________________________________________________
    
    def compile() {
        if(reporter.hasErrors) return
        
        while(!toBeResolved.isEmpty) {
            val compUnit = toBeResolved.dequeue()
            val resolve = Resolve(this, compUnit)
            resolve.resolveAllClassDecls().foreach { case cdecl =>
                val sym = classes(cdecl.name.className).asInstanceOf[Symbol.ClassFromSource]
                sym.resolvedSource = outCdecl
                toBeLowered += sym
            }
        }
        
        if(reporter.hasErrors) return
        
        while(!toBeLowered.isEmpty) {
            val csym = toBeLowered.dequeue()
            csym.loweredSource = Lower(this).lowerClassDecl(csym.resolvedSource)
            toBeBytecoded += csym
            
            if(config.dumpLoweredTrees) {
                csym.loweredSource.println(PrettyPrinter.stdout)
            }
        }
        
        if(!config.ignoreErrors && reporter.hasErrors) return
        
        while(!toBeBytecoded.isEmpty) {
            val csym = toBeBytecoded.dequeue()
            GatherOverrides(this).forSym(csym)
            ByteCode(this).writeClassSymbol(csym)
        }
    }
    
}
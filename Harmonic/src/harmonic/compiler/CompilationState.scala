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
    
    private[this] def createSymbolsAndResolve(compUnits: List[Ast.Parse.CompUnit]) {
        // Create symbols for each class:
        //    We have to do this first so as to resolve 
        //    cyclic references between classes.
        compUnits.flatMap(_.definedClasses).foreach { case (qualName, cdecl) =>
            if(classes.isDefinedAt(qualName))
                reporter.report(cdecl.pos, "class.already.defined", qualName.toString)
            else
                classes(qualName) = new Symbol.ClassFromSource(qualName)
        }
        
        // Resolve the compilation unit and store those into the symbol:
        compUnits.foreach { compUnit =>
            Resolve(this, compUnit).foreach { cdecl =>
                val sym = classes(cdecl.name.qualName).asInstanceOf[Symbol.ClassFromSource]
                sym.resolvedSource = cdecl
                toBeLowered += sym
            }
        }
    }
    
    def loadInitialSources(files: List[java.io.File]) {
        val compUnits = files.flatMap(Parse(this, _))
        createSymbolsAndResolve(compUnits)
    }
    
    /** True if a class with the name `qualName` has been
      * loaded or we can find source for it (in which case
      * that source is loaded). */
    def loadedOrLoadable(qualName: Name.Qual) = {
        classes.isDefinedAt(qualName) || locateSource(qualName)
    }
    
    /** Loads a class named `qualName`.  If it fails, reports an 
      * error and inserts a dummy into the symbol table. */
    def requireLoadedOrLoadable(pos: Position, qualName: Name.Qual) = {
        if(!loadedOrLoadable(qualName)) {
            reporter.report(pos, "cannot.find.class", qualName.toString)
            classes(qualName) = new Symbol.ClassFromErroroneousSource(qualName)
        }
    }
    
    /** Tries to locate a source for `qualName`.  If a source
      * is found, instantiates a corresponding symbol and
      * returns true.  Otherwise returns false. */
    def locateSource(qualName: Name.Qual) = {        
        
        /** When we find a reference to a source file, we parse it and load
          * the resulting classes into the symbol table.  For each class,
          * we also resolve any references it may have to other classes. */
        def loadSourceFile(file: java.io.File) {
            Parse(this, file) match {
                case None => { 
                    // Parse error:
                    classes(qualName) = new Symbol.ClassFromErroroneousSource(qualName)
                }

                case Some(compUnit) => {
                    // Check that we got (at least) the class we expected to find:
                    compUnit.definedClasses.find(_._1 == qualName) match {
                        case None => {
                            reporter.report(
                                InterPosition.forFile(file),
                                "expected.to.find.class", 
                                qualName.toString
                            )
                            classes(qualName) = new Symbol.ClassFromErroroneousSource(qualName)                            
                        }
                        
                        case Some(_) =>
                    }
                    
                    // Process the loaded classes.  May trigger recursive calls to locateSource().
                    createSymbolsAndResolve(List(compUnit)) 
                }
            }
        }

        val sourceFiles = config.sourceFiles(qualName)
        val classFiles = config.classFiles(qualName)
        val reflClasses = config.reflectiveClasses(qualName)
        (sourceFiles, classFiles, reflClasses) match {
            case (List(), List(), None) => false
            case (List(), List(), Some(reflClass)) => {
                classes(qualName) = new Symbol.ClassFromReflection(qualName, reflClass)
                true
            }
            case (sourceFile :: _, List(), _) => {
                loadSourceFile(sourceFile)
                true
            }
            case (List(), classFile :: _, _) => {
                classes(qualName) = new Symbol.ClassFromClassFile(qualName, classFile)
                true
            }
            case (sourceFile :: _, classFile :: _, _) => {
                if (sourceFile.lastModified > classFile.lastModified) {
                    loadSourceFile(sourceFile)
                    true
                } else {
                    classes(qualName) = new Symbol.ClassFromClassFile(qualName, classFile)
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
package inter.compiler

import java.io.File
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

class CompilationState(
    val config: Config,
    val reporter: Reporter
) {
    val symtab = new SymbolTable()
    val toBeLowered = new Queue[Symbol.ClassFromInterFile]()
    val inferStack = new HashSet[Symbol.MemberId]()
    val inferReported = new HashSet[Symbol.MemberId]()
    val intrinsics = new HashMap[(Type.Ref, Name.Method), List[Symbol.Method]]()
    
    def addIntrinsic(rcvrTy: Type.Ref, msym: Symbol.Method) {
        val mthdName = msym.name
        intrinsics((rcvrTy, mthdName)) = msym :: intrinsics.get((rcvrTy, mthdName)).getOrElse(Nil)
    }
    
    private[this] def createSymbolsAndResolve(compUnits: List[Ast.Parse.CompUnit]) {
        // Create symbols for each class:
        //    We have to do this first so as to resolve 
        //    cyclic references between classes.
        compUnits.flatMap(_.definedClasses).foreach { case (qualName, cdecl) =>
            if(symtab.classes.isDefinedAt(qualName))
                reporter.report(cdecl.pos, "class.already.defined", qualName.toString)
            else
                symtab.classes(qualName) = new Symbol.ClassFromInterFile(qualName)
        }
        
        // Resolve the compilation unit and store those into the symbol:
        compUnits.foreach { compUnit =>
            Resolve(this, compUnit).foreach { cdecl =>
                val sym = symtab.classes(cdecl.name.qualName).asInstanceOf[Symbol.ClassFromInterFile]
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
        println("loadedOrLoadable(%s)".format(qualName))
        symtab.classes.isDefinedAt(qualName) || locateSource(qualName)
    }
    
    /** Tries to locate a source for `qualName`.  If a source
      * is found, instantiates a corresponding symbol and
      * returns true.  Otherwise returns false. */
    def locateSource(qualName: Name.Qual) = {        
        
        /** When we find a reference to a .inter file, we parse it and load
          * the resulting classes into the symbol table.  For each class,
          * we also resolve any references it may have to other classes. */
        def loadSourceFile(file: java.io.File) {
            Parse(this, file) match {
                case None => { 
                    // Parse error:
                    symtab.classes(qualName) = new Symbol.ClassFromErroroneousSource(qualName)
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
                            symtab.classes(qualName) = new Symbol.ClassFromErroroneousSource(qualName)                            
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
                symtab.classes(qualName) = new Symbol.ClassFromReflection(qualName, reflClass)
                true
            }
            case (sourceFile :: _, List(), _) => {
                loadSourceFile(sourceFile)
                true
            }
            case (List(), classFile :: _, _) => {
                symtab.classes(qualName) = new Symbol.ClassFromClassFile(qualName, classFile)
                true
            }
            case (sourceFile :: _, classFile :: _, _) => {
                if (sourceFile.lastModified > classFile.lastModified) {
                    loadSourceFile(sourceFile)
                    true
                } else {
                    symtab.classes(qualName) = new Symbol.ClassFromClassFile(qualName, classFile)
                    true
                }
            }
        }                
    }
    
    /** Checks for an intrinsic method --- i.e., one that is built-in to the compiler ---
      * defined on the type `rcvrTy` with the name `name`. */
    def lookupIntrinsic(rcvrTy: Type.Ref, name: Name.Method): List[Symbol.Method] = {
        intrinsics.get((rcvrTy, name)).getOrElse(List())
    }
    
    def compile() {
        if(reporter.hasErrors) return
        
        while(!toBeLowered.isEmpty) {
            val csym = toBeLowered.dequeue()
            csym.loweredSource = Lower.lowerClassDecl(this, csym.resolvedSource)
        }
        
        if(reporter.hasErrors) return
    }
    
}
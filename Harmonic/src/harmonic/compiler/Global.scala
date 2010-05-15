package harmonic.compiler

import java.io.File
import scala.util.parsing.input.Position
import scala.collection.mutable
import ch.ethz.intervals.Interval
import Util._

class Global(
    val config: Config,
    val reporter: Reporter
) {
    // ___ Main Routine _____________________________________________________
    
    var master: Interval = null
    
    def compile() = {
        inlineInterval { inter =>
            master = inter
            
            // Start by parsing the input files.  This will create
            // various subintervals of master, which will begin to
            // execute once all parsing and loading is complete.
            // It is necessary for those subintervals to wait till all
            // the initial files have been loaded because they may
            // reference symbols created in one another.
            config.inputFiles.foreach { inputFile =>
                Parse(this, inputFile).foreach(createSymbols)
            }
        }
    }
    
    // ___ Private Data For Other Classes ___________________________________
    //
    // Various passes and helpers, such as Lower or MethodResolutionOrder,
    // require state that should persist within a particular compilation.
    // They store their data in the `State` using the data() method.
    
    private[this] val privateData = new mutable.HashMap[java.lang.Class[_], Any]()
    
    def data[C](cls: java.lang.Class[C]): C = synchronized {
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
    
    val intrinsics = new mutable.HashMap[(Name.Qual, Name.Method), List[MethodSymbol]]()
    
    def addIntrinsic(msym: MethodSymbol) {
        val key = (msym.clsName, msym.name)
        intrinsics(key) = msym :: intrinsics.get(key).getOrElse(Nil)
    }
    
    /** Checks for an intrinsic method --- i.e., one that is built-in to the compiler ---
      * defined on the type `rcvrTy` with the name `name`. */
    def lookupIntrinsic(className: Name.Qual, methodName: Name.Method): Option[List[MethodSymbol]] = {
        intrinsics.get((className, methodName))
    }
    
    // ___ Class Symbols ____________________________________________________
    
    /** Maps a class name to its symbol.  Only accessed under lock. */
    private[this] val classMap = 
        new mutable.HashMap[Name.Class, ClassSymbol]()
        
    def csym(name: Name.Class) = synchronized {
        classMap(name)
    }
    
    private[this] def addCsym[C <: ClassSymbol](cls: C): C = synchronized {
        classMap(cls.name) = cls
        cls
    }
    
    /** True if a class with the name `className` has been
      * loaded or we can find source for it (in which case
      * that source is loaded). */
    def loadedOrLoadable(className: Name.Class) = synchronized {
        classMap.isDefinedAt(className) || locateSource(className)
    }
    
    /** Loads a class named `className`.  If it fails, reports an 
      * error and inserts a dummy into the symbol table. */
    def requireLoadedOrLoadable(pos: Position, className: Name.Class) = synchronized {
        val result = loadedOrLoadable(className)
        if(!result) {
            reporter.report(pos, "cannot.find.class", className.toString)
            addCsym(new ClassFromErroroneousSource(className, this))
        }
        result
    }
    
    /** Creates source symbols from all class defn. in `compUnit`.
      * Also creates the header, body, lower, create, and byte-code
      * intervals for them. */
    private[this] def createSymbols(compUnit: Ast.Parse.CompUnit) = synchronized {
        Ast.Parse.definedClasses(compUnit).foreach { case (className, cdecl) =>
            classMap.get(className) match {
                case Some(_) => {
                    reporter.report(cdecl.pos, "class.already.defined", className.toString)
                }
                
                case None => {
                    val csym = addCsym(new ClassFromSource(className, this, compUnit, cdecl))
                }
            }
        }        
    }
    
    /** Tries to locate a source for `className`.  If a source
      * is found, instantiates a corresponding symbol and
      * returns true.  Otherwise returns false. */
    private[this] def locateSource(className: Name.Class) = synchronized {        
        
        /** When we find a reference to a source file, we parse it and load
          * the resulting classes into the symbol table.  For each class,
          * we also resolve any references it may have to other classes. */
        def loadSourceFile(file: java.io.File) {
            Parse(this, file) match {
                case None => { 
                    // Parse error:
                    addCsym(new ClassFromErroroneousSource(className, this))
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
                            addCsym(new ClassFromErroroneousSource(className, this))
                        }
                        
                        case Some(_) =>
                    }
                    
                    // Process the loaded classes.
                    createSymbols(compUnit) 
                }
            }
        }

        val sourceFiles = config.sourceFiles(className)
        val classFiles = config.classFiles(className)
        val reflClasses = config.reflectiveClasses(className)
        (sourceFiles, classFiles, reflClasses) match {
            case (List(), List(), None) => false
            case (List(), List(), Some(reflClass)) => {
                addCsym(new ClassFromReflection(className, this, reflClass))
                true
            }
            case (sourceFile :: _, List(), _) => {
                loadSourceFile(sourceFile)
                true
            }
            case (List(), classFile :: _, _) => {
                addCsym(new ClassFromClassFile(className, this, classFile))
                true
            }
            case (sourceFile :: _, classFile :: _, _) => {
                if (sourceFile.lastModified > classFile.lastModified) {
                    loadSourceFile(sourceFile)
                    true
                } else {
                    addCsym(new ClassFromClassFile(className, this, classFile))
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
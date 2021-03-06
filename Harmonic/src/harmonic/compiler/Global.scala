package harmonic.compiler

import java.io.File
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scala.collection.mutable
import ch.ethz.intervals.Interval
import ch.ethz.intervals.Intervals
import com.smallcultfollowing.lathos.NoneServer
import com.smallcultfollowing.lathos.LathosServer
import com.smallcultfollowing.lathos.Context
import com.smallcultfollowing.lathos.Page
import com.smallcultfollowing.lathos.JettyLathosServer
import com.smallcultfollowing.lathos.Lathos
import Util._

class Global(
    val config: Config,
    reporter: Reporter
) {
    // ___ Debugging ________________________________________________________
    
    def debugging = config.debugPort != 0

    val debugServer = 
        if(debugging) JettyLathosServer.start(config.debugPort)
        else new NoneServer()
        
    debugServer.addDefaultRenderers
    debugServer.registerPage(ch.ethz.intervals.impl.Debug.debug)

    val rootPage = debugServer.topLevelPage("Root")
    val errorsPage = debugServer.topLevelPage("Errors")
    val intervalsPage = new debug.Intervals(debugServer, rootPage)
    
    // ___ Reporting Errors _________________________________________________
    
    /** Tracks the position of the "current node" in the AST tree.
      * Used when reporting errors in the environment or other places
      * where we have lost "position" information. */
    val curPosVar = Intervals.context.scopedVar(NoPosition)
    
    def curPos = curPosVar.get
    
    def hasErrors = reporter.hasErrors
    
    def report(pos: Position, msgKey: String, msgArgs: String*): Unit = {
        val error = reporter.Error(pos, msgKey, msgArgs.toList)
        reporter.report(error)
        
        // Add to appropriate logs, cross-referencing to the current page:
        val errorLog = debugServer.contextForPage(errorsPage)
        val msgArgsString = msgArgs.mkString(", ")
        Lathos.context.log("Error ", msgKey, "(", msgArgsString, ")")
        errorLog.log("Error ", msgKey, "(", msgArgsString, ")", " at ", Lathos.context.topPage)
    }
    
    // ___ Fact propagation _________________________________________________
    
    val network = new HarmonicRulesNetwork(debugServer)
    
    // ___ Main Routine _____________________________________________________
    
    var master: Interval = null
    
    def compile() = {
        usingLog(debugServer.context) {
            try {
                val log = Lathos.context

                Util.measure("master") {
                    inlineInterval("master") { inter =>
                        master = inter

                        Intrinsic(this).add()

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
                
                val cnt = reporter.errorCount
                if(cnt > config.joinIfMore || cnt < config.joinIfLess) {
                    println("Joining debug server (%d errors): http://localhost:%d/Errors".format(cnt, config.debugPort))
                    debugServer.join            
                }
            } catch {
                case e: RuntimeException => {
                    println("Joining due to excepiton %s", e)
                    debugServer.join            
                }
            }            
        }
    }
    
    // ___ Intrinsics _______________________________________________________
    
    val intrinsics = new mutable.HashMap[(Name.Qual, Name.Method), List[VirtualMethodSymbol]]()
    
    def addIntrinsic(msym: VirtualMethodSymbol) {
        val key = (msym.className, msym.name)
        intrinsics(key) = msym :: intrinsics.get(key).getOrElse(Nil)
    }
    
    /** Checks for an intrinsic method --- i.e., one that is built-in to the compiler ---
      * defined on the type `rcvrTy` with the name `name`. */
    def lookupIntrinsic(className: Name.Qual, methodName: Name.Method): Option[List[VirtualMethodSymbol]] = {
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
            report(pos, "cannot.find.class", className.toString)
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
                    report(cdecl.pos, "class.already.defined", className.toString)
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
                            report(
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
    
    // ___ Resolving symbols ________________________________________________
    //
    // This must be done after the lowering phase of the appropriate class.
    
    def staticMethods(className: Name.Class, methodName: Name.Method): List[StaticMethodSymbol] = {
        csym(className).methodsNamed(methodName).flatMap(_.ifStatic)
    }
    
    def methodSymbol(methodId: MethodId.Virtual): Option[VirtualMethodSymbol] = {
        csym(methodId.className).methodsNamed(methodId.methodName).firstSome(_.ifId(methodId))
    }
    
    def methodSymbol(methodId: MethodId.Static): Option[StaticMethodSymbol] = {
        csym(methodId.className).methodsNamed(methodId.methodName).firstSome(_.ifId(methodId))
    }
    
    def methodSymbolOrError(methodId: MethodId.Virtual): VirtualMethodSymbol = {
        methodSymbol(methodId).getOrElse {
            Error.NoSuchMethodId(methodId).report(this)
            MethodSymbol.error(methodId)(this)
        }        
    }
    
    def methodSymbolOrError(methodId: MethodId.Static): StaticMethodSymbol = {
        methodSymbol(methodId).getOrElse {
            Error.NoSuchMethodId(methodId).report(this)
            MethodSymbol.error(methodId)(this)
        }        
    }
    
    def fieldSymbol(memberName: Name.Member) = {
        csym(memberName.className).fieldNamed(memberName)
    }
    
    def fieldSymbolOrError(memberName: Name.Member) = {
        fieldSymbol(memberName).getOrElse {
            Error.NoSuchField(memberName).report(this)
            VarSymbol.errorField(memberName, None)(this)
        }
    }
    
    def ghostSymbol(memberName: Name.Member) = {
        csym(memberName.className).ghostNamed(memberName)
    }
    
    // ___ Freshness ________________________________________________________
    
    private[this] val freshCounter = new java.util.concurrent.atomic.AtomicInteger()
    def freshInteger = freshCounter.getAndIncrement
    def freshLocalName = Name.LocalVar("<temp-%d>".format(freshInteger))
    
}
package inter.compiler

import org.objectweb.asm
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import asm.{Opcodes => O}

import Ast.{Lower => in}

/** The final step in compilation: generates appropriate 
  * .class files for each class.
  *
  * Given a class Foo, the following class files are generated:
  * - Foo.class: a Java interface defining all members of Foo
  * - Foo$.class: a Java class defining static members for all 
  *   of the method definitions in Foo.  It also contains instance
  *   members making it suitable for use for any new statement
  *   that does not define new members.
  */
class ByteCode(state: CompilationState) {
    
    val interSuffix = ""
    val implSuffix = "$"
    
    // ___ Types and Asm Types ______________________________________________
    
    val asmObjectArrayType = asm.Type.getType("[Ljava/lang/Object;")
    val asmObjectType = asm.Type.getObjectType("java/lang/Object")
    
    def asmType(ty: Type.Ref) = ty match {
        case Type.Class(name, List()) => asm.Type.getObjectType(name.internalName)
        case Type.Tuple(_) => asmObjectArrayType
        case _ => asmObjectType
    }
    
    def methodDesc(returnTy: Type.Ref, parameterPatterns: List[Pattern.Ref]): String = {
        def types(pattern: Pattern.Ref): List[Type.Ref] = pattern match {
            case Pattern.Var(_, ty) => List(ty)
            case Pattern.Tuple(patterns) => patterns.flatMap(types)
        }
        
        val parameterTypes = parameterPatterns.flatMap(types)
        asm.Type.getMethodDescriptor(
            asmType(returnTy),
            parameterTypes.map(asmType).toArray
        )
    }
    
    sealed case class ExtendedVisitor(mvis: asm.MethodVisitor) {
        def pushIntegerConstant(value: Int) = value match {
            case 0 => mvis.visitInsn(O.ICONST_0)
            case 1 => mvis.visitInsn(O.ICONST_1)
            case 2 => mvis.visitInsn(O.ICONST_2)
            case 3 => mvis.visitInsn(O.ICONST_3)
            case _ => mvis.visitLdcInsn(value)
        }
        
        def downcast(ty: Type.Ref) = ty match {
            case Type.Class(name, List()) => mvis.visitTypeInsn(O.CHECKCAST, name.internalName)
            case Type.Tuple(_) => mvis.visitTypeInsn(O.CHECKCAST, asmObjectArrayType.getInternalName)
            case Type.Var(_, _) | Type.Null => 
        }
    }
    implicit def extendedVisitor(mvis: asm.MethodVisitor) = ExtendedVisitor(mvis)
    
    // ___ Intrinsics _______________________________________________________
    
    
    
    // ___ Access Paths and Maps ____________________________________________
    //
    // An AccessPath encodes the route to obtain the value for some local
    // variable.  This value may be present on the stack or it may require
    // dereferences through the fields of other objects to be loaded.
    
    sealed abstract class AccessPath {
        /** Type of the value at the other end of this path. */
        def asmType: asm.Type
        
        def push(mvis: asm.MethodVisitor): Unit
        def pushLvalue(mvis: asm.MethodVisitor): Unit
        def storeLvalue(mvis: asm.MethodVisitor): Unit
    }
    
    sealed case class AccessVar(
        index: Int, 
        asmType: asm.Type
    ) extends AccessPath {
        def push(mvis: asm.MethodVisitor) {
            mvis.visitIntInsn(O.ALOAD, index)            
        }
        
        def pushLvalue(mvis: asm.MethodVisitor) {
        }
        
        def storeLvalue(mvis: asm.MethodVisitor) {
            mvis.visitIntInsn(O.ASTORE, index)            
        }
    }
    
    sealed case class AccessIndex(
        owner: AccessPath,
        index: Int,
        asmType: asm.Type
    ) extends AccessPath {
        def push(mvis: asm.MethodVisitor) {
            owner.push(mvis)
            mvis.pushIntegerConstant(index)
            mvis.visitInsn(O.AALOAD)
            mvis.visitTypeInsn(O.CHECKCAST, asmType.getInternalName)
        }
        
        def pushLvalue(mvis: asm.MethodVisitor) {
            owner.push(mvis)
            mvis.pushIntegerConstant(index)
        }
        
        def storeLvalue(mvis: asm.MethodVisitor) {
            mvis.visitInsn(O.AASTORE)
        }        
    }
    
    sealed case class AccessField(
        owner: AccessPath,
        name: String,
        asmType: asm.Type
    ) extends AccessPath {
        def push(mvis: asm.MethodVisitor) {
            owner.push(mvis)
            mvis.visitFieldInsn(
                O.GETFIELD, 
                owner.asmType.getInternalName,
                name,
                asmType.getDescriptor
            )
        }
        
        def pushLvalue(mvis: asm.MethodVisitor) {
            owner.push(mvis)
        }
        
        def storeLvalue(mvis: asm.MethodVisitor) {
            mvis.visitFieldInsn(
                O.PUTFIELD, 
                owner.asmType.getInternalName,
                name,
                asmType.getDescriptor
            )
        }        
    }
    
    case class AccessMap(
        syms: Map[Symbol.Var, AccessPath],
        boxedArrayPath: AccessPath,
        stashSlot: Int,
        maxSlot: Int,
        maxIndex: Int
    ) {
        
        def addUnboxedSym(sym: Symbol.Var) = {
            copy(
                syms = syms + (sym -> AccessVar(maxSlot, asmType(sym.ty))),
                maxSlot = maxSlot + 1
            )
        }
        
        def addBoxedSym(sym: Symbol.Var) = {
            copy(
                syms = syms + (sym -> AccessIndex(boxedArrayPath, maxIndex, asmType(sym.ty))),
                maxIndex = maxIndex + 1
            )
        }
        
    }
    
    object AccessMap {
        def apply(parameterSymbols: List[Symbol.Var]) = {
            var accessMap = new AccessMap(Map(), AccessVar(0, asmObjectArrayType), 0, 0, 0)
            accessMap = parameterSymbols.foldLeft(accessMap)(_ addUnboxedSym _)
            accessMap.copy(
                boxedArrayPath = AccessVar(accessMap.maxSlot, asmObjectArrayType),
                stashSlot = accessMap.maxSlot + 1,
                maxSlot = accessMap.maxSlot + 2
            )
        }
    }
    
    // ___ Symbol Summarizing _______________________________________________
    //
    // A "symbol summary" is a listing of all the symbols created within a 
    // method body.  
    
    case class SymbolSummary(
        // Symbols that are declared:
        declaredSyms: Set[Symbol.Var],
        
        // Symbols that are read:
        //    (Not necessarily a subset of declaredSyms.)
        readSyms: Set[Symbol.Var],
        
        // Symbols that are re-assigned:
        //    (Not necessarily a subset of declaredSyms.)
        writeSyms: Set[Symbol.Var],
        
        // Symbols which are accessed from an inner class of some kind: 
        //    (A subset of readSyms and writeSyms)
        sharedSyms: Set[Symbol.Var]
    ) {
        def boxedSyms(sym: Symbol.Var) = writeSyms(sym) && sharedSyms(sym)
    }
    
    object SymbolSummary {
        val empty = SymbolSummary(Set(), Set(), Set(), Set())
    }

    def symbolsReassignedInLvalue(lvalue: in.Pattern[in.OptionalTypeRef]): List[Symbol.Var] = lvalue match {
        case in.TuplePattern(lvalues, _) => lvalues.flatMap(symbolsReassignedInLvalue)
        case in.VarPattern(_, in.InferredTypeRef(), _, sym) => List(sym)
        case in.VarPattern(_, _, _, _) => Nil
    }
    
    def symbolsDeclaredInLvalue(lvalue: in.Pattern[in.OptionalTypeRef]): List[Symbol.Var] = lvalue match {
        case in.TuplePattern(lvalues, _) => lvalues.flatMap(symbolsDeclaredInLvalue)
        case in.VarPattern(_, in.InferredTypeRef(), _, _) => Nil
        case in.VarPattern(_, _, _, sym) => List(sym)
    }
    
    def summarizeSymbolsInExpr(summary: SymbolSummary, expr: in.Expr): SymbolSummary = {
        expr match {
            case in.Tuple(exprs, _) => exprs.foldLeft(summary)(summarizeSymbolsInExpr)
            case tmpl: in.Tmpl => {
                val summaryTmpl = summarizeSymbolsInStmts(tmpl.stmts)
                summary.copy(
                    readSyms = summary.readSyms ++ summaryTmpl.readSyms,
                    writeSyms = summary.writeSyms ++ summaryTmpl.writeSyms,
                    sharedSyms = summary.sharedSyms ++ summaryTmpl.readSyms ++ summaryTmpl.writeSyms
                )
            }
            case in.Literal(_, _) => summary
            case in.Var(_, sym, _) => summary.copy(readSyms = summary.readSyms + sym)
            case in.Field(owner, _, _, _) => summarizeSymbolsInExpr(summary, owner)
            case in.MethodCall(receiver, parts, _, _) => {
                val receiverSummary = summarizeSymbolsInExpr(summary, receiver)
                parts.map(_.arg).foldLeft(receiverSummary)(summarizeSymbolsInExpr)
            }
            case in.NewJava(_, arg, _) => summarizeSymbolsInExpr(summary, arg)
            case in.Null(_) => summary
            case in.ImpVoid(_) => summary /* XXX: Refine types so that ImpThis no longer appears */
            case in.ImpThis(_) => summary /* XXX: Refine types so that ImpThis no longer appears */
        }
    }
    
    def summarizeSymbolsInStmt(summary: SymbolSummary, stmt: in.Stmt): SymbolSummary = {
        stmt match {
            case expr: in.Expr => summarizeSymbolsInExpr(summary, expr)
            
            case in.Labeled(name, in.Body(stmts)) => {
                stmts.foldLeft(summary)(summarizeSymbolsInStmt)
            }
            
            case in.Assign(lvalue, expr) => {
                summarizeSymbolsInExpr(summary, expr).copy(
                    declaredSyms = summary.declaredSyms ++ symbolsDeclaredInLvalue(lvalue),
                    writeSyms = summary.writeSyms ++ symbolsReassignedInLvalue(lvalue)
                )
            }
        }
    }
    
    def summarizeSymbolsInStmts(stmts: List[in.Stmt]) = {
        stmts.foldLeft(SymbolSummary.empty)(summarizeSymbolsInStmt)
    }
    
    // ___ Method Bodies ____________________________________________________
    
    def constructAccessMap(
        receiverSymbol: Symbol.Var, 
        parameterPatterns: List[in.Pattern[in.TypeRef]], 
        stmts: List[in.Stmt]
    ) = {
        val accessMapParams = AccessMap(receiverSymbol :: parameterPatterns.flatMap(_.symbols))
        val summary = summarizeSymbolsInStmts(stmts)
        summary.declaredSyms.foldLeft(accessMapParams) {
            case (am, sym) if summary.boxedSyms(sym) => am.addBoxedSym(sym)
            case (am, sym) => am.addUnboxedSym(sym)
        }
    }
    
    class StatementVisitor(accessMap: AccessMap, mvis: asm.MethodVisitor) {
        
        def evalExpr()
        
        /** Evaluates `expr`, pushing the result onto the stack.
          *
          * Stack: ... => ..., value
          */
        def evalExpr(expr: in.Expr) {
            expr match {
                case in.Tuple(List(), _) => {
                    mvis.visitInsn(O.ACONST_NULL)
                }
                
                case in.Tuple(List(expr), _) => {
                    evalExpr(expr)
                }
                
                case in.Tuple(exprs, _) => {
                    mvis.pushIntegerConstant(exprs.length)
                    mvis.visitTypeInsn(O.ANEWARRAY, asmObjectType.getInternalName)
                    exprs.zipWithIndex.foreach { case (expr, index) =>
                        mvis.visitInsn(O.DUP)
                        mvis.pushIntegerConstant(index)         
                        mvis.visitInsn(O.AASTORE)
                    }
                }
                
                case in.InlineTmpl(stmts, _) => {
                    throw new RuntimeException("TODO")
                }
                
                case in.AsyncTmpl(stmts, _) => {
                    throw new RuntimeException("TODO")                    
                }
                
                case in.Literal(obj, _) => {
                    mvis.visitLdcInsn(obj)
                }
                
                case in.Var(name, sym, _) => {
                    accessMap.syms(sym).push(mvis)
                }
                
                case in.Field(owner, name, sym, _) => {
                    evalExpr(owner)
                }
                
                case in.MethodCall(receiver, parts, sym, _) => {
                    def callWithOpcode(op: Int) = {
                        evalExpr(receiver)
                        parts.foreach(p => evalExpr(p.arg))
                        mvis.visitMethodInsn(op, owner, sym.name.javaName, desc)
                    }
                    
                    sym.kind match {
                        case Symbol.IntrinsicMath => {
                            
                        }
                        
                        case Symbol.Inter => callWithOpcode(O.INVOKEINTERFACE)
                        case Symbol.JavaVirtual => callWithOpcode(O.INVOKEVIRTUAL)
                        case Symbol.JavaInterface => callWithOpcode(O.INVOKEINTERFACE)
                        case Symbol.JavaStatic => callWithOpcode(O.INVOKESTATIC)
                        
                        case Symbol.ErrorMethod => {
                            throw new RuntimeException("TODO")
                        }
                    }
                }
                
                case in.NewJava(tref, arg, ty) => {
                    throw new RuntimeException("TODO")                    
                }
                
                case in.ImpVoid(_) | in.Null(_) => {
                    mvis.visitInsn(O.ACONST_NULL)                    
                }
                
                case in.ImpThis(_) => {
                    mvis.visitIntInsn(O.ALOAD, 0)
                }
            }
        }
        
        /** Generates the instructionso to store to an lvalue, unpacking
          * tuple values as needed.  
          *
          * Stack: ..., value => ...
          */
        def storeLvalue(lvalue: in.Pattern[_]) {
            lvalue match {
                case in.TuplePattern(List(lvalue), _) => {
                    storeLvalue(lvalue)
                }
                
                case in.TuplePattern(lvalues, Type.Tuple(tys)) => {
                    lvalues.zip(tys).zipWithIndex.foreach { case ((lvalue, ty), idx) =>
                        mvis.visitInsn(O.DUP)
                        mvis.pushIntegerConstant(idx)
                        mvis.visitInsn(O.AALOAD)
                        mvis.downcast(ty)
                        storeLvalue(lvalue)
                    }
                    mvis.visitInsn(O.POP)
                }
                
                case in.VarLvalue(_, _, _, sym) => {
                    mvis.visitIntInsn(O.ASTORE, accessMap.stashSlot)
                    val accessPath = accessMap.syms(sym)
                    accessPath.pushLvalue(mvis)
                    mvis.visitIntInsn(O.ALOAD, accessMap.stashSlot)
                    accessPath.storeLvalue(mvis)
                }
            }
        }

        /** Executes `stmt`.  Stack is unaffected.
          *
          * Stack: ... => ...
          */
        def execStatement(stmt: in.Stmt) {
            stmt match {
                case expr: in.Expr => evalExpr(expr); mvis.visitInsn(O.POP)

                case in.Labeled(name, in.Body(stmts)) => {
                    stmts.foreach(execStatement) // XXX Not really right.
                }

                case in.Assign(in.VarLvalue(_, _, _, sym), expr) => {
                    val accessPath = accessMap.syms(sym)
                    accessPath.pushLvalue(mvis)
                    evalExpr(expr)
                    accessPath.storeLvalue(mvis)
                }

                case in.Assign(lvalue, expr) => {
                    evalExpr(expr)
                    storeLvalue(lvalue)
                }
            }
        }

    }
    
    // ___ Tracing __________________________________________________________
    
    def file(qualName: Name.Qual, suffix: String, ext: String) = {
        val relPath = qualName.components.mkString("/") + suffix + ext
        new java.io.File(state.config.outputDir, relPath)
    }

    def trace(qualName: Name.Qual, suffix: String, cvis: asm.ClassVisitor) = {
        if(!state.config.dumpBytecode) {
            (cvis, None)
        } else {
            val sFile = file(qualName, suffix, ".s")
            try {
                val writer = new java.io.FileWriter(sFile)
                (new asm.util.TraceClassVisitor(cvis, new java.io.PrintWriter(writer)), Some(writer))
            } catch {
                case err: java.io.IOException => {
                    println("Error writing to %s: %s".format(sFile, err))
                    (cvis, None)
                }
            }
            
        }
    }
    
    // ___ Classes __________________________________________________________
    
    class ClassVisitors(csym: Symbol.ClassFromInterFile) {
        val interWriter = new asm.ClassWriter(0)
        val implWriter = new asm.ClassWriter(0)
        
        val (interTracer, interTraceWriter) = trace(csym.name, interSuffix, interWriter)
        val (implTracer, implTraceWriter) = trace(csym.name, implSuffix, implWriter)
        
        val inter = interTracer
        val impl = implTracer
        
        inter.visit(
            O.V1_5,
            O.ACC_ABSTRACT + O.ACC_INTERFACE + O.ACC_PUBLIC,
            csym.name.internalName,
            null, // XXX Signature
            "java/lang/Object",
            csym.superClassNames(state).map(_.internalName).toArray
        )
        
        impl.visit(
            O.V1_5,
            O.ACC_PUBLIC,
            csym.name.internalName + "$",
            null, // XXX Signature
            "java/lang/Object",
            Array(csym.name.internalName)
        )
        
        def visitMethod(mdecl: in.MethodDecl) = {
            val minter = inter.visitMethod(
                O.ACC_PUBLIC,
                mdecl.name.javaName,
                methodDesc(mdecl.returnTy, mdecl.parts.map(_.pattern)),
                null, // generic signature
                null  // thrown exceptions
            )
            
            val thisPattern = Pattern.Var(Name.This, Type.Class(csym.name, List()))
            val mimpl = impl.visitMethod(
                O.ACC_PUBLIC + O.ACC_STATIC,
                mdecl.name.javaName,
                methodDesc(mdecl.returnTy, thisPattern :: mdecl.parts.map(_.pattern)),
                null, // generic signature
                null  // thrown exceptions                
            )
            
            mimpl.visitEnd
            minter.visitEnd
        }
        
        private[this] def writeClassFile(
            qualName: Name.Qual, 
            suffix: String, 
            classWriter: asm.ClassWriter
        ) = {
            val clsFile = file(qualName, suffix, ".class")
            try {
                val out = new java.io.FileOutputStream(clsFile)
                out.write(classWriter.toByteArray)
                out.close()
            } catch {
                case err: java.io.IOError => {
                    state.reporter.report(
                        InterPosition.forFile(clsFile),
                        "io.error",
                        err.toString
                    )
                }
            }
        }

        def end {
            inter.visitEnd
            impl.visitEnd
            interTraceWriter.foreach(_.close)
            implTraceWriter.foreach(_.close)
            writeClassFile(csym.name, interSuffix, interWriter)
            writeClassFile(csym.name, implSuffix, implWriter)
        }
    }
    
    def writeClassSymbol(csym: Symbol.ClassFromInterFile) = {
        // XXX Need to generate only a class if we extend a Java class.
        val visitors = new ClassVisitors(csym)
    }
    
}
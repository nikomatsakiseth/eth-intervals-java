package inter.compiler

import org.objectweb.asm
import scala.collection.mutable
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import asm.{Opcodes => O}

import Ast.{Lower => in}
import Util._

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
    
    def methodDesc(returnTy: Type.Ref, parameterTypes: List[Type.Ref]): String = {
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
    
    class AccessMap 
    {
        val syms = new mutable.HashMap[Symbol.Var, AccessPath]()
        private[this] var maxSlot = 0
        
        def pathToFreshSlot(asmTy: asm.Type) = {
            val slot = maxSlot
            maxSlot += 1
            AccessVar(slot, asmTy)
        }

        def addUnboxedSym(sym: Symbol.Var) = {
            syms(sym) = pathToFreshSlot(asmType(sym.ty))
        }
        
        def withStashSlot(func: (Int => Unit)) {
            val stashSlot = maxSlot
            maxSlot += 1
            func(stashSlot)
            maxSlot -= 1
        }
    }
    
    class BoxedArray(accessMap: AccessMap)
    {
        private[this] val boxedArrayPath = accessMap.pathToFreshSlot(asmObjectArrayType)
        private[this] var maxIndex = 0
        
        def addBoxedSym(sym: Symbol.Var) = {
            accessMap.syms(sym) = AccessIndex(boxedArrayPath, maxIndex, asmType(sym.ty))
            maxIndex += 1
        }
        
        def createArray(mvis: asm.MethodVisitor) = {
            boxedArrayPath.pushLvalue(mvis)
            mvis.pushIntegerConstant(maxIndex)
            mvis.visitTypeInsn(O.ANEWARRAY, asmObjectType.getInternalName)
            boxedArrayPath.storeLvalue(mvis)
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

    def symbolsReassignedInLocal(local: in.Local): List[Symbol.Var] = local match {
        case in.TupleLocal(locals) => locals.flatMap(symbolsReassignedInLocal)
        case in.VarLocal(_, in.InferredTypeRef(), _, sym) => List(sym)
        case in.VarLocal(_, _, _, _) => Nil
    }
    
    def symbolsDeclaredInLocal(local: in.Local): List[Symbol.Var] = local match {
        case in.TupleLocal(locals) => locals.flatMap(symbolsDeclaredInLocal)
        case in.VarLocal(_, in.InferredTypeRef(), _, _) => Nil
        case in.VarLocal(_, _, _, sym) => List(sym)
    }
    
    def summarizeSymbolsInExpr(summary: SymbolSummary, expr: in.Expr): SymbolSummary = {
        expr match {
            case in.Tuple(exprs) => exprs.foldLeft(summary)(summarizeSymbolsInExpr)
            case tmpl: in.Tmpl => {
                val summaryTmpl = summarizeSymbolsInStmts(tmpl.stmts)
                summary.copy(
                    readSyms = summary.readSyms ++ summaryTmpl.readSyms,
                    writeSyms = summary.writeSyms ++ summaryTmpl.writeSyms,
                    sharedSyms = summary.sharedSyms ++ summaryTmpl.readSyms ++ summaryTmpl.writeSyms
                )
            }
            case in.Literal(_, _) => summary
            case in.Var(_, sym) => summary.copy(readSyms = summary.readSyms + sym)
            case in.Field(owner, _, _, _) => summarizeSymbolsInExpr(summary, owner)
            case in.MethodCall(receiver, parts, _) => {
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
            
            case in.Assign(local, expr) => {
                summarizeSymbolsInExpr(summary, expr).copy(
                    declaredSyms = summary.declaredSyms ++ symbolsDeclaredInLocal(local),
                    writeSyms = summary.writeSyms ++ symbolsReassignedInLocal(local)
                )
            }
        }
    }
    
    def summarizeSymbolsInStmts(stmts: List[in.Stmt]) = {
        stmts.foldLeft(SymbolSummary.empty)(summarizeSymbolsInStmt)
    }
    
    // ___ Method Bodies ____________________________________________________
    
    def constructAccessMap(
        mvis: asm.MethodVisitor,
        receiverSymbol: Symbol.Var, 
        parameterPatterns: List[in.Param],
        stmts: List[in.Stmt]
    ) = {
        val accessMap = new AccessMap()
        (receiverSymbol :: parameterPatterns.flatMap(_.symbols)).foreach(accessMap.addUnboxedSym)
        val boxedArray = new BoxedArray(accessMap)
        val summary = summarizeSymbolsInStmts(stmts)
        summary.declaredSyms.foreach { sym =>
            if(summary.boxedSyms(sym)) boxedArray.addBoxedSym(sym)
            else accessMap.addUnboxedSym(sym)
        }
        boxedArray.createArray(mvis)
    }
    
    class StatementVisitor(accessMap: AccessMap, mvis: asm.MethodVisitor) {
        
        /** Generates the instructions to store to an lvalue, unpacking
          * tuple values as needed.  
          *
          * Stack: ..., value => ...
          */
        def store(lvalue: in.Lvalue, rvalue: in.Expr) {
            lvalue match {
                case in.TupleLvalue(List(sublvalue), _) => {
                    store(sublvalue, rvalue)
                }
                
                case in.VarLvalue(_, _, _, sym) => {
                    // Micro-optimize generated code to avoid using stashSlot:
                    val accessPath = accessMap.syms(sym)
                    accessPath.pushLvalue(mvis)
                    pushExprValue(rvalue)
                    accessPath.storeLvalue(mvis)
                }
                
                case _ => {
                    pushRvalues(in.toPattern(lvalue), rvalue)
                    popRvalues(lvalue, rvalue)
                }
            }
        }

        /** Evaluates `expr` to a form suitable for being stored
          * into `lvalue`.  Values for each variable in lvalue 
          * are pushed in pre-order. In other words, if `lvalue` were a 
          * pattern like `((a, b), c)`, then the values for `a`, `b`, 
          * and `c` would be pushed in that order.
          */
        def pushRvalues(lvalue: Pattern.Anon, rvalue: in.Expr) {
            (lvalue, rvalue) match {
                case (Pattern.AnonTuple(List(sublvalue)), _) =>
                    pushRvalues(sublvalue, rvalue)
                    
                case (_, in.Tuple(List(subexpr))) =>
                    pushRvalues(lvalue, subexpr)
                    
                case (Pattern.AnonTuple(sublvalues), in.Tuple(subexprs)) 
                if sameLength(sublvalues, subexprs) =>
                    sublvalues.zip(subexprs).foreach { case (l, e) => pushRvalues(l, e) }
                    
                case _ => {
                    pushExprValue(rvalue)
                    expand(lvalue)                    
                }
            }
        }
        
        /** Pops the rvalues which were pushed by `pushRvalues(lvalue)(rvalue)`,
          * storing them into `lvalue`. */
        def popRvalues(lvalue: in.Lvalue, rvalue: in.Expr) {
            (lvalue, rvalue) match {
                case (in.TupleLvalue(List(sublvalue), _), _) =>
                    popRvalues(sublvalue, rvalue)
                    
                case (_, in.Tuple(List(subexpr))) =>
                    popRvalues(lvalue, subexpr)
                    
                case (in.TupleLvalue(sublvalues, _), in.Tuple(subexprs)) 
                if sameLength(sublvalues, subexprs) =>
                    sublvalues.zip(subexprs).reverse.foreach { case (l, e) => popRvalues(l, e) }
                    
                case _ => {
                    contract(lvalue)                    
                }
            }
        }
        
        def expand(lvalue: Pattern.Anon) {
            lvalue match {
                case Pattern.AnonTuple(sublvalues) => {
                    accessMap.withStashSlot { stashSlot =>
                        mvis.visitIntInsn(O.ASTORE, stashSlot) // Stack: ...
                        sublvalues.zipWithIndex.foreach { case (sublvalue, idx) =>
                            mvis.visitIntInsn(O.ALOAD, stashSlot) // Stack: ..., array
                            mvis.pushIntegerConstant(idx) // Stack: ..., array, index
                            mvis.visitInsn(O.AALOAD) // Stack: ..., array[index]
                            mvis.downcast(sublvalue.ty) // Stack: ..., array[index]
                            expand(sublvalue) 
                        }
                        // Stack: ..., array[0], ..., array[N]
                    }
                }

                case _: Pattern.AnonVar => 
            }
        }
        
        def contract(lvalue: in.Lvalue) {
            lvalue match {
                case in.TupleLvalue(sublvalues, _) => {
                    sublvalues.reverse.foreach(contract)
                }

                case in.VarLvalue(_, _, _, sym) => {
                    accessMap.withStashSlot { stashSlot =>
                        // Stack: ..., value
                        mvis.visitIntInsn(O.ASTORE, stashSlot) // Stack: ...
                        val accessPath = accessMap.syms(sym)
                        accessPath.pushLvalue(mvis)
                        mvis.visitIntInsn(O.ALOAD, stashSlot) // Stack: ..., value
                        accessPath.storeLvalue(mvis)
                    }
                }
            }
        }
        
        /** Evaluates `expr`, pushing the result onto the stack.
          *
          * Stack: ... => ..., value
          */
        def pushExprValue(expr: in.Expr) {
            expr match {
                case in.Tuple(List()) => {
                    mvis.visitInsn(O.ACONST_NULL)
                }
                
                case in.Tuple(List(expr)) => {
                    pushExprValue(expr)
                }
                
                case in.Tuple(exprs) => {
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
                
                case in.Var(name, sym) => {
                    accessMap.syms(sym).push(mvis)
                }
                
                case in.Field(owner, name, sym, _) => {
                    pushExprValue(owner)
                }
                
                case in.MethodCall(receiver, parts, (sym, msig)) => {
                    def callWithOpcode(op: Int) = {
                        pushExprValue(receiver)
                        msig.parameterPatterns.zip(parts).foreach { case (pattern, part) =>
                            pushRvalues(pattern, part.arg)
                        }
                        //mvis.visitMethodInsn(op, owner, sym.name.javaName, desc)
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
        
        /** Executes `stmt`.  Stack is unaffected.
          *
          * Stack: ... => ...
          */
        def execStatement(stmt: in.Stmt) {
            stmt match {
                case expr: in.Expr => {
                    pushExprValue(expr)
                    mvis.visitInsn(O.POP)
                }

                case in.Labeled(name, in.Body(stmts)) => {
                    stmts.foreach(execStatement) // XXX Not really right.
                }

                case in.Assign(lvalue, rvalue) => {
                    store(lvalue, rvalue)
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
            val paramTys = mdecl.params.map(_.ty)
            
            val minter = inter.visitMethod(
                O.ACC_PUBLIC,
                mdecl.name.javaName,
                methodDesc(mdecl.returnTy, paramTys),
                null, // generic signature
                null  // thrown exceptions
            )
            
            val mimpl = impl.visitMethod(
                O.ACC_PUBLIC + O.ACC_STATIC,
                mdecl.name.javaName,
                methodDesc(mdecl.returnTy, Type.Class(csym.name, List()) :: paramTys),
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
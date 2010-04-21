package inter.compiler

import org.objectweb.asm
import org.objectweb.asm.Type.getMethodDescriptor
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
case class ByteCode(state: CompilationState) {
    
    val noSuffix = ""
    val implSuffix = "$"
    
    // ___ Generating fresh, unique class names _____________________________
    
    def freshQualName(context: Name.Qual) = {
        context.withSuffix("$" + state.freshInteger())
    }
    
    def freshVarName(base: Option[Name.Var]) = {
        Name.Var("$%s$%s".format(base.getOrElse(""), state.freshInteger()))
    }
    
    // ___ Types and Asm Types ______________________________________________
    
    val asmObjectArrayType = asm.Type.getType("[Ljava/lang/Object;")
    val asmObjectType = asm.Type.getObjectType("java/lang/Object")
    val asmVoidType = asm.Type.getObjectType("java/lang/Void")
    val asmBooleanType = asm.Type.getObjectType("java/lang/Boolean")
    
    val primitives = Map[java.lang.Class[_], asm.Type](
        (classOf[java.lang.Boolean] -> asm.Type.BOOLEAN_TYPE),
        (classOf[java.lang.Byte] -> asm.Type.BYTE_TYPE),
        (classOf[java.lang.Character] -> asm.Type.CHAR_TYPE),
        (classOf[java.lang.Short] -> asm.Type.SHORT_TYPE),
        (classOf[java.lang.Integer] -> asm.Type.INT_TYPE),
        (classOf[java.lang.Long] -> asm.Type.LONG_TYPE),
        (classOf[java.lang.Float] -> asm.Type.FLOAT_TYPE),
        (classOf[java.lang.Double] -> asm.Type.DOUBLE_TYPE)
    )
    
    def asmType(ty: Type.Ref) = ty match {
        case Type.Class(name, List()) => asm.Type.getObjectType(name.internalName)
        case Type.Tuple(_) => asmObjectArrayType
        case _ => asmObjectType
    }
    
    def asmClassType(name: Name.Qual) = asm.Type.getObjectType(name.internalName)
    
    def methodDesc(returnTy: Type.Ref, parameterTypes: List[Type.Ref]): String = {
        getMethodDescriptor(
            asmType(returnTy),
            parameterTypes.map(asmType).toArray
        )
    }
    
    def methodDesc(msig: Symbol.MethodSignature[Pattern.Anon]): String = {
        methodDesc(
            msig.returnTy,
            msig.parameterPatterns.flatMap(_.varTys)
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
    
    // ___ Writing .class, .s files _________________________________________
    
    class ClassWriter(qualName: Name.Qual, suffix: String) 
    {
        private[this] def fileWithExtension(ext: String) = {
            val relPath = qualName.components.mkString("/") + suffix + ext
            new java.io.File(state.config.outputDir, relPath)
        }

        private[this] def trace(cvis: asm.ClassVisitor) = {
            if(!state.config.dumpBytecode) {
                (cvis, None)
            } else {
                val sFile = fileWithExtension(".s")
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

        val writer = new asm.ClassWriter(0)
        val (cvis, optTraceWriter) = trace(writer)

        def end() {
            cvis.visitEnd()
            optTraceWriter.foreach(_.close())
            
            val clsFile = fileWithExtension(".class")
            try {
                clsFile.getParentFile.mkdirs()
                val out = new java.io.FileOutputStream(clsFile)
                out.write(writer.toByteArray)
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
    }
    
    // ___ Access Paths and Maps ____________________________________________
    //
    // An AccessPath encodes the route to obtain the value for some local
    // variable.  This value may be present on the stack or it may require
    // dereferences through the fields of other objects to be loaded.
    
    sealed abstract class AccessPath {
        /** Type of the value at the other end of this path. */
        def asmType: asm.Type
        
        // Stack: ... => ..., value
        def push(mvis: asm.MethodVisitor): Unit
        
        // Stack: ... => ..., <lvalue>
        def pushLvalue(mvis: asm.MethodVisitor): Unit
        
        // Stack: ..., <lvalue>, val => ...
        def storeLvalue(mvis: asm.MethodVisitor): Unit

        // Stack: ..., val => ...
        def storeLvalueWithoutPush(mvis: asm.MethodVisitor): Unit
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
            storeLvalueWithoutPush(mvis)
        }
        
        def storeLvalueWithoutPush(mvis: asm.MethodVisitor) {
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
        
        def storeLvalueWithoutPush(mvis: asm.MethodVisitor) {
            owner.push(mvis)
            mvis.visitInsn(O.SWAP)
            mvis.pushIntegerConstant(index)
            mvis.visitInsn(O.SWAP)
            storeLvalue(mvis)
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
         
        def storeLvalueWithoutPush(mvis: asm.MethodVisitor) {
            owner.push(mvis)
            mvis.visitInsn(O.SWAP)
            storeLvalue(mvis)
        } 
    }
    
    class AccessMap(val context: Name.Qual)
    {
        val syms = new mutable.HashMap[Symbol.Var, AccessPath]()
        private[this] var maxSlot = 0
        
        def pathToFreshSlot(asmTy: asm.Type) = {
            val slot = maxSlot
            maxSlot += 1
            AccessVar(slot, asmTy)
        }
        
        def addSym(sym: Symbol.Var, accessPath: AccessPath) {
            syms(sym) = accessPath
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
        
        def createArrayIfNeeded(mvis: asm.MethodVisitor) = {
            if(maxIndex > 0) {
                boxedArrayPath.pushLvalue(mvis)
                mvis.pushIntegerConstant(maxIndex)
                mvis.visitTypeInsn(O.ANEWARRAY, asmObjectType.getInternalName)
                boxedArrayPath.storeLvalue(mvis)                
            }
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
        def accessSyms = readSyms ++ writeSyms
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
            case tmpl: in.Block => {
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
        }
    }
    
    def summarizeSymbolsInStmt(summary: SymbolSummary, stmt: in.Stmt): SymbolSummary = {
        stmt match {
            case expr: in.Expr => summarizeSymbolsInExpr(summary, expr)
            
            case in.Labeled(name, in.Body(stmts)) => {
                stmts.foldLeft(summary)(summarizeSymbolsInStmt)
            }
            
            case in.Assign(local, expr) => {
                val summaryExpr = summarizeSymbolsInExpr(summary, expr)
                summaryExpr.copy(
                    declaredSyms = summaryExpr.declaredSyms ++ symbolsDeclaredInLocal(local),
                    writeSyms = summaryExpr.writeSyms ++ symbolsReassignedInLocal(local)
                )
            }
        }
    }
    
    def summarizeSymbolsInStmts(stmts: List[in.Stmt]) = {
        stmts.foldLeft(SymbolSummary.empty)(summarizeSymbolsInStmt)
    }
    
    // ___ Statements _______________________________________________________
    
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
                    // Stack: ..., value
                    val accessPath = accessMap.syms(sym)
                    accessPath.storeLvalueWithoutPush(mvis)
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
                        pushExprValue(expr)
                        mvis.visitInsn(O.AASTORE)
                    }
                }
                
                case tmpl: in.Block => {
                    pushAnonymousBlock(tmpl)
                }
                
                case in.Literal(obj: java.lang.String, _) => {
                    mvis.visitLdcInsn(obj)
                }
                
                case in.Literal(java.lang.Boolean.TRUE, _) => {
                    mvis.visitFieldInsn(
                        O.GETSTATIC, 
                        asmBooleanType.getInternalName,
                        "TRUE",
                        asmBooleanType.getDescriptor
                    )
                }
                
                case in.Literal(java.lang.Boolean.FALSE, _) => {
                    mvis.visitFieldInsn(
                        O.GETSTATIC, 
                        asmBooleanType.getInternalName,
                        "FALSE",
                        asmBooleanType.getDescriptor
                    )                    
                }
                
                case in.Literal(obj, _) => {
                    mvis.visitLdcInsn(obj)
                    
                    val objClass = obj.getClass
                    val classType = asm.Type.getType(objClass)
                    val primType = primitives(objClass)
                    
                    mvis.visitMethodInsn(
                        O.INVOKESTATIC,
                        classType.getInternalName,
                        "valueOf",
                        getMethodDescriptor(classType, Array(primType))
                    )
                }
                
                case in.Var(name, sym) => {
                    accessMap.syms(sym).push(mvis)
                }
                
                case in.Field(owner, name, sym, _) => {
                    pushExprValue(owner)
                }
                
                case in.MethodCall(receiver, parts, (msym, msig)) => {
                    def callWithDetails(op: Int, ownerAsmType: asm.Type, desc: String) = {
                        pushExprValue(receiver)
                        msig.parameterPatterns.zip(parts).foreach { case (pattern, part) =>
                            pushRvalues(pattern, part.arg)
                        }
                        mvis.visitMethodInsn(op, ownerAsmType.getInternalName, msym.name.javaName, desc)
                    }
                    
                    def callWithOpcode(op: Int) = {
                        val ownerAsmType = asmType(msig.receiverTy)
                        val desc = methodDesc(msym.msig)
                        callWithDetails(op, ownerAsmType, desc)
                    }
                    
                    msym.kind match {
                        case Symbol.IntrinsicMath(mthdName, leftClass, rightClass, returnClass) => {
                            assert(parts.length == 1)
                            pushExprValue(receiver)
                            pushExprValue(parts.head.arg)
                            mvis.visitMethodInsn(
                                O.INVOKESTATIC, 
                                asm.Type.getType(classOf[IntrinsicMathGen]).getInternalName, 
                                mthdName,
                                getMethodDescriptor(
                                    asm.Type.getType(returnClass),
                                    Array(asm.Type.getType(leftClass), asm.Type.getType(rightClass))
                                )
                            )
                        }
                        
                        case Symbol.IntrinsicControlFlow(mthdName, argumentClasses, resultClass) => {
                            callWithDetails(
                                O.INVOKESTATIC,
                                asm.Type.getType(classOf[IntrinsicControlFlow]),
                                getMethodDescriptor(
                                    asm.Type.getType(resultClass), 
                                    argumentClasses.map(asm.Type.getType))
                            )
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
                
                case in.Null(_) => {
                    mvis.visitInsn(O.ACONST_NULL)                    
                }
            }
        }
        
        /** Executes `stmt` and discards the result.
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

        /** Executes `stmt` and pushes the result 
          * onto the stack. */
        def pushStatement(stmt: in.Stmt) {
            stmt match {
                case expr: in.Expr => {
                    pushExprValue(expr)
                }
                
                case _ => {
                    execStatement(stmt)
                    mvis.visitInsn(O.ACONST_NULL)                                        
                }
            }
        }
        
        def returnResultOfStatements(stmts: List[in.Stmt]) {
            stmts match {
                case List() => 
                    throw new RuntimeException("No empty lists")
                case List(stmt) => {
                    pushStatement(stmt)
                    mvis.visitInsn(O.ARETURN)                
                }
                case hd :: tl => {
                    execStatement(hd)
                    returnResultOfStatements(tl)
                }                
            }
        }
        
        /** Returns an access map for a method-local class with
          * the name `cname`.  Any local variables defined in the
          * current scope but referenced by `cname` will be stored
          * into fields of the new object.  Emits instructions to
          * copy those values, assuming that an instance of
          * `cname` is at the top of the stack.
          *
          * Stack: ..., instance => ..., instance
          */
        def deriveAccessMap(
            cname: Name.Qual,
            cvis: asm.ClassVisitor,
            stmts: List[in.Stmt]
        ) = {
            val derivedAccessMap = new AccessMap(cname)
            val summary = summarizeSymbolsInStmts(stmts)
            val cache = new mutable.HashMap[AccessPath, AccessPath]()
            val thisAccessPath = derivedAccessMap.pathToFreshSlot(asmClassType(cname))
            
            def redirect(optName: Option[Name.Var], accessPath: AccessPath): AccessPath = {
                cache.get(accessPath) match {
                    case Some(redirectedAccessPath) => redirectedAccessPath
                    case None => accessPath match {
                        case AccessVar(index, asmType) => {
                            val fieldName = freshVarName(optName)
                            
                            // Add declaration for field:
                            val fvis = cvis.visitField(
                                O.ACC_PUBLIC, 
                                fieldName.javaName,
                                asmType.getDescriptor,
                                null,
                                null
                            )
                            fvis.visitEnd
                            
                            // Emit instructions to store the result:
                            mvis.visitInsn(O.DUP)
                            accessPath.push(mvis)
                            mvis.visitFieldInsn(
                                O.PUTFIELD,
                                cname.internalName,
                                fieldName.javaName,
                                asmType.getDescriptor
                            )
                            
                            // Final result:
                            val result = AccessField(thisAccessPath, fieldName.javaName, asmType)
                            cache(accessPath) = result
                            result
                        }
                        
                        case AccessIndex(owner, index, asmType) =>
                            AccessIndex(redirect(None, owner), index, asmType)
                            
                        case AccessField(owner, name, asmType) =>
                            AccessField(redirect(None, owner), name, asmType)
                    }
                }
            }
            
            // For every symbol `sym` accessed by `stmts` and in scope from the outside:
            summary.accessSyms.foreach { sym => 
                accessMap.syms.get(sym).foreach { accessPath =>
                    val redirectedAccessPath = redirect(Some(sym.name), accessPath)
                    derivedAccessMap.addSym(sym, redirectedAccessPath)
                }
            }
            
            derivedAccessMap
        }
        
        /** Creates a new class representing the statements
          * in `tmpl` and pushes an instance of that class
          * onto the bytecode stack.  The class will have fields
          * for any captured local variables.  Also emits 
          * instructions to initialize those fields. */
        def pushAnonymousBlock(
            tmpl: in.Block
        ) {
            val name = freshQualName(accessMap.context)
            val tmplwr = new ClassWriter(name, noSuffix)

            tmplwr.cvis.visit(
                O.V1_5,
                O.ACC_PUBLIC,
                name.internalName,
                null, // XXX Signature
                "java/lang/Object",
                Array(tmpl.className.internalName)
            )
            
            // Create the new object and copy over values for
            // any local variables which it references:
            mvis.visitTypeInsn(O.NEW, name.internalName)
            val derivedAccessMap = deriveAccessMap(name, tmplwr.cvis, tmpl.stmts)

            // 
            val tmplmvis = tmplwr.cvis.visitMethod(
                O.ACC_PUBLIC,
                Name.ValueMethod.javaName,
                getMethodDescriptor(asmObjectType, Array(asmObjectType)),
                null, // generic signature
                null  // thrown exceptions
            )
            
            // Add parameters to the access map:
            //    If there are no parameter, there will still be one in the bytecode of type Void,
            //    so just reserve the local variable slot.
            //    XXX -> This won't really work.  Bytecode always has a single param, we have to
            //           explode the tuple etc.  In fact, this is a more general problem that needs
            //           to be addressed.  
            tmpl.param.symbols match {
                case List() => accessMap.pathToFreshSlot(asmVoidType)
                case syms => syms.foreach(derivedAccessMap.addUnboxedSym)
            }
            
            // Add local variables declared within `tmpl.stmts` to the access map:
            addSymbolsDeclaredIn(derivedAccessMap, tmpl.stmts, tmplmvis)
            
            // Visit the statements:
            val stmtVisitor = new StatementVisitor(derivedAccessMap, tmplmvis)
            stmtVisitor.returnResultOfStatements(tmpl.stmts)
            tmplmvis.visitEnd

            tmplwr.end()
        }
        
    }
    
    // ___ Methods __________________________________________________________
    
    def methodParameterTypes(params: List[in.Param]) = 
        params.flatMap(p => in.toPattern(p).varTys)
    
    def writeInterMethodInterface(
        csym: Symbol.ClassFromInterFile, 
        cvis: asm.ClassVisitor, 
        decl: in.MethodDecl
    ) {
        val returnAsmTy = asmType(decl.returnTy)
        val paramAsmTys = methodParameterTypes(decl.params).map(asmType)
        val mvis = cvis.visitMethod(
            O.ACC_PUBLIC,
            decl.name.javaName,
            getMethodDescriptor(returnAsmTy, paramAsmTys.toArray),
            null, // generic signature
            null  // thrown exceptions
        )
        mvis.visitEnd
    }
    
    def writeInterMethodImpl(
        csym: Symbol.ClassFromInterFile, 
        cvis: asm.ClassVisitor, 
        decl: in.MethodDecl
    ) {
        val returnAsmTy = asmType(decl.returnTy)
        val receiverAsmTy = asm.Type.getObjectType(csym.name.internalName)
        val paramAsmTys = methodParameterTypes(decl.params).map(asmType)
        val mvis = cvis.visitMethod(
            O.ACC_PUBLIC,
            decl.name.javaName,
            getMethodDescriptor(returnAsmTy, (receiverAsmTy :: paramAsmTys).toArray),
            null, // generic signature
            null  // thrown exceptions
        )
        decl.optBody.foreach { body =>
            val accessMap = constructAccessMap(
                csym.name.withSuffix("$" + decl.name),
                mvis, 
                decl.receiverSym, 
                decl.params, 
                body.stmts
            )
            val stmtVisitor = new StatementVisitor(accessMap, mvis)
            stmtVisitor.returnResultOfStatements(body.stmts)
        }
        mvis.visitEnd
    }
    
    def constructAccessMap(
        context: Name.Qual,
        mvis: asm.MethodVisitor,
        receiverSymbol: Symbol.Var, 
        parameterPatterns: List[in.Param],
        stmts: List[in.Stmt]
    ) = {
        val accessMap = new AccessMap(context)
        (receiverSymbol :: parameterPatterns.flatMap(_.symbols)).foreach(accessMap.addUnboxedSym)
        addSymbolsDeclaredIn(accessMap, stmts, mvis)
        accessMap
    }
    
    def addSymbolsDeclaredIn(
        accessMap: AccessMap, 
        stmts: List[in.Stmt],
        mvis: asm.MethodVisitor        
    ) {
        val boxedArray = new BoxedArray(accessMap)
        val summary = summarizeSymbolsInStmts(stmts)
        stmts.foreach(_.println(PrettyPrinter.stdout))
        summary.declaredSyms.foreach { sym =>
            if(summary.boxedSyms(sym)) boxedArray.addBoxedSym(sym)
            else accessMap.addUnboxedSym(sym)
        }
        boxedArray.createArrayIfNeeded(mvis)
    }

    // ___ Classes __________________________________________________________
    
    def writeInterClassInterface(csym: Symbol.ClassFromInterFile) {
        val wr = new ClassWriter(csym.name, noSuffix)
        import wr.cvis
        
        val superClassNames = csym.superClassNames(state).toList
        cvis.visit(
            O.V1_5,
            O.ACC_ABSTRACT + O.ACC_INTERFACE + O.ACC_PUBLIC,
            csym.name.internalName,
            null, // XXX Signature
            "java/lang/Object",
            superClassNames.map(_.internalName).toArray
        )
        
        csym.loweredSource.members.foreach {
            case decl: in.MethodDecl => writeInterMethodInterface(csym, cvis, decl)
            case _ =>
        }
        
        wr.end()
    }
    
    def writeInterClassImpl(csym: Symbol.ClassFromInterFile) {
        val wr = new ClassWriter(csym.name, implSuffix)
        import wr.cvis

        cvis.visit(
            O.V1_5,
            O.ACC_PUBLIC,
            csym.name.internalName + implSuffix,
            null, // XXX Signature
            "java/lang/Object",
            Array(csym.name.internalName)
        )
        
        csym.loweredSource.members.foreach {
            case decl: in.MethodDecl => writeInterMethodImpl(csym, cvis, decl)
            case _ =>
        }
        
        wr.end()
    }
    
    def writeClassSymbol(csym: Symbol.ClassFromInterFile) = {
        writeInterClassInterface(csym)
        writeInterClassImpl(csym)
    }
    
}
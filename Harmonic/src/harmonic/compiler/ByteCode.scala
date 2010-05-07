package harmonic.compiler

import org.objectweb.asm
import org.objectweb.asm.Type.getMethodDescriptor
import scala.collection.mutable
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import asm.{Opcodes => O}

import Ast.{Lower => in}
import Util._

object ByteCode {
    val noSuffix = ""
    val implSuffix = "$Impl"
    val staticSuffix = "$Static"
}

/** The final step in compilation: generates appropriate 
  * .class files for each class.
  *
  * Given a class Foo, the following class files are generated:
  * - Foo.class: a Java interface defining all members of Foo
  * - Foo$Impl.class: a Java class that implements Foo, suitable
  *   for instantiation.  (Only if Foo is not abstract)
  * - Foo$Static.class: a Java class defining static members for all 
  *   of the method definitions in Foo.  
  */
case class ByteCode(global: Global) {
    import ByteCode.noSuffix
    import ByteCode.implSuffix
    import ByteCode.staticSuffix
    
    // ___ Generating fresh, unique class names _____________________________
    
    def freshClassName(context: Name.Class) = {
        context.withSuffix("$" + global.freshInteger())
    }
    
    def freshVarName(base: Option[Name.Var]) = {
        Name.LocalVar("$%s$%s".format(base.getOrElse(""), global.freshInteger()))
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
    
    def asmType(ty: Type.Ref): asm.Type = ty match {
        case Type.Class(name, List()) => asm.Type.getObjectType(name.internalName)
        case Type.Tuple(List()) => asmVoidType
        case Type.Tuple(List(ty)) => asmType(ty)
        case Type.Tuple(_) => asmObjectArrayType
        case _ => asmObjectType
    }

    def downcastNeeded(toAsmTy: asm.Type, fromAsmTy: asm.Type): Boolean = {
        toAsmTy != asmObjectType && toAsmTy != fromAsmTy
    }
    
    def downcastNeeded(toTy: Type.Ref, fromTy: Type.Ref): Boolean = {
        (toTy, fromTy) match {
            case (Type.Class(toName, _), Type.Class(fromName, _)) => {
                val toSym = global.csym(toName)
                val fromSym = global.csym(fromName)
                !fromSym.isSubclass(toSym)
            }
            
            case _ => downcastNeeded(asmType(toTy), asmType(fromTy))
        }                    
    }
    
    def asmClassType(name: Name.Class) = asm.Type.getObjectType(name.internalName)
    
    def plainMethodDescFromSig(msig: MethodSignature[Pattern.Anon]): String = {
        getMethodDescriptor(
            asmType(msig.returnTy), 
            msig.parameterPatterns.flatMap(_.varTys).map(asmType).toArray
        )
    }
    
    def mroMethodDescFromSig(msig: MethodSignature[Pattern.Anon]): String = {
        getMethodDescriptor(
            asmType(msig.returnTy), 
            (asm.Type.INT_TYPE :: msig.parameterPatterns.flatMap(_.varTys).map(asmType)).toArray
        )
    }
    
    def staticMethodDescFromSym(msym: MethodSymbol): String = {
        val ret = asmType(msym.msig.returnTy)
        val rcvr = asm.Type.getObjectType(msym.clsName.internalName)
        val params = msym.msig.parameterPatterns.flatMap(_.varTys).map(asmType)
        getMethodDescriptor(ret, (rcvr :: asm.Type.INT_TYPE :: params).toArray)
    }
    
    sealed case class ExtendedMethodVisitor(mvis: asm.MethodVisitor) {
        def complete {
            mvis.visitMaxs(0, 0)
            mvis.visitEnd
        }
        
        def loadVar(index: Int, asmTy: asm.Type) = {
            mvis.visitVarInsn(asmTy.getOpcode(O.ILOAD), index)
        }
        
        def storeVar(index: Int, asmTy: asm.Type) = {
            println("storeVar: %s, %s".format(index, asmTy))
            mvis.visitVarInsn(asmTy.getOpcode(O.ISTORE), index)
        }
        
        def getField(fsym: VarSymbol.Field) = {
            val op = {
                if(fsym.modifiers.isStatic) O.GETSTATIC
                else O.GETFIELD                
            }
            mvis.visitFieldInsn(
                op, 
                fsym.name.className.internalName,
                fsym.name.javaName,
                asmType(fsym.ty).getDescriptor
            )
        }
        
        def pushIntegerConstant(value: Int) = value match {
            case 0 => mvis.visitInsn(O.ICONST_0)
            case 1 => mvis.visitInsn(O.ICONST_1)
            case 2 => mvis.visitInsn(O.ICONST_2)
            case 3 => mvis.visitInsn(O.ICONST_3)
            case _ if value < 128 => mvis.visitIntInsn(O.BIPUSH, value)
            case _ => mvis.visitLdcInsn(value)
        }
        
        def downcast(toAsmTy: asm.Type) {
            toAsmTy.getSort match {
                case asm.Type.ARRAY =>
                    mvis.visitTypeInsn(O.CHECKCAST, toAsmTy.getInternalName)
                    
                case asm.Type.OBJECT =>
                    mvis.visitTypeInsn(O.CHECKCAST, toAsmTy.getInternalName)                        
            }                
        }
        
        def downcast(toTy: Type.Ref) {
            downcast(asmType(toTy))
        }
        
        def downcastIfNeeded(toTy: Type.Ref, fromTy: Type.Ref) {
            if(downcastNeeded(toTy, fromTy)) downcast(toTy)
        }
    }
    implicit def extendedMethodVisitor(mvis: asm.MethodVisitor) = ExtendedMethodVisitor(mvis)
    
    // ___ Writing .class, .s files _________________________________________
    
    class ClassWriter(className: Name.Class, suffix: String) 
    {
        private[this] def fileWithExtension(ext: String) = {
            val relPath = className.relPath + suffix + ext
            new java.io.File(global.config.outputDir, relPath)
        }

        private[this] def trace(cvis: asm.ClassVisitor) = {
            if(!global.config.dumpBytecode) {
                (cvis, None)
            } else {
                val sFile = fileWithExtension(".s")
                try {
                    sFile.getParentFile.mkdirs()
                    val writer = new java.io.FileWriter(sFile)
                    (new asm.util.TraceClassVisitor(cvis, new java.io.PrintWriter(writer)), Some(writer))
                } catch {
                    case err: java.io.IOException => {
                        System.err.printf("Error writing to %s: %s\n", sFile, err)
                        (cvis, None)
                    }
                }

            }
        }
        
        private[this] def check(cvis: asm.ClassVisitor) = {
            if(global.config.checkBytecode) new asm.util.CheckClassAdapter(cvis, true)
            else cvis            
        }

        val writer = new asm.ClassWriter(asm.ClassWriter.COMPUTE_MAXS | asm.ClassWriter.COMPUTE_FRAMES)
        val checker = check(writer)
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
                    Error.IOError(err).report(global, InterPosition.forFile(clsFile))
                }
            }
        }
    }
    
    // ___ Access Paths and Maps ____________________________________________
    //
    // An AccessPath encodes the route to obtain the value for some local
    // variable.  This value may be present on the stack or it may require
    // dereferences through the fields of other objects to be loaded.
    
    sealed abstract class ValuePath {
        /** Type of the value at the other end of this path. */
        def asmType: asm.Type
        
        // Stack: ... => ..., value
        def push(mvis: asm.MethodVisitor): Unit
    }
    
    sealed case class IntConstant(
        value: Int
    ) extends ValuePath {
        def asmType = asm.Type.INT_TYPE
        
        def push(mvis: asm.MethodVisitor) {
            mvis.pushIntegerConstant(value)
        }
    }
    
    sealed abstract class AccessPath extends ValuePath {
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
            mvis.loadVar(index, asmType)
        }
        
        def pushLvalue(mvis: asm.MethodVisitor) {
        }
        
        def storeLvalue(mvis: asm.MethodVisitor) {
            storeLvalueWithoutPush(mvis)
        }
        
        def storeLvalueWithoutPush(mvis: asm.MethodVisitor) {
            mvis.storeVar(index, asmType)
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
    
    class AccessMap(val context: Name.Class)
    {
        val syms = new mutable.HashMap[VarSymbol.Any, AccessPath]()
        private[this] var maxSlot = 0
        
        def pathToFreshSlot(asmTy: asm.Type) = {
            val slot = maxSlot
            maxSlot += 1
            AccessVar(slot, asmTy)
        }
        
        def addSym(sym: VarSymbol.Any, accessPath: AccessPath) {
            syms(sym) = accessPath
        }

        def addUnboxedSym(sym: VarSymbol.Any) = {
            println("sym %s assigned to slot %d".format(sym, maxSlot))
            syms(sym) = pathToFreshSlot(asmType(sym.ty))
        }
        
        def withStashSlot(func: (Int => Unit)) {
            val stashSlot = maxSlot
            println("> Stash Slot %d".format(maxSlot))
            maxSlot += 1
            func(stashSlot)
            maxSlot -= 1
            println("< Stash Slot %d".format(maxSlot))
        }
        
        def pushSym(sym: VarSymbol.Any, mvis: asm.MethodVisitor) = syms(sym).push(mvis)
    }
    
    class BoxedArray(accessMap: AccessMap)
    {
        private[this] val boxedArrayPath = accessMap.pathToFreshSlot(asmObjectArrayType)
        private[this] var maxIndex = 0
        
        def addBoxedSym(sym: VarSymbol.Any) = {
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
        declaredSyms: Set[VarSymbol.Any],
        
        // Symbols that are read:
        //    (Not necessarily a subset of declaredSyms.)
        readSyms: Set[VarSymbol.Any],
        
        // Symbols that are re-assigned:
        //    (Not necessarily a subset of declaredSyms.)
        writeSyms: Set[VarSymbol.Any],
        
        // Symbols which are accessed from an inner class of some kind: 
        //    (A subset of readSyms and writeSyms)
        sharedSyms: Set[VarSymbol.Any]
    ) {
        def accessSyms = readSyms ++ writeSyms
        def boxedSyms(sym: VarSymbol.Any) = writeSyms(sym) && sharedSyms(sym)
    }
    
    object SymbolSummary {
        val empty = SymbolSummary(Set(), Set(), Set(), Set())
    }

    def symbolsReassignedInLvalue(local: in.Lvalue): List[VarSymbol.Any] = local match {
        case in.TupleLvalue(locals) => locals.flatMap(symbolsReassignedInLvalue)
        case in.ReassignVarLvalue(_, sym) => List(sym)
        case _ => Nil
    }
    
    def symbolsDeclaredInLvalue(local: in.Lvalue): List[VarSymbol.Any] = local match {
        case in.TupleLvalue(locals) => locals.flatMap(symbolsDeclaredInLvalue)
        case in.DeclareVarLvalue(_, _, _, sym) => List(sym)
        case _ => Nil
    }

    def summarizeSymbolsInRcvr(summary: SymbolSummary, rcvr: in.Rcvr): SymbolSummary = {
        rcvr match {
            case in.Static(_) => summary
            case in.Super(_) => summary
            case expr: in.Var => summarizeSymbolsInExpr(summary, expr)
        }
    }
    
    def summarizeSymbolsInOwner(summary: SymbolSummary, owner: in.Owner): SymbolSummary = {
        owner match {
            case in.Static(_) => summary
            case expr: in.Var => summarizeSymbolsInExpr(summary, expr)
        }
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
            case in.Cast(subexpr, _) => summarizeSymbolsInExpr(summary, subexpr)
            case in.Literal(_, _) => summary
            case in.Var(_, sym) => summary.copy(readSyms = summary.readSyms + sym)
            case in.Field(owner, _, _, _) => summarizeSymbolsInOwner(summary, owner)
            case in.MethodCall(receiver, _, args, _) => {
                val receiverSummary = summarizeSymbolsInRcvr(summary, receiver)
                args.foldLeft(receiverSummary)(summarizeSymbolsInExpr)
            }
            case in.NewCtor(_, arg, _, _) => summarizeSymbolsInExpr(summary, arg)
            case in.NewAnon(_, arg, mems, _, _, _) => summarizeSymbolsInExpr(summary, arg) // FIXME mems
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
                    declaredSyms = summaryExpr.declaredSyms ++ symbolsDeclaredInLvalue(local),
                    writeSyms = summaryExpr.writeSyms ++ symbolsReassignedInLvalue(local)
                )
            }
        }
    }
    
    def summarizeSymbolsInStmts(stmts: List[in.Stmt]) = {
        stmts.foldLeft(SymbolSummary.empty)(summarizeSymbolsInStmt)
    }
    
    // ___ Statements _______________________________________________________
    
    class StatementVisitor(accessMap: AccessMap, nextMro: ValuePath, mvis: asm.MethodVisitor) {
        
        /** Generates the instructions to store to an lvalue, unpacking
          * tuple values as needed.  
          *
          * Stack: ... => ...
          */
        def store(lvalue: in.Lvalue, rvalue: in.Expr) {
            lvalue match {
                case in.TupleLvalue(List(sublvalue)) => {
                    store(sublvalue, rvalue)
                }
                
                case varPat: in.VarAstPattern[VarSymbol.Any] => {
                    // Micro-optimize generated code to avoid using stashSlot:
                    val sym = varPat.sym
                    val accessPath = accessMap.syms(sym)
                    accessPath.pushLvalue(mvis)
                    pushExprValueDowncastingTo(lvalue.ty, rvalue)
                    accessPath.storeLvalue(mvis)
                }
                
                case _ => {
                    pushRvalues(in.toPatternAnon(lvalue), rvalue)
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
                case (Pattern.AnonTuple(List(l)), _) =>
                    pushRvalues(l, rvalue)
                    
                case (_, in.Tuple(List(r))) =>
                    pushRvalues(lvalue, r)
                    
                case (Pattern.AnonTuple(ls), in.Tuple(rs)) if sameLength(ls, rs) =>
                    ls.zip(rs).foreach { case (l, r) => pushRvalues(l, r) }
                    
                case _ => {
                    pushExprValueDowncastingTo(lvalue.ty, rvalue)
                    expand(lvalue)
                }
            }
        }
        
        /** Pops the rvalues which were pushed by `pushRvalues(lvalue)(rvalue)`,
          * storing them into `lvalue`. */
        def popRvalues(lvalue: in.Lvalue, rvalue: in.Expr) {
            (lvalue, rvalue) match {
                case (in.TupleLvalue(List(l)), _) =>
                    popRvalues(l, rvalue)
                    
                case (_, in.Tuple(List(r))) =>
                    popRvalues(lvalue, r)
                    
                case (in.TupleLvalue(ls), in.Tuple(rs)) if sameLength(ls, rs) =>
                    ls.zip(rs).reverse.foreach { case (l, r) => popRvalues(l, r) }
                    
                case _ => {
                    contract(lvalue)                    
                }
            }
        }
        
        def expand(lvalue: Pattern.Anon) {
            lvalue match {
                case Pattern.AnonTuple(sublvalues) => {
                    accessMap.withStashSlot { stashSlot =>
                        mvis.visitVarInsn(O.ASTORE, stashSlot) // Stack: ...
                        sublvalues.zipWithIndex.foreach { case (sublvalue, idx) =>
                            mvis.visitVarInsn(O.ALOAD, stashSlot) // Stack: ..., array
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
            def storeSym(sym: VarSymbol.Any) =
                accessMap.syms(sym).storeLvalueWithoutPush(mvis)
            lvalue match {
                case in.TupleLvalue(sublvalues) => sublvalues.reverse.foreach(contract)
                case in.DeclareVarLvalue(_, _, _, sym) => storeSym(sym)
                case in.ReassignVarLvalue(_, sym) => storeSym(sym)
                case in.FieldLvalue(_, sym) => storeSym(sym)
            }
        }
        
        def pushExprValueDowncastingTo(toAsmTy: asm.Type, expr: in.Expr) {
            pushExprValue(expr)
            if(downcastNeeded(toAsmTy, asmType(expr.ty)))
                mvis.downcast(toAsmTy)
        }
        
        def pushExprValueDowncastingTo(toTy: Type.Ref, expr: in.Expr) {
            pushExprValue(expr)
            if(downcastNeeded(toTy, expr.ty))
                mvis.downcast(toTy)
        }
        
        def pushMethodArgs(
            msig: MethodSignature[Pattern.Anon],
            args: List[in.Expr]
        ) {
            msig.parameterPatterns.zip(args).foreach { case (pattern, arg) =>
                pushRvalues(pattern, arg)
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
                
                case in.Cast(subexpr, tref) => {
                    pushExprValue(subexpr)
                    mvis.downcastIfNeeded(tref.ty, subexpr.ty)
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
                
                case in.Var(_, sym) => {
                    accessMap.syms(sym).push(mvis)
                }
                
                case in.Field(in.Static(_), _, fsym, _) => {
                    mvis.getField(fsym)
                }
                
                case in.Field(owner: in.Var, _, fsym, _) => {
                    pushExprValue(owner)
                    mvis.getField(fsym)
                }

                case in.MethodCall(in.Static(_), name, args, (msym, msig)) => {
                    pushMethodArgs(msig, args)
                    mvis.visitMethodInsn(
                        O.INVOKESTATIC,
                        msym.clsName.internalName,
                        msym.name.javaName,
                        mroMethodDescFromSig(msig)
                    )
                }
                
                case in.MethodCall(in.Super(_), name, args, (msym, msig)) => {
                    mvis.visitVarInsn(O.ALOAD, 0)   // load this ptr
                    nextMro.push(mvis)              // load next index in MRO
                    pushMethodArgs(msig, args)
                    mvis.visitMethodInsn(
                        O.INVOKEINTERFACE,
                        msym.clsName.internalName,
                        msym.name.javaName,
                        mroMethodDescFromSig(msig)
                    )
                }
                
                case in.MethodCall(receiver: in.Var, name, args, (msym, msig)) => {
                    def callWithDetails(op: Int, ownerAsmType: asm.Type, desc: String) = {
                        pushExprValue(receiver)
                        pushMethodArgs(msig, args)
                        mvis.visitMethodInsn(op, ownerAsmType.getInternalName, msym.name.javaName, desc)
                    }
                    
                    def callWithOpcode(op: Int) = {
                        val ownerAsmType = asmType(msig.receiverTy)
                        val desc = plainMethodDescFromSig(msym.msig)
                        callWithDetails(op, ownerAsmType, desc)
                    }
                    
                    msym.kind match {
                        case MethodKind.IntrinsicMath(mthdName, leftClass, rightClass, returnClass) => {
                            assert(args.length == 1)
                            val leftAsmTy = asm.Type.getType(leftClass)
                            val rightAsmTy = asm.Type.getType(rightClass)
                            pushExprValueDowncastingTo(leftAsmTy, receiver)
                            pushExprValueDowncastingTo(rightAsmTy, args.head)
                            mvis.visitMethodInsn(
                                O.INVOKESTATIC, 
                                asm.Type.getType(classOf[IntrinsicMathGen]).getInternalName, 
                                mthdName,
                                getMethodDescriptor(asm.Type.getType(returnClass), Array(leftAsmTy, rightAsmTy))
                            )
                        }
                        
                        case MethodKind.IntrinsicStatic(ownerClass, mthdName, argumentClasses, resultClass) => {
                            val resultAsmTy = asm.Type.getType(resultClass)
                            val argAsmTys = argumentClasses.map(asm.Type.getType)
                            pushExprValueDowncastingTo(argAsmTys(0), receiver)
                            pushMethodArgs(msig, args)
                            mvis.visitMethodInsn(
                                O.INVOKESTATIC,
                                asm.Type.getType(ownerClass).getInternalName,
                                mthdName,
                                getMethodDescriptor(resultAsmTy, argAsmTys)
                            )
                        }
                        
                        case MethodKind.Inter => callWithOpcode(O.INVOKEINTERFACE)
                        case MethodKind.InterCtor => callWithOpcode(O.INVOKESPECIAL)
                        case MethodKind.JavaVirtual => callWithOpcode(O.INVOKEVIRTUAL)
                        case MethodKind.JavaInterface => callWithOpcode(O.INVOKEINTERFACE)
                        case MethodKind.JavaStatic => callWithOpcode(O.INVOKESTATIC)
                        
                        case MethodKind.ErrorMethod => {
                            throw new RuntimeException("TODO")
                        }
                    }
                }
                
                case in.NewCtor(tref, arg, msym, Type.Class(name, _)) => {
                    val internalImplName = global.csym(name).internalImplName
                    mvis.visitTypeInsn(O.NEW, internalImplName)
                    mvis.visitInsn(O.DUP)
                    mvis.visitMethodInsn(
                        O.INVOKESPECIAL,
                        internalImplName,
                        Name.InitMethod.javaName,
                        getMethodDescriptor(
                            asm.Type.VOID_TYPE,
                            msym.msig.parameterPatterns.flatMap(_.varTys).map(asmType).toArray
                        )
                    )
                }
                
                case in.NewAnon(tref, arg, mems, csym, msym, ty) => {
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
                    stmts.foreach(execStatement) // FIXME Not really right.
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
            cname: Name.Class,
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
            val name = freshClassName(accessMap.context)
            val blockTy = Type.Class(name, List())
            val tmplwr = new ClassWriter(name, noSuffix)

            tmplwr.cvis.visit(
                O.V1_5,
                O.ACC_PUBLIC,
                name.internalName,
                null, // FIXME Signature
                "java/lang/Object",
                Array(tmpl.className.internalName)
            )
            
            writeEmptyCtor(name, Name.ObjectClass, tmplwr.cvis)

            // Create the new object and copy over values for
            // any local variables which it references:
            mvis.visitTypeInsn(O.NEW, name.internalName)
            mvis.visitInsn(O.DUP)
            mvis.visitMethodInsn(
                O.INVOKESPECIAL,
                name.internalName,
                Name.InitMethod.javaName,
                "()V"
            )
            val derivedAccessMap = deriveAccessMap(name, tmplwr.cvis, tmpl.stmts)
            
            val methodSig = MethodSignature(
                tmpl.returnTref.ty,
                blockTy,
                List(in.toPatternRef(tmpl.param))
            )

            // 
            val tmplmvis = tmplwr.cvis.visitMethod(
                O.ACC_PUBLIC,
                Name.ValueMethod.javaName,
                plainMethodDescFromSig(methodSig),
                null, // generic signature
                null  // thrown exceptions
            )
            tmplmvis.visitCode
            
            // Add parameters to the access map:
            //    If there are no parameter, there will still be one in the bytecode of type Void,
            //    so just reserve the local variable slot.
            tmpl.param.symbols match {
                case List() => accessMap.pathToFreshSlot(asmVoidType)
                case syms => syms.foreach(derivedAccessMap.addUnboxedSym)
            }
            
            // Add local variables declared within `tmpl.stmts` to the access map:
            addSymbolsDeclaredIn(derivedAccessMap, tmpl.stmts, tmplmvis)
            
            // Visit the statements:
            val stmtVisitor = new StatementVisitor(derivedAccessMap, IntConstant(1), tmplmvis)
            stmtVisitor.returnResultOfStatements(tmpl.stmts)
            tmplmvis.complete
            
            // Emit a forwarding method from the interface version:
            val interfaceMethodSig = MethodSignature(
                Type.Object,
                blockTy,
                List(Pattern.Var(Name.LocalVar("arg"), Type.Object))
            )
            writeForwardingMethodIfNeeded(
                className     = name, 
                cvis          = tmplwr.cvis,
                methodName    = Name.ValueMethod,
                withMroIndex  = false,
                masterSig     = methodSig,
                overriddenSig = interfaceMethodSig,
                invokeOp      = O.INVOKEVIRTUAL
            )

            tmplwr.end()
        }
        
    }
    
    // ___ Adapting Parameters ______________________________________________
    //
    // Due to erasure and the fact that the JVM type system does not match 
    // ours 100%, we often have to create multiple versions of the same
    // method.  These different versions all expect to be invoked with the
    // same parameters, but they may be "packaged" differently.  
    //
    // Consider a block like `{ (x: Integer, y: Integer): Integer -> x + y }`.
    // This block will define a method like:
    //      Integer value(Integer x, Integer y) { return x + y; }
    //
    // However, to fulfill the `Block` interface, the class must also 
    // define a method like:
    //      Object value(Object arg) {
    //          Object[] array = (Object[]) arg;
    //          return value((Integer)array[0], (Integer)array[1]);
    //      }
    // This method is called a "forwarding" method.  It's only purpose is
    // to downcast and pack/unpack its arguments.   Our type check should
    // ensure that these downcasts and array dereferences succeed.
    //
    // To do this adaptation, we make use of the `pushRvalues()` method 
    // defined above, which is the same method that pushes arguments to
    // normal method calls.  To use the function we create an expression
    // representing the "source" arguments (in the example above this would
    // just be `arg`, but if the source has tuples it can be slightly
    // more involved).  We then insert a top-level downcast, so the final
    // expression passed to `pushRvalues()` would be `((Integer, Integer)) arg`
    // (mixing Java cast syntax with Harmonic types).
    
    class ParameterAdapter(
        accessMap: AccessMap,
        srcPatterns: List[Pattern.Ref]
    ) {
        // Construct expressions mirroring the patterns of the source method.
        // Also create symbols for each of the src params and add them to `accessMap`.
        //
        // So if the source patterns were `(a: A, b: B)` and `(c: C)`, we would:
        // (a) Create three symbols `a`, `b`, and `c`
        // (b) Return the expressions `(a, b)` and `c`.
        def constructExprFromPattern(pattern: Pattern.Ref): in.AtomicExpr = pattern match {
            case Pattern.Tuple(subpatterns) => 
                in.Tuple(subpatterns.map(constructExprFromPattern))
                
            case Pattern.Var(name, ty) => {
                val sym = new VarSymbol.Local(Modifier.Set.empty, name, ty)
                accessMap.addUnboxedSym(sym)
                in.Var(Ast.LocalName(name), sym)
            }
        }
        val rvalues = srcPatterns.map(constructExprFromPattern)
        
        def adaptTo(tarPatterns: List[Pattern.Ref], stmtVisitor: StatementVisitor) {
            // Push the values from each of the `rvalues` expressions onto the
            // stack.  We may have to cast the rvalue to the target type, because
            // the types of the source parameters may be supertypes of the target types.
            tarPatterns.zip(rvalues).foreach { case (tarPattern, rvalue) =>
                val casted = 
                    if(downcastNeeded(tarPattern.ty, rvalue.ty))
                        in.Cast(rvalue, in.TypeRef(tarPattern.ty))
                    else 
                        rvalue
                stmtVisitor.pushRvalues(tarPattern, casted)
            }
        }
    }
    
    /** Due to generic types and erasure, we often end up with methods whose 
      * signature is more specialized in the subtype than in the supertype.
      * The interface Block, for example, defines a method Object value(Object arg).
      * Most blocks however will be specialized for a particular type.  Therefore
      * we emit a forwarding method that simply downcasts (or tuple-expands) arg as
      * needed to invoke the more specific version. */
    def writeForwardingMethodIfNeeded(
        className: Name.Class,
        cvis: asm.ClassVisitor, 
        methodName: Name.Method,
        withMroIndex: Boolean,
        masterSig: MethodSignature[Pattern.Ref],
        overriddenSig: MethodSignature[Pattern.Ref],
        invokeOp: Int
    ) {
        val descFunc = if(withMroIndex) mroMethodDescFromSig _ else plainMethodDescFromSig _
        val masterDesc = descFunc(masterSig)
        val overriddenDesc = descFunc(overriddenSig)
        if(masterDesc == overriddenDesc)
            return; // No need for a forwarding method.
        
        // Begin visiting method:
        val mvis = cvis.visitMethod(
            O.ACC_PUBLIC,
            methodName.javaName,
            overriddenDesc,
            null, // generic signature
            null  // thrown exceptions
        )
        mvis.visitCode
        
        // Push this pointer and adapted forms of the parameters:
        val accessMap = new AccessMap(className)
        val thisPtr = accessMap.pathToFreshSlot(asmType(Type.Class(className, List()))) // reserve this ptr
        thisPtr.push(mvis)
        
        if(withMroIndex) {
            val mroIndex = accessMap.pathToFreshSlot(asm.Type.INT_TYPE)
            mroIndex.push(mvis)
        }
            
        val adapter = new ParameterAdapter(accessMap, overriddenSig.parameterPatterns)
        val stmtVisitor = new StatementVisitor(accessMap, IntConstant(0), mvis)
        adapter.adaptTo(masterSig.parameterPatterns, stmtVisitor)
        
        // Invoke master version of the method and return its result:
        mvis.visitMethodInsn(
            invokeOp,
            className.internalName,
            methodName.javaName,
            masterDesc
        )
        mvis.visitInsn(O.ARETURN)
        mvis.complete
    }        
    
    /** Emits forwarding methods for each method overridden by `msym` 
      * whose signature has a different method descriptor.  This includes
      * both a default and a MRO version. */
    def writeForwardingMethods(
        csym: ClassFromSource, 
        cvis: asm.ClassVisitor,
        group: MethodGroup
    ) {
        group.msyms.foreach { msym =>
            List(true, false).foreach { withMroIndex =>
                writeForwardingMethodIfNeeded(
                    className     = csym.name,
                    cvis          = cvis,
                    methodName    = msym.name,
                    withMroIndex  = withMroIndex,
                    masterSig     = group.msig,
                    overriddenSig = msym.msig,
                    invokeOp      = O.INVOKEINTERFACE
                )                
            }
        }
    }
    
    // ___ Methods __________________________________________________________
    
    def addSymbolsDeclaredIn(
        accessMap: AccessMap, 
        stmts: List[in.Stmt],
        mvis: asm.MethodVisitor        
    ) {
        val boxedArray = new BoxedArray(accessMap)
        val summary = summarizeSymbolsInStmts(stmts)
        summary.declaredSyms.foreach { sym =>
            if(summary.boxedSyms(sym)) boxedArray.addBoxedSym(sym)
            else accessMap.addUnboxedSym(sym)
        }
        boxedArray.createArrayIfNeeded(mvis)
    }

    /** Generates a static method which contains the actual implementation. */
    def writeStaticMethodImpl(
        csym: ClassFromSource, 
        cvis: asm.ClassVisitor, 
        msym: MethodSymbol,
        decl: in.MethodDecl
    ) {
        decl.optBody.foreach { body => // Only for non-abstract methods:
            val mvis = cvis.visitMethod(
                O.ACC_PUBLIC + O.ACC_STATIC,
                decl.name.javaName,
                staticMethodDescFromSym(msym),
                null, // generic signature
                null  // thrown exceptions
            )
            mvis.visitCode

            val accessMap = new AccessMap(csym.name)
            accessMap.addUnboxedSym(decl.receiverSym)
            val nextMro = accessMap.pathToFreshSlot(asm.Type.INT_TYPE)
            decl.params.flatMap(_.symbols).foreach(accessMap.addUnboxedSym)

            // Construct access map:
            addSymbolsDeclaredIn(accessMap, body.stmts, mvis)

            // Emit statements:
            val stmtVisitor = new StatementVisitor(accessMap, nextMro, mvis)
            stmtVisitor.returnResultOfStatements(body.stmts)                
            mvis.complete
        }
    }
    
    /** The default version of a method simply takes all the user-
      * specified arguments. */
    def visitPlainMethod(
        csym: ClassFromSource, 
        cvis: asm.ClassVisitor, 
        methodName: Name.Method,
        msig: MethodSignature[Pattern.Ref],
        opts: Int
    ) = {
        val mvis = cvis.visitMethod(
            opts + O.ACC_PUBLIC,
            methodName.javaName,
            plainMethodDescFromSig(msig),
            null, // generic signature
            null  // thrown exceptions
        )
        mvis.visitCode
        mvis
    }
    
    /** We generate an additional version of every method
      * which has one preliminary argument, `int mro`.  
      * `mro` is used for super calls: it indicates the
      * index of the next item in the Method Resolution Order (MRO). */
    def visitMethodWithMro(
        csym: ClassFromSource, 
        cvis: asm.ClassVisitor, 
        methodName: Name.Method,
        msig: MethodSignature[Pattern.Ref],
        opts: Int
    ) = {
        val mvis = cvis.visitMethod(
            opts + O.ACC_PUBLIC,
            methodName.javaName,
            mroMethodDescFromSig(msig),
            null, // generic signature
            null  // thrown exceptions
        )
        mvis.visitCode
        mvis
    }
    
    /** Generates corresponding entries in the interface for a
      * given Harmonic class.  For each user-defined method there
      * are several generated methods.  This includes the "plain"
      * and "nextMro" methods described above but also various 
      * forwarding methods that come about through erasure. */
    def writeMethodInterface(
        csym: ClassFromSource, 
        cvis: asm.ClassVisitor, 
        msym: MethodSymbol
    ) {
        val mvisPlain = visitPlainMethod(csym, cvis, msym.name, msym.msig, O.ACC_ABSTRACT)
        mvisPlain.complete
        val mvisMro = visitMethodWithMro(csym, cvis, msym.name, msym.msig, O.ACC_ABSTRACT)
        mvisMro.complete
    }
    
    /** Generates the actual implementation methods on a class.
      * These methods switch using the "mro" parameter and 
      * invoke the appropriate static method. 
      *
      * Given a method String add(Integer x, Integer y), this would generate: 
      *   String add(Integer x, Integer y) { return add(0, x, y); }
      */
    def writePlainToMroDispatch(
        csym: ClassFromSource, 
        cvis: asm.ClassVisitor,
        group: MethodGroup
    ) {
        val mvis = visitPlainMethod(csym, cvis, group.methodName, group.msig, 0)
        val accessMap = new AccessMap(csym.name)
        val thisPtr = accessMap.pathToFreshSlot(asmType(Type.Class(csym.name, List()))) // reserve this ptr
        val adapter = new ParameterAdapter(accessMap, group.msig.parameterPatterns)
        thisPtr.push(mvis) // push this ptr
        mvis.pushIntegerConstant(0) // push default index for 'mro'
        val stmtVisitor = new StatementVisitor(accessMap, IntConstant(0), mvis)
        adapter.adaptTo(group.msig.parameterPatterns, stmtVisitor) // push parameters
        mvis.visitMethodInsn(
            O.INVOKEINTERFACE,
            csym.name.internalName,
            group.methodName.javaName,
            mroMethodDescFromSig(group.msig)
        )
        mvis.visitInsn(O.ARETURN)
        mvis.complete
    }
    
    /** Computes the method symbol, if any, to be invoked from each MRO index. */
    def computeVersions(
        /** The method whose super-versions we are computing. */
        group: MethodGroup,
        
        /** List of superclasses, in order. */
        mro: List[ClassSymbol], 
        
        /** Remaining methods from group.  */
        msyms: List[MethodSymbol]
    ): List[Option[MethodSymbol]] = {
        mro match {
            case List() => List()
            case csym :: tl => {
                val matching = msyms.takeWhile(_.isFromClassNamed(csym.name))
                val remaining = msyms.drop(matching.length)
                val notAbstract = matching.filter(_.modifiers.isNotAbstract)
                notAbstract match {
                    case List() => None :: computeVersions(group, tl, remaining)
                    case msyms => {
                        // FIXME Should pick the symbol whose signature best matches group.msig
                        Some(msyms.head) :: computeVersions(group, tl, remaining)
                    }
                }
            }
        }
    }
    
    /** Writes the "master method" for `msym`, which simply takes the 
      * current index into the MRO and dispatches to the appropriate
      * static function.  The static functions are defined in 
      * `writeStaticMethodImpl`. */
    def writeMroMethodImpl(
        csym: ClassFromSource, 
        cvis: asm.ClassVisitor,
        group: MethodGroup
    ) {
        val mvis = visitMethodWithMro(csym, cvis, group.methodName, group.msig, 0)
        val accessMap = new AccessMap(csym.name)
        val thisPtr = accessMap.pathToFreshSlot(asmType(Type.Class(csym.name, List())))
        val mroInt = accessMap.pathToFreshSlot(asm.Type.INT_TYPE) 
        val adapter = new ParameterAdapter(accessMap, group.msig.parameterPatterns)
        
        // compute all versions, indexed by mro.
        val msyms = computeVersions(group, MethodResolutionOrder(global).forSym(csym), group.msyms)
        
        // emit table switch
        val dfltLabel = new asm.Label()
        val labels = msyms.map(_ => new asm.Label()).toArray
        mroInt.push(mvis)
        mvis.visitTableSwitchInsn(0, msyms.length - 1, dfltLabel, labels)
        
        // every other version: forward as appropriate
        msyms.zip(labels).zipWithIndex.foreach { 
            case ((Some(verMsym), verLabel), verInt) => {
                // Method defined for this version: 
                //    invoke static impl
                mvis.visitLabel(verLabel)
                thisPtr.push(mvis)                      // push this ptr
                mvis.pushIntegerConstant(verInt + 1)    // push next MRO
                val stmtVisitor = new StatementVisitor(accessMap, IntConstant(verInt + 1), mvis)
                adapter.adaptTo(verMsym.msig.parameterPatterns, stmtVisitor) // push args
                mvis.visitMethodInsn(
                    O.INVOKESTATIC,
                    verMsym.clsName.internalName + staticSuffix,
                    verMsym.name.javaName,
                    staticMethodDescFromSym(verMsym)
                ) // invoke appropriate static implementation
                mvis.visitInsn(O.ARETURN)                
            }
            
            case ((None, verLabel), verInt) => {
                // No method defined for this version: 
                //    fall through to the next one
                mvis.visitLabel(verLabel) 
            }
        }
        
        // default: just return NULL
        mvis.visitLabel(dfltLabel)
        mvis.visitInsn(O.ACONST_NULL)
        mvis.visitInsn(O.ARETURN)
        
        mvis.complete
    }
    
    def writeEmptyCtor(
        name: Name.Class,
        superName: Name.Class,
        cvis: asm.ClassVisitor
    ) {
        // FIXME We have to figure out our ctor model.
        val mvis = cvis.visitMethod(
            O.ACC_PUBLIC,
            Name.InitMethod.javaName,
            "()V",
            null, // generic signature
            null  // thrown exceptions
        )
        mvis.visitCode
        mvis.visitVarInsn(O.ALOAD, 0)
        mvis.visitMethodInsn(
            O.INVOKESPECIAL,
            superName.internalName,
            Name.InitMethod.javaName,
            "()V"
        )
        mvis.visitInsn(O.RETURN)
        mvis.complete
    }
    
    // ___ Classes __________________________________________________________
    
    def writeInterClassInterface(csym: ClassFromSource) {
        val wr = new ClassWriter(csym.name, noSuffix)
        import wr.cvis
        
        val superClassNames = csym.superClassNames
        cvis.visit(
            O.V1_5,
            O.ACC_ABSTRACT + O.ACC_INTERFACE + O.ACC_PUBLIC,
            csym.name.internalName,
            null, // FIXME Signature
            "java/lang/Object",
            superClassNames.map(_.internalName).toArray
        )
        
        csym.allMethodSymbols.foreach { msym =>
            writeMethodInterface(csym, cvis, msym)
        }
        
        wr.end()
    }
    
    def writeImplClass(csym: ClassFromSource) {
        val wr = new ClassWriter(csym.name, implSuffix)
        import wr.cvis

        cvis.visit(
            O.V1_5,
            O.ACC_PUBLIC,
            csym.name.internalName + implSuffix,
            null, // FIXME Signature
            Name.ObjectClass.internalName,
            Array(csym.name.internalName)
        )
        
        writeEmptyCtor(csym.name, Name.ObjectClass, cvis)

        csym.methodGroups.foreach { group =>
            writePlainToMroDispatch(csym, cvis, group)
            writeMroMethodImpl(csym, cvis, group)
            writeForwardingMethods(csym, cvis, group)
        }
        
        wr.end()
    }
    
    def writeStaticClass(csym: ClassFromSource) {
        val wr = new ClassWriter(csym.name, staticSuffix)
        import wr.cvis

        cvis.visit(
            O.V1_5,
            O.ACC_PUBLIC,
            csym.name.internalName + staticSuffix,
            null, // FIXME Signature
            "java/lang/Object",
            Array(csym.name.internalName)
        )
        
        writeEmptyCtor(csym.name, Name.ObjectClass, cvis)
        
        csym.loweredMethods.foreach { case (msym, mdecl) =>
            writeStaticMethodImpl(csym, cvis, msym, mdecl)
        }
        
        wr.end()
    }
    
    def writeClassSymbol(csym: ClassFromSource) = {
        writeInterClassInterface(csym)
        if(!csym.modifiers.isAbstract)
            writeImplClass(csym)
        writeStaticClass(csym)
    }
    
}
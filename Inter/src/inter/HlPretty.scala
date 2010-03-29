package inter

abstract class HlPretty {
    protected[this] def indent(): Unit
    protected[this] def undent(): Unit
    protected[this] def write(fmt: String, args: Any*): Unit
    protected[this] def writeln(fmt: String, args: Any*): Unit
    
    protected[this] def indented(start: String, end: String)(func: => Unit) {
        write(start)
        indent()
        writeln("")
        func
        undent()
        write(end)
    }
    
    def println(compUnit: hl.CompUnit) {
        writeln("package %s;", compUnit.pkg)
        compUnit.imports.foreach(println)
        compUnit.classes.foreach(println)
    }
    
    def println(importDecl: hl.ImportDecl) {
        writeln("%s;", importDecl)
    }
    
    def println(cdecl: hl.ClassDecl) {
        cdecl.annotations.foreach(println)
        writeln("class %s%s", cdecl.name, cdecl.pattern)
        cdecl.superClasses.foreach(c => writeln("extends %s", c))
        indented("{", "}") {
            cdecl.members.dropRight(1).foreach { mem =>
                println(mem)
                writeln("")
            }
            cdecl.members.takeRight(1).foreach(println)
        }
        writeln("")
    }
    
    def println(mdecl: hl.MemberDecl) {
        mdecl match {
            case decl: hl.IntervalDecl => println(decl)
            case decl: hl.MethodDecl => println(decl)
            case decl: hl.FieldDecl => println(decl)
            case decl: hl.HbDecl => println(decl)
            case decl: hl.LockDecl => println(decl)
        }
    }
    
    def println(idecl: hl.IntervalDecl) {
        idecl.annotations.foreach(println)
        idecl match {
            case hl.IntervalDecl(_, name, None, _) => {
                writeln("interval %s", name)
            }
            case hl.IntervalDecl(_, name, Some(par), _) => {
                writeln("interval %s(%s)", name, par)
            }
        }
        printlnOptBody(idecl.optBody)
    }
    
    def println(mdecl: hl.MethodDecl) {
        mdecl.annotations.foreach(println)
        writeln("%s %s", mdecl.retTref, mdecl.parts.mkString(" "))
        mdecl.requirements.foreach(println)
        printlnOptBody(mdecl.optBody)
    }
    
    def println(req: hl.Requirement) {
        writeln("%s", req)
    }
    
    def printlnOptBody(optBody: Option[hl.Block]) {
        optBody match {
            case None => writeln(";")
            case Some(body) => println(body)
        }
    }
    
    def println(fdecl: hl.FieldDecl) {
        fdecl.annotations.foreach(println)
        fdecl.tref match {
            case None => write("%s", fdecl.name)
            case Some(tref) => write("%s %s", tref, fdecl.name)
        }
        fdecl.value match {
            case None => 
            case Some(expr) => write(" = "); print(expr)
        }
        writeln(";")
    }
    
    def println(hbdecl: hl.HbDecl) {
        hbdecl.annotations.foreach(println)
        writeln("%s -> %s", hbdecl.from, hbdecl.to)
    }
    
    def println(ldecl: hl.LockDecl) {
        ldecl.annotations.foreach(println)
        writeln("%s locks %s", ldecl.interval, ldecl.lock)
    }
    
    def println(ann: hl.Annotation) {
        writeln("%s", ann)
    }
    
    def println(expr: hl.Expr) {
        print(expr)
        writeln("")
    }
    
    def print(expr: hl.Expr) {
        expr match {
            case hl.Tuple(exprs) => {
                write("(")
                exprs.dropRight(1).foreach { expr =>
                    print(expr); write(", ")
                }
                exprs.takeRight(1).foreach(print)
                write(")")
            }
            
            case hl.Block(stmts) => {
                indented("{", "}") {
                    stmts.foreach(println)                    
                }
            }
            
            case hl.Literal(obj) => write("%s", obj)
            
            case hl.Assign(l, r) => {
                print(l)
                write(" = ")
                print(r)
            }
            
            case hl.Var(name) => write("%s", name)
            
            case hl.Field(owner, name) => {
                print(owner)
                write(".%s", name)                
            }
            
            case hl.MethodCall(rcvr, parts) => {
                print(rcvr)
                write(".")
                parts.foreach { part =>
                    write("%s", part.ident)
                    print(part.arg)
                }
            }
            
            case hl.New(t, a) => {
                write("new %s", t)
                print(a)
            }
            
            case hl.ImpVoid | hl.ImpThis => write("%s", expr)

            case hl.IfElse(c, t, f) => {
                indented("(", ")") {
                    write("if(")
                    print(c)
                    write(")")
                    println(t)
                
                    write("else ")
                    println(f)                    
                }
            }

            case hl.For(l, s, b) => {
                indented("(", ")") {
                    write("for(")
                    print(l)
                    write(" : ")
                    print(s)
                    writeln(")")
                    
                    indent()
                    println(b)
                    undent()
                }
            }
            
            case hl.While(c, b) => {
                indented("(", ")") {
                    write("if(")
                    print(c)
                    writeln(")")
                    
                    indent()
                    println(b)
                    undent()
                }
            }
        }
    }
    
    def print(lv: hl.Lvalue) {
        lv match {
            case pat: hl.Pattern => write(pat.toString)
            case ex: hl.Expr => print(ex.asInstanceOf[hl.Expr])
        }
    }
    
    private[this] def printsub(stmt: hl.Stmt) {
        stmt match {
            case hl.Block(_) =>
                println(stmt)
            case _ => {
                writeln("")
                indent()
                println(stmt)
                undent()                
            }
        }
    }
    
    def println(stmt: hl.Stmt) {
        stmt match {
            case hl.IfElse(c, t, f) => {
                write("if(")
                print(c)
                write(")")
                printsub(t)                    
                write("else ")
                printsub(f)                  
            }

            case hl.For(l, s, b) => {
                write("for(")
                print(l)
                write(" : ")
                print(s)
                write(")")
                printsub(b)
            }
            
            case hl.While(c, b) => {
                write("while(")
                print(c)
                write(")")
                printsub(b)
            }
            
            case hl.Throw(e) => {
                write("throw ")
                print(e)
                writeln(";")
            }
            
            case hl.Break(None) => writeln("break;")
            case hl.Break(Some(v)) => writeln("break %s;", v)
            
            case hl.Continue(None) => writeln("continue;")
            case hl.Continue(Some(v)) => writeln("continue %s;", v)

            case hl.Return(expr) => {
                write("return ")
                print(expr)
                writeln(";")
            }
            
            case hl.Labeled(n, b) => {
                write("%s: ", n)
                println(b)
            }
            
            case hl.Block(stmts) => {
                indented("{", "}") {
                    stmts.foreach(println)
                }
                writeln("")
            }
            
            case expr: hl.Expr => {
                print(expr)
                writeln(";")
            }
        }
    }    
}

object HlPretty {
    
    object stdout extends HlPretty {
        var ind = 0
        var nl = false
        override def indent() {
            ind += 2
        }
        override def undent() {
            ind -= 2
        }
        override def write(fmt: String, args: Any*) {
            if(nl) {
                System.out.print(" " * ind)
                nl = false
            }
            System.out.print(fmt.format(args.map(_.toString): _*))
        }
        override def writeln(fmt: String, args: Any*) {
            if(nl) System.out.print(" " * ind)
            System.out.println(fmt.format(args.map(_.toString): _*))
            nl = true
        }
    }
    
}
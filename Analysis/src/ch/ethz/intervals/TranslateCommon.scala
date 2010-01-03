package ch.ethz.intervals

import javax.lang.model.element.{ElementKind => EK}

// Shared code between TranslateInterface and TranslateImplementation
class TranslateCommon(tctx: TranslateContext) {
    
    def classAttrs(telem: TypeElement) = telem.getKind match {
        case EK.INTERFACE | EK.ANNOTATION_TYPE => ir.interfaceAttrs
        case _ => ir.noAttrs
    }
    
    // Translates an erased type to an IR erased type.
    def translateTy(ty: TypeMirror): Option[ir.ClassName] =
        ty.getKind match {
            case TK.DECLARED => 
                val telem = ty.asInstanceOf[DeclaredType].asElement.asInstanceOf[TypeElement]
                Some(tctx.classAttrs(telem))
                
            case TK.ARRAY =>
                Some(ir.c_array)
            
            case TK.VOID =>
                Some(ir.c_void)
            
            case TK.BOOLEAN | TK.BYTE | TK.CHAR | TK.DOUBLE | TK.FLOAT | TK.INT | TK.LONG | TK.SHORT =>
                Some(ir.c_scalar)
            
            case _ => None
        }
        
    def superTys(telem: TypeElement) =
        telem.getSuperclass :: telem.getInterfaces
        
    def dummyClassDecl(telem: TypeElement) = 
        ir.ClassDecl(
            /* Attrs:   */  classAttrs(telem),
            /* Name:    */  tctx.className(telem),
            /* Extends: */  List(),
            /* Ghosts:  */  List(),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_init, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(m_init, List())),
                            /* goto:  */ List()
                        )
                    )
                )),
            /* Fields:  */  List(),
            /* Methods: */  List()
        )
    
    // Returns a partial class decl. including ctor, fields, methods
    def headerClassDecl(telem: TypeElement): ir.ClassDecl = 
        log.indentedRes("headerClassDecl: %s", telem) {
            at(telem, dummyClassDecl(telem)) {        
                val parser = tctx.AnnotParser(scope)
        
                val ghosts = ghostFieldsGivenValueOnElem(telem).map { case (f, s) =>
                    ir.Ghost(f, parser.path(s))
                }
        
                ir.ClassDecl(
                    /* Attrs:   */  classAttrs(telem),
                    /* Name:    */  tctx.className(telem),
                    /* Extends: */  superTys(telem).flatMap(translateTy(_).toList)
                    /* Ghosts:  */  ghosts,
                    /* Reqs:    */  List(), // TODO
                    /* Ctor:    */  List(), // Not included in header.
                    /* Fields:  */  List(), // Not included in header. 
                    /* Methods: */  List()  // Not included in header.
                )
            }            
        }
    
}
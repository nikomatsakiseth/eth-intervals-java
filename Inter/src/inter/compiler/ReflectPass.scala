package inter.compiler

import Hl.{RN => out}

object ReflectPass {
    import Hl.abs
    
    def absName(clss: Array[Class[_]]) = {
        clss.toList.map(Option(_)).flatMap {
            case None => None
            case Some(cls) if cls == classOf[Boolean] => Some(abs("java.lang.Boolean"))
            case Some(cls) if cls == classOf[Char] => Some(abs("java.lang.Character"))
            case Some(cls) if cls == classOf[Byte] => Some(abs("java.lang.Byte"))
            case Some(cls) if cls == classOf[Short] => Some(abs("java.lang.Short"))
            case Some(cls) if cls == classOf[Int] => Some(abs("java.lang.Integer"))
            case Some(cls) if cls == classOf[Long] => Some(abs("java.lang.Long"))
            case Some(cls) if cls == classOf[Float] => Some(abs("java.lang.Float"))
            case Some(cls) if cls == classOf[Double] => Some(abs("java.lang.Double"))
            case Some(cls) if cls == classOf[Unit] => Some(abs("java.lang.Void"))
            case Some(cls) => Some(abs(cls.getName))
        }
    }
    
    def apply(state: CompilationState, cls: Class[_]) {
        val classDecl = out.CompUnit(
            pkg = abs(cls.getPackage.getName),
            imports = List(),
            classes = List(
                out.ClassDecl(
                    name = cls.getSimpleName,
                    annotations = List(
                        
                        /* XXX: Convert classObj.getDeclaredAnnotations */
                    ),
                    superClasses = absName(Array(cls.getSuperclass)) ++ absName(cls.getInterfaces),
                    pattern = out.TuplePattern(List()), /* */
                    members = List() /* TODO */
                )
            )
        )
    }
}
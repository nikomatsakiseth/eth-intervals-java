package ch.ethz.intervals

import javax.lang.model.element._
import javax.lang.model.element.{ElementKind => EK}

class TranslateInterface(
    tctx: TranslateContext
) extends TranslateCommon(tctx)  {
    
    def translateTelem(telem: TypeElement) {
        val c = tctx.className(telem)
        val attrs = 
    }
    
    def translateElem(elem: Element) = elem.getKind match {
        case EK.CLASS | EK.INTERFACE | EK.ENUM | EK.ANNOTATION_TYPE =>
            translateTelem(elem.asInstanceOf[TypeElement])
            
        case _ => ()
    }
    
}
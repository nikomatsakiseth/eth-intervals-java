package harmonic.scala

/** A Fact represents an atomic unit of knowledge. 
  * It must be a value type.  A scala "case class"
  * is perfect. Also, you should derive from
  * Fact.Forward or Fact.Backward. */
sealed trait Fact {
    def kind: Fact.Kind
}

object Fact {
    /** Base type for all fact kinds */
    sealed trait Kind {
        val kindClass: Class[_ <: Fact]
    }
    
    /** Kind for facts derivable via forward-chaining */
    sealed trait ForwardKind extends Kind {
        val kindClass: Class[_ <: Fact.Forward]
    }
    
    /** Kind for facts derivable via backward-chaining */
    sealed trait BackwardKind extends Kind {
        val kindClass: Class[_ <: Fact.Backward]
    }
    
    /** Base class for forward-chainable facts */
    sealed trait Forward extends Fact {
        def kind: Fact.ForwardKind
    }

    /** Base class for backward-chainable facts */
    sealed trait Backward extends Fact {
        def kind: Fact.BackwardKind
    }
}

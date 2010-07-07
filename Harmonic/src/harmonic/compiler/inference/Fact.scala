package harmonic.compiler.inference

/** A Fact represents an atomic unit of knowledge. 
  * It must be a value type.  A scala "case class"
  * is perfect. Also, you should derive from
  * Fact.Forward or Fact.Backward. */
sealed trait Fact {
    def kind: Fact.Kind = getClass.asSubclass(classOf[Fact])
}

object Fact {
    // A fact's "Kind" is always its class
    type Kind = Class[_ <: Fact]
    type ForwardKind = Class[_ <: Fact.Forward]
    type BackwardKind = Class[_ <: Fact.Backward]
    
    /** Base class for forward-chainable facts */
    trait Forward extends Fact {
        override def kind: Fact.ForwardKind = getClass.asSubclass(classOf[Forward])
    }

    /** Base class for backward-chainable facts */
    trait Backward extends Fact {
        override def kind: Fact.BackwardKind = getClass.asSubclass(classOf[Backward])
    }
}

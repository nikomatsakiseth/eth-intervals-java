package harmonic.inference

import scala.collection.mutable.HashSet

trait Memory {

    def alpha(factKind: Fact.ForwardKind): Set[Fact.Forward]
    def beta(factKinds: List[Fact.ForwardKind]): Set[List[Fact.Forward]]
    def omega(kind: Fact.Backward): Boolean
    
    def addAlpha(fact: Fact.Forward): Boolean
    def addBeta(factKinds: List[Fact.FowardKind], factList: List[Fact.Forward]): Boolean
    def addOmega(fact: Fact.Backward): Boolean
    
}

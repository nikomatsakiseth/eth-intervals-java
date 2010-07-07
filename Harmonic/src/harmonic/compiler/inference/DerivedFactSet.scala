package harmonic.compiler.inference

import scala.collection.mutable
import scala.collection.immutable.Map
import scala.collection.immutable.Set

object DerivedFactSet {
    def apply(baseFactSet: InternalFactSet, addedFacts: Iterable[Fact]) = {
        val pending = new mutable.Queue[Fact.Forward]()
        var backwardFacts: List[Fact.Backward] = Nil
        addedFacts.foreach {
            case fact: Fact.Forward => pending.enqueue(fact)
            case fact: Fact.Backward => backwardFacts = fact :: backwardFacts
        }
        val result = new DerivedFactSet(baseFactSet, pending)
        backwardFacts.foldLeft(result)(_ addOmega _)
    }
}

class DerivedFactSet(
    baseFactSet: InternalFactSet,
    pendingFacts: mutable.Queue[Fact.Forward],
) extends InternalFactSet with Memory {
    val network = baseFactSet.network
    def plusFacts(facts: Iterable[Fact]): FactSet = DerivedFactSet(this, facts)
    
    // ___ Memory interface _________________________________________________
    //
    // Only used by Network.  Assume that the lock on this is held.
    
    private[this] var optAlphaMemories: Option[Map[Fact.ForwardKind, Set[Fact.Forward]]] = None
    private[this] var optBetaMemories: Option[Map[List[Fact.ForwardKind], Set[List[Fact.Forward]]]] = None
    private[this] var optOmegaMemories: Option[Set[Fact.Backward]] = None
    
    private[this] def alphaMemories = {
        optAlphaMemories.getOrElse(baseFactSet.resolvedAlphaMemories)
    }
    
    private[this] def betaMemories = {
        optBetaMemories.getOrElse(baseFactSet.resolvedBetaMemories)
    }
    
    private[this] def omegaMemories = {
        optOmegaMemories.getOrElse(baseFactSet.currentOmegaMemories)
    }
    
    def alpha(factKind: Fact.ForwardKind): Set[Fact.Forward] = {
        alphaMemories.getOrElse(factKind, Set())
    }
    
    def beta(factKinds: List[Fact.ForwardKind]): Set[List[Fact.Forward]] = {
        betaMemories.getOrElse(factKinds, Set())
    }
    
    def omega(fact: Fact.Backward): Boolean = {
        omegaMemories(fact)
    }
    
    def addAlpha(fact: Fact.Forward): Boolean = {
        val am = alphaMemories
        am.get(fact.kind) match {
            case Some(set) if set(fact) => {
                false
            }
            
            case None => {
                optAlphaMemories = Some(am + (fact.kind -> Set(fact)))
                true
            }
            
            case Some(set) => {
                optAlphaMemories = Some(am + (fact.kind -> (set + fact)))
                true
            }
        }
    }
    
    def addBeta(factKinds: List[Fact.FowardKind], factList: List[Fact.Forward]): Boolean = {
        val bm = betaMemories
        bm.get(factKinds) match {
            case Some(set) if set(factList) => {
                false
            }
            
            case None => {
                optBetaMemories = Some(bm + (factKinds -> Set(factList)))
                true
            }
            
            case Some(set) => {
                optBetaMemories = Some(bm + (factKinds -> (set + factList)))
                true
            }
        }
    }
    
    def addOmega(fact: Fact.Backward): Boolean = {
        val om = omegaMemories
        if(om(fact)) {
            false
        } else {
            optOmegaMemories = Some(om + fact)
            true
        }
    }
    
    // ___ InternalFactSet interface ________________________________________
    
    def resolvedAlphaMemories: Map[Fact.ForwardKind, Set[Fact.Forward]] = synchronized {
        resolvePendingFacts
        alphaMemories
    }
    
    def resolvedBetaMemories: Map[List[Fact.ForwardKind], Set[List[Fact.Forward]]] = synchronized {
        resolvePendingFacts
        betaMemories
    }
    
    def currentOmegaMemories: Set[Fact.Backward] = synchronized {
        omegaMemories
    }
        
    // ___ FactSet interface ________________________________________________
    
    private[this] def resolvePendingFacts: Unit = {
        if(!pendingFacts.isEmpty) {
            network.state(this, pendingFacts).drainQueue
        }
    }
    
    def contains(fact: Fact): Boolean = synchronized {
        if(!baseFactSet.contains(fact)) {
            resolvePendingFacts
            fact match {
                case fact: Fact.Forward =>
                    alpha(fact.kind)(fact)
                    
                case fact: Fact.Backward =>
                    if(omegaMemories(fact)) {
                        true
                    } else {
                        network.state.backwardFact(fact)
                    }
            }            
        }
    }
    
    def allFactsOfKinds(kind: Fact.ForwardKind): List[Fact] = synchronized {
        resolvePendingFacts
        val baseResult = baseFactSet.allFactsOfKind(kind)
        alphaMemories.get(kind) {
            case Some(mem) => mem.allFacts.foldLeft(baseResult) { (r, f) => f :: r }
            case None => baseResult
        }
    }
    
}

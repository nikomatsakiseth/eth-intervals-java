package harmonic.compiler.inference

import scala.collection.mutable
import scala.collection.immutable.Map
import scala.collection.immutable.Set

import com.smallcultfollowing.lathos.model._
import com.smallcultfollowing.lathos.model.{Util => LathosUtil}

import harmonic.compiler.Util._

object DerivedFactSet {
    private[this] def create[X](
        baseFactSet: InternalFactSet[X],
        pendingFacts: mutable.Queue[Fact.Forward],
        backwardFacts: Iterable[Fact.Backward],
        xtra: X
    ) = {
        val result = new DerivedFactSet(baseFactSet, pendingFacts, xtra)
        backwardFacts.foreach(result.addOmega(_))
        result
    }
    
    def apply[X](baseFactSet: InternalFactSet[X], addedFacts: Iterable[Fact], xtra: X): FactSet[X] = {
        val pendingFacts = new mutable.Queue[Fact.Forward]()
        var backwardFacts: List[Fact.Backward] = Nil
        addedFacts.foreach {
            case fact: Fact.Forward => pendingFacts.enqueue(fact)
            case fact: Fact.Backward => backwardFacts = fact :: backwardFacts
        }
        create(baseFactSet, pendingFacts, backwardFacts, xtra)
    }
    
    def apply[X](baseFactSet: InternalFactSet[X], addedFactSet: FactSet[X], xtra: X): FactSet[X] = {
        // TODO: If we make FactSets an inner-class of network, can we enforce this statically?
        assert(baseFactSet.network == addedFactSet.network) 
        val pendingFacts = new mutable.Queue[Fact.Forward]()
        val internalFactSet = addedFactSet.asInstanceOf[InternalFactSet[X]]
        internalFactSet.resolvedAlphaMemories.values.foreach { alphaFacts =>
            pendingFacts ++= alphaFacts
        }
        val backwardFacts = internalFactSet.currentOmegaMemories
        create(baseFactSet, pendingFacts, backwardFacts, xtra)
    }
}

class DerivedFactSet[X](
    baseFactSet: InternalFactSet[X],
    pendingFacts: mutable.Queue[Fact.Forward],
    xtra: X
) extends InternalFactSet[X] with Memory with DebugPage {
    val network = baseFactSet.network
    def plusFacts(facts: Iterable[Fact], xtra: X): FactSet[X] = DerivedFactSet(this, facts, xtra)
    def plusFactSet(factSet: FactSet[X], xtra: X): FactSet[X] = DerivedFactSet(this, factSet, xtra)
    
    // ___ Memory interface _________________________________________________
    //
    // Only used by Network.  Assume that the lock on this is held.
    
    private[this] var optAlphaMemories: Option[Map[Fact.ForwardKind, Set[Fact.Forward]]] = None
    private[this] var optBetaMemories: Option[Map[List[Fact.ForwardKind], Set[List[Fact.Forward]]]] = None
    private[this] var optOmegaMemories: Option[Set[Fact.Backward]] = None
    
    private[this] def alphaMemories: Map[Fact.ForwardKind, Set[Fact.Forward]] = {
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
    
    def addBeta(factKinds: List[Fact.ForwardKind], factList: List[Fact.Forward]): Boolean = {
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
            network.drainQueue(this, this, xtra, pendingFacts)
        }
    }
    
    override def contains(fact: Fact): Boolean = synchronized {
        if(baseFactSet.contains(fact)) {
            true
        } else {
            resolvePendingFacts
            fact match {
                case fact: Fact.Forward =>
                    alpha(fact.kind)(fact)
                    
                case fact: Fact.Backward =>
                    if(omegaMemories(fact)) {
                        true
                    } else {
                        network.contains(this, this, xtra, pendingFacts)(fact)
                    }
            }            
        }
    }
    
    override def allFactsOfKind(kind: Fact.ForwardKind): Set[Fact.Forward] = synchronized {
        resolvePendingFacts
        alphaMemories.getOrElse(kind, Set())
    }
    
}

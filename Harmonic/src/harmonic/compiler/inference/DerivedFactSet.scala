package harmonic.compiler.inference

import scala.collection.mutable
import scala.collection.immutable.Map
import scala.collection.immutable.Set

import com.smallcultfollowing.lathos.model._
import com.smallcultfollowing.lathos.model.{Util => LathosUtil}

import harmonic.compiler.Util._

object DerivedFactSet {
    def apply(baseFactSet: InternalFactSet, addedFacts: Iterable[Fact]) = {
        val pending = new mutable.Queue[Fact.Forward]()
        var backwardFacts: List[Fact.Backward] = Nil
        addedFacts.foreach {
            case fact: Fact.Forward => pending.enqueue(fact)
            case fact: Fact.Backward => backwardFacts = fact :: backwardFacts
        }
        val result = new DerivedFactSet(baseFactSet, pending)
        backwardFacts.foreach(result.addOmega(_))
        result
    }
}

class DerivedFactSet(
    baseFactSet: InternalFactSet,
    pendingFacts: mutable.Queue[Fact.Forward]
) extends InternalFactSet with Memory with Page {
    val network = baseFactSet.network
    def plusFacts(facts: Iterable[Fact]): FactSet = DerivedFactSet(this, facts)
    
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
            network.state(this, pendingFacts).drainQueue
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
                        network.state(this, pendingFacts).backwardFact(fact)
                    }
            }            
        }
    }
    
    override def allFactsOfKind(kind: Fact.ForwardKind): Set[Fact.Forward] = synchronized {
        resolvePendingFacts
        alphaMemories.getOrElse(kind, Set())
    }
    
    // ___ Page interface ___________________________________________________
    
    override def toString = getId
    
    override def getId = "DerivedFactSet[%s]".format(System.identityHashCode(this))
    
    override def getParent = null
    
    override def addContent(content: PageContent) = throw new UnsupportedOperationException()
    
    override def renderInLine(out: Output): Unit = {
        LathosUtil.renderInLine(this, out)
    }
    
    override def renderInPage(out: Output): Unit = {
        out.startPage(this)
        
        out.startTable
        out.row("Derived from", baseFactSet)
        out.row("network", network)
        out.endTable
        
        out.subpage("Alpha") {
            out.map(alphaMemories)
        }

        out.subpage("Beta") {
            out.map(betaMemories)
        }

        out.subpage("Omega") {
            out.list(omegaMemories)
        }
        
        out.endPage(this)
    }
    
}

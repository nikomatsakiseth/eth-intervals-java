package harmonic.compiler.inference

import scala.collection.mutable
import scala.collection.immutable.Map
import scala.collection.immutable.Set

import ch.ethz.intervals.Intervals

import com.smallcultfollowing.lathos
import com.smallcultfollowing.lathos.Lathos

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
        val addedMem = addedFactSet.asInstanceOf[InternalFactSet[X]].resolvedMemories
        addedMem.alphas.values.foreach { alphaFacts =>
            pendingFacts ++= alphaFacts
        }
        create(baseFactSet, pendingFacts, addedMem.omegas, xtra)
    }
}

class DerivedFactSet[X](
    baseFactSet: InternalFactSet[X],
    pendingFacts: mutable.Queue[Fact.Forward],
    val xtra: X
) extends InternalFactSet[X] with Memory with DebugPage {
    val network = baseFactSet.network
    val lock = Intervals.lock(getId)
    
    def plusFacts(facts: Iterable[Fact], xtra: X): FactSet[X] = DerivedFactSet(this, facts, xtra)
    def plusFactSet(factSet: FactSet[X], xtra: X): FactSet[X] = DerivedFactSet(this, factSet, xtra)
    
    // ___ Memory interface _________________________________________________
    //
    // Only used by Network.  Assume that `lock` is held.  Note that to actually
    // MODIFY the optAlphaMemories, optBetaMemories, and optOmegaMemories 
    // fields, we briefly acquire the synchronized lock.  This is to coordinate
    // with the DebugPage trait, which may be rendering a web page at that moment.
    
    @lathos.Ignore 
    private[this] var optMemories: Option[Memories] = None
    
    private[this] def memories = {
        optMemories.getOrElse(baseFactSet.resolvedMemories)
    }
    
    private[this] def setMemories(memories: Memories) = synchronized {
        optMemories = Some(memories)
    }
    
    def alpha(factKind: Fact.ForwardKind): Set[Fact.Forward] = {
        memories.alphas.getOrElse(factKind, Set())
    }
    
    def beta(factKinds: List[Fact.ForwardKind]): Set[List[Fact.Forward]] = {
        memories.betas.getOrElse(factKinds, Set())
    }
    
    def omega(fact: Fact.Backward): Boolean = {
        memories.omegas(fact)
    }
    
    def addAlpha(fact: Fact.Forward): Boolean = {
        val mem = memories
        mem.alphas.get(fact.kind) match {
            case Some(set) if set(fact) => {
                false
            }
            
            case None => {
                val newValue = mem.alphas + (fact.kind -> Set(fact))
                setMemories(mem.copy(alphas = newValue))
                true
            }
            
            case Some(set) => {
                val newValue = mem.alphas + (fact.kind -> (set + fact))
                setMemories(mem.copy(alphas = newValue))
                true
            }
        }
    }
    
    def addBeta(factKinds: List[Fact.ForwardKind], factList: List[Fact.Forward]): Boolean = {
        val mem = memories
        mem.betas.get(factKinds) match {
            case Some(set) if set(factList) => {
                false
            }
            
            case None => {
                val newValue = mem.betas + (factKinds -> Set(factList))
                setMemories(mem.copy(betas = newValue))
                true
            }
            
            case Some(set) => {
                val newValue = mem.betas + (factKinds -> (set + factList))
                setMemories(mem.copy(betas = newValue))
                true
            }
        }
    }
    
    def addOmega(fact: Fact.Backward): Boolean = {
        val mem = memories
        if(mem.omegas(fact)) {
            false
        } else {
            val newValue = mem.omegas + fact
            setMemories(mem.copy(omegas = newValue))
            true
        }
    }
    
    // ___ InternalFactSet interface ________________________________________
    
    private[this] def resolvePendingFacts: Unit = {
        if(!pendingFacts.isEmpty) {
            network.state(this, this, xtra, pendingFacts).drainQueue
        }
    }
    
    override def resolvedMemories = withLock(lock) {
        resolvePendingFacts
        memories
    }
        
    // ___ FactSet interface ________________________________________________
    
    override def contains(fact: Fact): Boolean = withLock(lock) {
        Lathos.para(Lathos.context.server, this, "contains(", fact, ")")
        network.state(this, this, xtra, pendingFacts).contains(fact)
    }
    
    override def query[F <: Fact.Forward](
        kind: Class[F], 
        optArgs: Option[Any]*
    ): Set[F] = withLock(lock) {
        Lathos.para(Lathos.context.server, this, "query(", kind, ": ", optArgs, ")")
        network.state(this, this, xtra, pendingFacts).query(kind, optArgs: _*)
    }
    
    // ___ Lathos ___________________________________________________________
    
    protected[this] override def renderMoreFields(out: lathos.Output): Unit = {
        optMemories.foreach { mem => 
            for((kind, facts) <- mem.alphas) {
                out.par { out.bolded { out.outputText(kind.getSimpleName) } }
                out.list(facts)
            }
        }
    }
    
}

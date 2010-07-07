package harmonic.compiler.inference

import scala.collection.mutable
import Breaks._

class Network {
    
    class Alpha(
        kind: Fact.ForwardKind
    ) {
        val betas = new mutable.ListBuffer[Beta]()
        
        def add(state: State, fact: Fact.Forward) = {
            assert(fact.kind == kind)
            if(mem.addAlpha(fact)) {
                betas.foreach(_.added(state, fact))                
            }
        }
    }
    
    trait Suffix[+F <: Fact] {
        def suffixes(state: State): Iterable[List[F]]
    }
    
    object EmptySuffix extends Suffix[Fact.Forward] {
        override def suffixes(state: State) = Nil        
    }

    trait Join[+F <: Fact] extends Suffix[F] {
        def added(state: State, fact: F): Unit
    }
    
    class Beta(
        kinds: List[Fact.ForwardKind],
        rhs: SuffixProducing[Fact.Forward]
    ) extends Join[Fact.Forward] {
        val rules = new mutable.ListBuffer[Rule.Forward]()
        
        override def suffixes(state: State) = state.mem.beta(kinds)
        
        override def added(state: State, fact: Fact.Forward) = {
            val factLists = rhs.suffixes(state).map(factList => fact :: factList).toList
            factLists.foreach { factList =>
                if(mem.addBeta(factList)) {
                    templates.foreach { template =>
                        val rule = template.instantiate(factLists)
                        state.queue ++= rule.derive(state)                        
                    }
                }
            }
        }
    }
    
    class Omega(
        kind: Fact.BackwardKind
    ) {
        val rules = new mutable.ListBuffer[Rule.Backward]()
        
        def derive(state: State): Boolean = {
            if(state.mem.omega(fact)) {
                return true
            } else {
                for(template <- templates) {
                    val rule = template.instantiate(fact)
                    if(rule.canInfer(state, fact)) {
                        mem.addOmega(fact)
                        return true
                    }
                }
                return false
            }
        }
   
    }
    
    class State(
        val mem: Memory,
        val queue: mutable.Queue[Fact.Forward]
    ) {
        private[this] var backwardStack: List[Fact.Backward] = Nil
        
        def drainQueue() = {
            while(!queue.isEmpty) {
                val fact = queue.dequeue
                alphaNodes.get(fact.kind).foreach(_.add(this, fact))
            }
        }
        
        def forwardFacts(factKind: Fact.ForwardKind) = {
            state.mem.alpha(factKind)
        }

        def backwardFact(fact: Fact.Backward) = {
            if(backwardStack.contains(fact)) {
                false
            } else {
                backwardStack = fact :: backwardStack
                omegaNodes.get(fact.kind).exists(_.resolve(this))                
                backwardStack = backwardStack.tail
            }
        }
    }
    
    def state(mem: Memory, queue: mutable.Queue[Fact.Forward]) = {
        new State(mem, queue)
    }

    // ___ Defining and storing the network _________________________________
    //
    // Network should be defined completely before use 
    // and not modified thereafter.

    val alphaNodes = new mutable.HashSet[Fact.ForwardKind, Alpha]()
    val betaNodes = new mutable.HashSet[List[Fact.ForwardKind], Beta]()
    val omegaNodes = new mutable.HashSet[Fact.BackwardKind, Omega]()

    private[this] addAlpha(kind: Fact.ForwardKind): Alpha = {
        alphaNodes.get(kind).getOrElse {
            val alpha = new Alpha(kind)
            alphaNodes(kind) = alpha
            alpha
        }
    }
    
    private[this] addBeta(kinds: List[Fact.ForwardKind]): Beta = {
        betaNodes.get(kinds).getOrElse {
            val alpha = addAlpha(kinds.head)
            
            val rhs = {
                if(kinds.length <= 1) EmptySuffix
                else addBeta(kinds.tail)
            }
            
            val beta = new Beta(kinds, rhs)
            alphas.betas += beta
            
            betaNodes(kind) = beta
            beta
        }
    }
    
    private[this] addOmega(kind: Fact.BackwardKind): Omega = {
        omegaNodes.get(kind).getOrElse {
            val omega = new Omega(kind)
            omegaNodes(kind) = omega
            omega
        }
    }
    
    def addRule(rule: Rule) = rule match {
        case rule: Rule.Forward => {
            val beta = addBeta(rule.inputKinds)
            beta.rules += rule
        }
        
        case rule: Rule.Backward => {
            val omega = addOmega(rule.outputKind)
            omega.rules += rule
        }
    }
    
}
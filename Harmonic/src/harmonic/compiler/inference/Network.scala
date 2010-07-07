package harmonic.compiler.inference

import scala.collection.mutable

class Network {
    
    class State(
        val mem: Memory,
        val queue: mutable.Queue[Fact.Forward]
    ) {
        var backwardStack: List[(Fact.Backward, Rule.Backward)] = Nil
        
        def drainQueue() = {
            while(!queue.isEmpty) {
                val fact = queue.dequeue
                alphaNodes.get(fact.kind).foreach(_.add(this, fact))
            }
        }
        
        /** Returns facts of the given kind known so far. */
        def forwardFacts(factKind: Fact.ForwardKind) = {
            mem.alpha(factKind)
        }

        /** Returns true if `fact` can be established at this time. */
        def backwardFact(fact: Fact.Backward) = {
            omegaNodes.get(fact.kind).exists(_.derive(this, fact))                
        }
    }
    
    def state(mem: Memory, queue: mutable.Queue[Fact.Forward]) = {
        new State(mem, queue)
    }
    
    class Alpha(
        kind: Fact.ForwardKind
    ) {
        val betas = new mutable.ListBuffer[Beta]()
        
        def add(state: State, fact: Fact.Forward) = {
            assert(fact.kind == kind)
            if(state.mem.addAlpha(fact)) {
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

    class Beta(
        kinds: List[Fact.ForwardKind],
        rhs: Suffix[Fact.Forward]
    ) extends Suffix[Fact.Forward] {
        val rules = new mutable.ListBuffer[Rule.Forward]()
        
        override def suffixes(state: State) = state.mem.beta(kinds)
        
        override def added(state: State, fact: Fact.Forward) = {
            val factLists = rhs.suffixes(state).map(factList => fact :: factList).toList
            factLists.foreach { factList =>
                if(state.mem.addBeta(kinds, factList)) {
                    rules.foreach { rule =>
                        state.queue ++= rule.derive(state, factList)                        
                    }
                }
            }
        }
    }
    
    class Omega(
        kind: Fact.BackwardKind
    ) {
        val rules = new mutable.ListBuffer[Rule.Backward]()
        
        def derive(state: State, fact: Fact.Backward): Boolean = {
            if(state.mem.omega(fact)) {
                return true
            } else {
                for(rule <- rules) {
                    if(!state.backwardStack.contains((fact, rule))) {
                        state.backwardStack = (fact, rule) :: state.backwardStack
                        try {
                            if(rule.canInfer(state, fact)) {
                                state.mem.addOmega(fact)
                                return true
                            }
                        } finally {
                            state.backwardStack = state.backwardStack.tail
                        }
                    }
                }
                return false
            }
        }
   
    }

    // ___ Defining and storing the network _________________________________
    //
    // Network should be defined completely before use 
    // and not modified thereafter.

    val alphaNodes = new mutable.HashMap[Fact.ForwardKind, Alpha]()
    val betaNodes = new mutable.HashMap[List[Fact.ForwardKind], Beta]()
    val omegaNodes = new mutable.HashMap[Fact.BackwardKind, Omega]()

    private[this] def addAlpha(kind: Fact.ForwardKind): Alpha = {
        alphaNodes.get(kind).getOrElse {
            val alpha = new Alpha(kind)
            alphaNodes(kind) = alpha
            alpha
        }
    }
    
    private[this] def addBeta(kinds: List[Fact.ForwardKind]): Beta = {
        betaNodes.get(kinds).getOrElse {
            val alpha = addAlpha(kinds.head)
            
            val rhs = {
                if(kinds.length <= 1) EmptySuffix
                else addBeta(kinds.tail)
            }
            
            val beta = new Beta(kinds, rhs)
            alpha.betas += beta
            
            betaNodes(kinds) = beta
            beta
        }
    }
    
    private[this] def addOmega(kind: Fact.BackwardKind): Omega = {
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
package harmonic.compiler.inference

import scala.collection.mutable

import com.smallcultfollowing.lathos

import harmonic.compiler.Util._

class Network(server: lathos.model.LathosServer) extends DebugPage {
    
    class State(
        page: lathos.model.Page,
        val mem: Memory,
        val queue: mutable.Queue[Fact.Forward]
    ) {
        val log = server.contextForPage(page)
        var backwardStack: List[(Fact.Backward, Rule.Backward)] = Nil
        
        def drainQueue() = log.indent("drainQueue") {
            while(!queue.isEmpty) {
                val fact = queue.dequeue
                log.indent("Fact ", fact) {
                    alphaNodes.get(fact.kind) match {
                        case Some(alpha) => alpha.add(this, fact)
                        case None => mem.addAlpha(fact)
                    }              
                }
            }
        }
        
        /** Returns facts of the given kind known so far. */
        def forwardFacts(factKind: Fact.ForwardKind) = log.indent("forwardFacts(", factKind, ")") {
            mem.alpha(factKind)
        }

        /** Returns true if `fact` can be established at this time. */
        def backwardFact(fact: Fact.Backward) = log.indent("backwardFact(", fact, ")") {
            omegaNodes.get(fact.kind).exists(_.derive(this, fact))                
        }
        
        /** Returns true if `fact` can be established at this time. */
        def contains(fact: Fact) = {
            fact match {
                case fact: Fact.Backward => backwardFact(fact)
                case fact: Fact.Forward => forwardFacts(fact.kind)(fact)
            }
        }
    }
    
    def state(page: lathos.model.Page, mem: Memory, queue: mutable.Queue[Fact.Forward]) = {
        new State(page, mem, queue)
    }
    
    trait Node extends DebugPage
    
    class Alpha(
        val kind: Fact.ForwardKind
    ) extends Node {
        val betas = new mutable.ListBuffer[Beta]()
        
        def get(state: State) = state.mem.alpha(kind)
        
        def add(state: State, fact: Fact.Forward) = {
            state.log.indent(this, ".add(", fact, ")") {
                assert(fact.kind == kind)
                if(state.mem.addAlpha(fact)) {
                    betas.foreach(_.factAdded(state, fact))                
                }
            }            
        }
    }
    
    trait Rhs extends Node {
        def get(state: State): Iterable[List[Fact.Forward]]
        def addBeta(beta: Beta): Unit
    }
    
    object EmptyRhs extends Rhs {
        override def get(state: State) = List(Nil)    
        def addBeta(beta: Beta) = ()
    }

    class Beta(
        val kinds: List[Fact.ForwardKind],
        val alpha: Alpha,
        val rhs: Rhs
    ) extends Rhs {
        val betas = new mutable.ListBuffer[Beta]()
        val rules = new mutable.ListBuffer[Rule.Forward]()
        
        override def toString = "Beta[%s]".format(kinds.map(_.getName).mkString(", "))
        
        override def get(state: State) = state.mem.beta(kinds)
        
        override def addBeta(beta: Beta) = (betas += beta)
        
        private[this] def newFactList(state: State, factList: List[Fact.Forward]): Unit = {
            state.log.log(this, ".newFactList(", factList, ")")
            if(state.mem.addBeta(kinds, factList)) {
                rules.foreach { rule =>
                    state.queue ++= rule.derive(state, factList)                        
                }
                
                betas.foreach(_.suffixAdded(state, factList))
            }
        }
        
        def factAdded(state: State, fact: Fact.Forward): Unit = {
            state.log.indent(this, ".factAdded(", fact, ")") {
                rhs.get(state).foreach { suffix => newFactList(state, fact :: suffix) }
            }            
        }
        
        def suffixAdded(state: State, suffix: List[Fact.Forward]): Unit = {
            state.log.indent(this, ".suffixAdded(", suffix, ")") {
                alpha.get(state).foreach { fact => newFactList(state, fact :: suffix) }
            }
        }
    }
    
    class Omega(
        val kind: Fact.BackwardKind
    ) extends Node {
        val rules = new mutable.ListBuffer[Rule.Backward]()
        
        def derive(state: State, fact: Fact.Backward): Boolean = {
            if(state.mem.omega(fact)) {
                state.log.log("Fact already known: ", fact)
                return true
            } else {
                state.log.indent(this, ".derive(", fact, ")") {
                    for(rule <- rules) {
                        if(!state.backwardStack.contains((fact, rule))) {
                            state.backwardStack = (fact, rule) :: state.backwardStack
                            try {
                                state.log.log("Invoking rule ", rule)
                                if(rule.canInfer(state, fact)) {
                                    state.mem.addOmega(fact)
                                    return true
                                }
                            } finally {
                                state.backwardStack = state.backwardStack.tail
                            }
                        } else {
                            state.log.log("Not invoking rule ", rule, " as it would cause an infinite loop.")
                        }
                    }
                    return false
                }
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
                if(kinds.length <= 1) EmptyRhs
                else addBeta(kinds.tail)
            }
            
            val beta = new Beta(kinds, alpha, rhs)
            rhs.addBeta(beta)
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
    
    def addRule(rule: Rule) = {
        val log = {
            if(true)
                server.contextForPage(this)
            else
                new lathos.none.NoneContext(server)
        }
        
        log.indent("addRule(", rule, ")") {
            rule match {
                case rule: Rule.Forward => {
                    val beta = addBeta(rule.inputKinds)
                    beta.rules += rule
                    log.log("Forward rule with inputKinds ", rule.inputKinds, " added to ", beta)
                }

                case rule: Rule.Backward => {
                    val omega = addOmega(rule.outputKind)
                    omega.rules += rule
                    log.log("Backward rule with outputKind ", rule.outputKind, " added to ", omega)
                }
            }            
        }
    }
    
}
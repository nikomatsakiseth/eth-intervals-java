package harmonic.compiler.inference

import scala.collection.mutable

import com.smallcultfollowing.lathos
import com.smallcultfollowing.lathos.Ignore

import harmonic.compiler.Util._

/** The Network defines the rules that will execute when facts are
  * added or queried.  It combines a simple Rete network (forward prop.)
  * with a backwards-propagating network.  The backward propagating nodes
  * may recurse and make additional queries. They may also query the
  * forward propagating nodes.
  * 
  * X is the type of the eXtra data that will be supplied to various rules. 
  */
class Network[X](server: lathos.LathosServer) extends DebugPage {
    
    def emptyFactSet(xtra: X) = EmptyFactSet.plusFacts(Nil, xtra)
    
    /** This is a bit of a hack: it allows us to artificially 
      * add forward facts before a forwards query is processed. 
      * We use it to add PathExists() and TypeExists() facts. */
    protected[this] def prequery(kind: Fact.Kind, args: Array[Option[Any]]): Iterable[Fact.Forward] = Nil
    
    /** This is a bit of a hack: it allows us to artificially 
      * add forward facts before a backwards query is processed. 
      * We use it to add PathExists() and TypeExists() facts. */
    protected[this] def precontains(fact: Fact): Iterable[Fact.Forward] = Nil
    
    object EmptyFactSet extends InternalFactSet[X] {
        def network = Network.this
        def contains(fact: Fact): Boolean = false
        def query[F <: Fact.Forward](
            kind: Class[F], 
            optArgs: Option[Any]*
        ): Set[F] = Set()
        def plusFacts(facts: Iterable[Fact], xtra: X): FactSet[X] = DerivedFactSet(this, facts, xtra)
        def plusFactSet(factSet: FactSet[X], xtra: X): FactSet[X] = DerivedFactSet(this, factSet, xtra)
        def resolvedAlphaMemories = Map()
        def resolvedBetaMemories = Map()
        def currentOmegaMemories = Set()
    }
    
    def state(
        page: lathos.Page,
        mem: Memory,
        xtra: X,
        queue: mutable.Queue[Fact.Forward]
    ) = new State(page, mem, xtra, queue)
    
    class State(
        page: lathos.Page,
        val mem: Memory,
        val xtra: X,
        val queue: mutable.Queue[Fact.Forward]
    ) extends Recurse[X] {
        val log = lathos.Lathos.context
        
        var backwardStack: List[(Fact.Backward, Rule.Backward[X])] = Nil
        
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
        
        /** See FactSet.queryAll */
        def queryAll(
            kind: Fact.ForwardKind
        ): Set[Fact.Forward] = log.indent("queueAll(", kind, ")") {
            drainQueue
            mem.alpha(kind)
        }
        
        /** See FactSet.queryN */
        def query[F <: Fact.Forward](
            kind: Class[F], 
            optArgs: Option[Any]*
        ): Set[F] = log.indent("queueN(", kind, ", ", optArgs, ")") {
            queue ++= prequery(kind, optArgs.toArray)
            val allFacts = queryAll(kind)
            if(optArgs.length == 0) {
                allFacts.map(kind.cast)
            } else {
                val queryArgs = optArgs.zipWithIndex
                allFacts.flatMap { fact =>
                    val matches = queryArgs.forall { 
                        case (None, _) => true
                        case (Some(v), idx) => (fact.productElement(idx) == v)
                    }
                    matches toOption kind.cast(fact)
                }                
            }
        }

        /** Returns true if `fact` can be established at this time. */
        def backwardFact(fact: Fact.Backward) = log.indent("backwardFact(", fact, ")") {
            omegaNodes.get(fact.kind).exists(_.derive(this, fact))                
        }
        
        /** Returns true if `fact` can be established at this time. 
          *
          * Note: Only invokable from backward rules. */
        def contains(fact: Fact) = log.indent("contains(", fact, ")") {
            queue ++= precontains(fact)
            fact match {
                case fact: Fact.Backward => backwardFact(fact)
                case fact: Fact.Forward => queryAll(fact.kind)(fact)
            }
        }
    }
    
    trait Node extends DebugPage
    
    /** Alpha nodes represent the set of facts of a particular kind. 
      * Their only task is to notify the beta nodes when new facts
      * are added. */
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

    /** Beta nodes represent a unique combination of facts of various kinds. 
      * They also contain a list of rules to execute each time a new 
      * fact set is generated. */
    class Beta(
        val kinds: List[Fact.ForwardKind],
        val alpha: Alpha,
        val rhs: Rhs
    ) extends Rhs {
        val betas = new mutable.ListBuffer[Beta]()
        val rules = new mutable.ListBuffer[Rule.Forward[X]]()
        
        override def toString = "Beta[%s]".format(kinds.map(_.getName).mkString(", "))
        
        override def get(state: State) = state.mem.beta(kinds)
        
        override def addBeta(beta: Beta) = (betas += beta)
        
        private[this] def newFactList(state: State, factList: List[Fact.Forward]): Unit = {
            state.log.log(this, ".newFactList(", factList, ")")
            if(state.mem.addBeta(kinds, factList)) {
                rules.foreach { rule =>
                    state.queue ++= rule.derive(state.xtra, factList)                        
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
    
    /** Omega nodes store the rules that handle backwards queries. */
    class Omega(
        val kind: Fact.BackwardKind
    ) extends Node {
        val rules = new mutable.ListBuffer[Rule.Backward[X]]()
        
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

    @Ignore val alphaNodes = new mutable.HashMap[Fact.ForwardKind, Alpha]()
    @Ignore val betaNodes = new mutable.HashMap[List[Fact.ForwardKind], Beta]()
    @Ignore val omegaNodes = new mutable.HashMap[Fact.BackwardKind, Omega]()

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
    
    def addRule(rule: Rule[X]) = {
        val log = {
            if(true)
                server.contextForPage(this)
            else
                new lathos.NoneContext(server)
        }
        
        log.indent("addRule(", rule, ")") {
            rule match {
                case rule: Rule.Forward[X] => {
                    val beta = addBeta(rule.inputKinds)
                    beta.rules += rule
                    log.log("Forward rule with inputKinds ", rule.inputKinds, " added to ", beta)
                }

                case rule: Rule.Backward[X] => {
                    val omega = addOmega(rule.outputKind)
                    omega.rules += rule
                    log.log("Backward rule with outputKind ", rule.outputKind, " added to ", omega)
                }
            }            
        }
    }
    
}
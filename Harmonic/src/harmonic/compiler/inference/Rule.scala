package harmonic.compiler.inference

import java.lang.reflect

sealed trait Rule

object Rule {
    private[this] def getTriggerMethod(cls: Class[_]): reflect.Method = {
        cls.getDeclaredMethods.find(_.getName == "trigger") match {
            case Some(m) => 
                m
            case None if (cls.getSuperclass == null) => 
                throw new RuntimeException("No trigger(...) method defined in %s".format(getClass))
            case None =>
                getTriggerMethod(cls.getSuperclass)
        }
    }
    
    private[this] def checkBasics(
        m: reflect.Method,
        returnClass: Class[_],
        minParams: Int,
        maxParams: Int
    ): reflect.Method = {
        if(!returnClass.isAssignableFrom(m.getReturnType)) {
            throw new RuntimeException("%s must return Iterable[Fact.Forward]".format(m))
        }

        if(m.getParameterTypes.length < minParams) {
            throw new RuntimeException("%s must take at least %d parameters".format(m, minParams))
        }
        
        if(m.getParameterTypes.length > maxParams) {
            throw new RuntimeException("%s must take at most %d parameters".format(m, minParams))
        }
        
        if(!m.getParameterTypes.apply(0).isAssignableFrom(classOf[Network#State])) {
            throw new RuntimeException("First argument of %s must have type Network#State".format(m))
        }
        
        m.setAccessible(true)
        
        m
    }
    
    trait Forward extends Rule {
        def inputKinds: List[Fact.ForwardKind]
        def derive(state: Network#State, facts: List[Fact.Forward]): Iterable[Fact.Forward]        
    }
    
    // Simply define a method trigger(state, fact1, fact2, fact3) and this
    // trait uncovers the input kinds by reflection.
    trait ReflectiveForward extends Forward {
        val method = checkBasics(getTriggerMethod(getClass), classOf[Iterable[Any]], 2, Integer.MAX_VALUE)
        
        val inputKinds = {
            try {
                method.getParameterTypes.toList.drop(1).map(_.asSubclass(classOf[Fact.Forward]))                   
            } catch {
                case c: ClassCastException => {
                    throw new RuntimeException("All fact arguments of %s must be forward facts".format(method), c)
                }
            }
        }
        
        def derive(state: Network#State, facts: List[Fact.Forward]): Iterable[Fact.Forward] = {
            method.invoke(this, (state :: facts): _*).asInstanceOf[Iterable[Any]].map(_.asInstanceOf[Fact.Forward])
        }
    }
    
    trait Backward extends Rule {
        def outputKind: Fact.BackwardKind
        def canInfer(state: Network#State, fact: Fact.Backward): Boolean        
    }
    
    // Simply define a method canInfer(state, fact) and the outputKind
    // will determined by reflection.
    trait ReflectiveBackward extends Backward {
        val method = checkBasics(getTriggerMethod(getClass), classOf[Boolean], 2, 2)
        
        val outputKind = {
            try {
                method.getParameterTypes.apply(1).asSubclass(classOf[Fact.Backward])                
            } catch {
                case c: ClassCastException => {
                    throw new RuntimeException("%s must take a backward fact argument".format(method), c)
                }
            }
        }
        
        def canInfer(state: Network#State, fact: Fact.Backward): Boolean = {
            method.invoke(this, state, fact).asInstanceOf[Boolean].booleanValue
        }
    }
    
}

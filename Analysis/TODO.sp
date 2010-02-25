___ To Do List _______________________________________________________

______ OOPSLA Bugs ___________________________________________________   

- Assertions

  || a hb b              | user's pnt.hb(pnt) ||
  || a locks b           | user's foo.holdsLock(bar) ||
  || a readableBy b      | user's foo.isReadable() translates to "foo readableBy method" ||
  || a writableBy b      | user's foo.isWritable() translates to "foo writableBy method" ||
  || a == b              | creates a temporary alias from b to a? ||
  || a == b->p           | creates a temporary alias from b->p to a ||

- Disallow a field f with a path type whose path is "this.f"

- Introduce immutableIn to supplement readableBy?

  This used to be covered by <f: hb this>.  An alternative would be
  to keep "hb this" but just not add it to the HB relation.
  
- Covariant type parameters to handle Array[]

- Make java.lang.Object a subtype of c_any
  
- Check in multiple inheritance that all paths lead to the same set of ghost parameters.
  Alternatively, make sure that all paths are fulfilled. Latter technique is "cooler" (then,
  for ex., a class modelling an empty set can fulfill virtually any type, etc)
    
- Static fields, methods

- Adjust "addRelation" methods

  We should assert subclass when adding relations directly, but only
  couldHaveClass when adding from method requirements.

______ Bugs __________________________________________________________   

- Check that there are no fields named f_objCtor declared anywhere

- Store the environments computed from a constructor in {.class} file

  As an interim measure we can also ensure that the implementation of
  the constructors from all supertypes is included, but this is not a 
  general solution.
  
- Method type parameters

______ Improvements to existing features _____________________________

- Require that a bound ghost object of type c be a subtype of
  {c @Constructor[c](hbNow)}?  In other words, //at least// the
  class {c} must be fully constructed.
  
- Check type args in wf check

- Sanity check casts or otherwise restrict them?

- Circular canonical paths

  [[@test-plugin/src/basic/CircularGhostA.java]] should conclude that the circular ghosts
  are equal (which it doesn't do now).
  
  Three possibilities:
  - The canon() function could return a set of canonical paths.
  - A {CanonPath} instance could be a set of path, type pairs.  
  - Use some strategy to pick "the best" of the options in the cycle.  Prefer reified with
    more specific types, longer paths, something like that.
  The last is clearly the least disruptive.

- More sophisticated merging with respect to temp/perm

  If all preds have the same effective mapping, but some in temp and some in pred, the succ
  should have the mapping in temp. For example:
  {
      if(...) {
          b = ...; // immutable object
          a = b.f; // permanent!
      } else {
          b = ...; // mutable object
          a = b.f; // temporary
      }
      // safe if b.f→a temporarily        
  }
 
- Extend canonical paths to allow fields of ghosts: we can use the
  class of the ghost type to determine what ghost/reified field exist,
  and can always produce a class lower-bound for the type.
  
- Improve enforcement of constraint that an interval's object constructor
  is always completed before it executes.  
  
  Currently we prohibit non-default ctors when instantiating an interval
  subtype.  This is unsatisfyingly restrictive.
  
  Not sure what is the best alternative. One thought is to require that the end of the
  interval's object ctor always //happens before// the end of the method or the enclosing
  inlineInterval: This works if eliminate explicit calls to {schedule()}, as the interval
  will only be scheduled when the {run()} method which created it returns. Unsatisfying
  because it distinguishes reified inlineIntervals somewhat, but perhaps that's ok.

______ Features ______________________________________________________

- {@Is} annotations on methods and variable declarations

- Requires in class bodies, declarations.

  If a class declared, for example, that two of its fields f and g have a HB relationship,
  then this would link f and g.  We can use the existing linked fields mechanisms to ensure
  that they are written together.
    
- User-specifiable primary guard

- Defaults for bound ghosts or just unbound ghosts?

- Allow user specified defaults.  

  Perhaps something like:
  {
      @Defaults({
          @Default(Foo.class, "this.some.path")
      })
      @Foo class Class {
      }      
  }
    
______ Things to Think About _________________________________________

- Improve efficiency of reaching a steady-state

  Right now we bring inner loops to a fixed state and then repeat.
  This is probably not ideal.  We could solve this either by going
  back to a CFG (not //that// tempting, I have to say) or by changing
  our iteration strategy.

- Package-Level Ghosts

  Before we found the idea of package-level regions quite useful. Are they still useful? How do
  they fit in with ghosts-as-fields? Could have a synthetic interface for every package. Each
  class implements the interface for the package in which it is declared. Seems like it could
  work.
        
- Allow multiple guards (or a more complex guard predicate of some kind)

  For example, lock AND written during.  This way, you could have shared data accessed by
  locks up to a point, where it becomes read-only without locks.

- Ensures declarations

  In place of the current inference, we should add ensures declarations on constructors
  (or potentially other methods).  These ensures declarations could be inferred by the
  compiler for constructors as we do today, but could also be manually specified to support
  virtual methods and helpers for constructors.
    
- Iterated linked fields

  Right now we screen out linked fields whose guards are known not to
  have happened --- but can we also screen out fields whose guards are
  themselves guarded by intervals that have not happened?  Maybe???
  Does that even make sense????
    
- Ordering among fields, parameters, ghosts?  

  We used to have some code that prevented the types of ghosts, fields, method
  parameters, etc from referencing later declarations.  I am not sure
  if there is any good reason for this code.  It may prevent infinite cycles 
  in the type check but I'm not sure where.
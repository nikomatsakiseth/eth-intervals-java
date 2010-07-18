package harmonic.compiler.mock

import scala.collection.mutable
import com.smallcultfollowing.lathos.Lathos
import harmonic.compiler._
import harmonic.compiler.Util._
import harmonic.compiler.Intrinsic.toHarmonicAnnotated
import harmonic.lang.StaticCheck
import java.lang.reflect
import java.lang.reflect.{Modifier => jModifier}

class MockEnv(
    recurse: inference.Recurse[Env.Xtra]
) {
    private[this] val xtra = recurse.xtra
    private[this] implicit val global = recurse.xtra.global
    private[this] val mockedObjects = new mutable.HashMap[Path, Object]()
    
    def contains(fact: inference.Fact): Boolean = {
        recurse.contains(fact)
    }
    
    def queryRGivenL[L, R](left: L, kind: Class[_ <: inference.Fact.Binary[L, R]]): Set[R] = {
        recurse.queryRGivenL(left, kind)
    }
    
    type Technique = (Path => Option[Object])
    
    private[this] def tryAlreadyMapped(path: Path) = {
        mockedObjects.get(path)
    }
    
    private[this] def tryConstant(path: Path) = {
        path match {
            case Path.Constant(obj) => Some(obj)
            case _ => None
        }
    }

    private[this] def tryBuiltin(path: Path) = {
        def isOfClass(path: Path, cls: Name.Class) = {
            contains(K.HasClass(path, cls))
        }
        
        path match {
            case _ if isOfClass(path, Name.RoIntervalClass) => Some(new MockRoInterval(path, this))
            case _ if isOfClass(path, Name.RoPointClass) => Some(new MockRoPoint(path, this))
            case _ if isOfClass(path, Name.RoLockClass) => Some(new MockRoLock(path, this))
            case _ => None
        }
    }
    
    private[this] def tryStaticField(path: Path) = {
        path match {
            case Path.Field(Path.Static, name) => {
                val csym = global.csym(name.className)
                csym.toReflectedJavaClass.flatMap { cls =>
                    val fld = cls.getDeclaredField(name.text)
                    if(fld.hasHAnnotation(classOf[StaticCheck])) {
                        try {
                            assert(fld.getModifiers.containsBits(jModifier.STATIC | jModifier.FINAL))
                            fld.setAccessible(true)
                            Some(fld.get(null))
                        } catch {
                            case exc: Exception => None
                        }                        
                    } else {
                        None
                    }
                }
            }
            
            case _ => None
        }
    }
    
    private[this] def tryStaticMethod(path: Path) = {
        path match {
            case Path.StaticCall(methodId, args) => {
                global.methodSymbol(methodId).flatMap { msym =>
                    msym.toReflectedJavaMethod.flatMap { mthd =>
                        if(mthd.hasHAnnotation(classOf[StaticCheck])) {
                            // TODO-- Infinite recursion
                            // TODO-- Check lvalues and incorporate into this test
                            val optMockedArgs = args.map(tryMock)
                            if(optMockedArgs.forall(_.isDefined)) {
                                val mockedArgs = optMockedArgs.flatten
                                
                                try {
                                    Some(mthd.invoke(null, mockedArgs: _*))
                                } catch {
                                    case exc: Exception => None
                                }                            
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                }
            }
            
            case _ => None
        }
    }
    
    // Applies `technique` to all of the objects in `paths`,
    // yielding the first successful result, or None.
    private[this] def attempt(paths: Set[Path])(technique: Technique) = {
        paths.firstSome(technique)
    }

    // List of techniques to try when mocking a path.  Assuming there is no existing
    // mapping for `path`, these techniques are attempted for all objects equatable 
    // with `path`.  We use the first succesful result.  
    //
    // TODO-- Address issues that can arise as equatable relation is not symmetrically computable
    private[this] val techniques = List[Technique](
        tryAlreadyMapped, 
        tryConstant, 
        tryStaticField, 
        tryStaticMethod,
        tryBuiltin
    )
    
    def tryMock(path: Path): Option[Object] = {
        val log = Lathos.context
        mockedObjects.get(path) match {
            case Some(obj) => Some(obj)
            
            case None => {
                log.indent("mock(", path, ")") {

                    // If path is not yet mocked, then we must find the best
                    // mock object that we can. 

                    val eqPaths = queryRGivenL(path, classOf[K.PathEq])
                    techniques.firstSome(attempt(eqPaths)) match {
                        case None => None

                        case Some(obj) => {
                            // Store in the hashtable for later, but don't overwrite existing entries.
                            for(eqPath <- eqPaths if !mockedObjects.isDefinedAt(eqPath)) {
                                mockedObjects(eqPath) = obj
                            }

                            Some(obj)
                        }
                    }
                }
            }
        }
    }
    
    // Only used if you know that mock must succeed.
    //
    // For example, a path like foo.(Interval#getStart())
    // will always yield SOME mocked interval.
    def mock(path: Path) = tryMock(path).get
}
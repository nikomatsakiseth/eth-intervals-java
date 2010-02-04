package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable.HashMap

import javax.lang.model.element.Element
import javax.lang.model.element.{ElementKind => EK}
import javax.lang.model.element.PackageElement
import javax.lang.model.element.TypeElement

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition

object Util {
    
    class WithPositional[P <: Positional](instance: P) {
        def hasPos = instance.pos != NoPosition
        
        def withPos(pos: Position): P = {
            instance.setPos(pos)
            instance
        }
    }
    
    implicit def positional2WithPositional[P <: Positional](instance: P) =
        new WithPositional[P](instance)
        
    def withPos[P <: Positional](p: Position, instance: P) = 
        instance.withPos(p)

    // ____________________________________________________________
    // Extensions to String
    
    case class UtilString(s: String) {
        def join(itemPrefix: String, iterable: Iterable[Any], itemSuffix: String): String =
            join(itemPrefix, iterable.elements, itemSuffix)
        def join(itemPrefix: String, elements: Iterator[Any], itemSuffix: String): String = {
            if(!elements.hasNext)
                ""
            else {
                val sb = new StringBuffer            
                sb.append(itemPrefix).append(elements.next).append(itemSuffix)
                while(elements.hasNext) {
                    sb.append(s)
                    sb.append(itemPrefix).append(elements.next).append(itemSuffix)
                }
                sb.toString
            }            
        }
        def join(itemPrefix: String, iterable: Iterable[Any]): String =
            join(itemPrefix, iterable, "")
        def join(iterable: Iterable[Any]): String =
            join("", iterable, "")
        def join(elements: Iterator[Any]): String =
            join("", elements, "")
    }
    
    implicit def string2UtilString(s: String) = UtilString(s)
    
    // ____________________________________________________________
    // Extensions to List
    
    case class UtilPairList[Q,R](i: List[(Q,R)]) {
        def map_1[S](f: (Q => S)) = i.map { case (q, r) => (f(q), r) }
        def map_2[S](f: (R => S)) = i.map { case (q, r) => (q, f(r)) }
        def _1 = i.map(_._1)
        def _2 = i.map(_._2)
    }
    implicit def list2UtilPairList[Q,R](i: List[(Q,R)]) = UtilPairList(i)
    
    case class UtilPairSeq[Q,R](i: Seq[(Q,R)]) {
        def _1 = i.map(_._1)
        def _2 = i.map(_._2)
    }
    implicit def list2UtilPairSeq[Q,R](i: Seq[(Q,R)]) = UtilPairSeq(i)
    
    // ____________________________________________________________
    // Extensions to List
    
    case class UtilList[E](l: List[E]) {
        def foldRightOpt[V](v: V)(f: ((E,V) => Option[V])) =
            l.foldRight[Option[V]](Some(v)) {
                case (_, None) => None
                case (e, Some(v0)) => f(e, v0)
            }
            
        // Applies fn to the members of this list in turn,
        // returning the first Some(_) that results (or None
        // if no Some(_) ever results)
        def firstSomeReturned[F](fn: Function[E, Option[F]]): Option[F] = {
            l match {
                case List() => None
                case hd :: tl =>
                    val r = fn(hd)
                    r match {
                        case Some(f) => r
                        case None => UtilList(tl).firstSomeReturned(fn)
                    }
            }
        }
        
        def endsWith(m: List[E]) =
                l.takeRight(m.length) == m
        
        def containsAll(m: Iterable[E]) =
                m.forall(l.contains)
    }
    implicit def list2UtilList[Q](i: List[Q]) = UtilList(i)
    
    // ____________________________________________________________
    // Extensions to Option
    
    case class UtilOption[X](o: Option[X]) {
        def mapToOption[Y](f: (X => Option[Y])) = o match {
            case None => None
            case Some(x) => f(x)
        }
        
        def foldLeft[Y](y0: Y)(f: ((Y, X) => Y)): Y =
            o.map(f(y0, _)).getOrElse(y0)
    }
    implicit def option2UtilOption[X](o: Option[X]) = UtilOption(o)
    
    // ____________________________________________________________
    // Extensions to Iterator and Iterable
    
    case class UtilIterator[Q](i: Iterator[Q]) {
        def makeMap[K,V](f: Q => (K,V)): Map[K,V] =
            i.foldLeft(Map.empty[K,V]) { case (m, q) => m + f(q) }

        def makeFlatMap[K,V](f: Q => Iterable[(K,V)]): Map[K,V] =
            i.foldLeft(Map.empty[K,V]) { case (m, q) => f(q).foldLeft(m)(_ + _) }
            
        def ->*(v: Iterable[Q]): (Q => Set[Q]) = // k ->* v makes a multi-substitution
            (MultiMap.empty[Q,Q] ++ i.zip(v.elements)).toMap.withDefault(k => Set(k))

        def mapTo[V](v: Iterator[V]): Map[Q,V] = // k.mapTo(v) makes a map from k to v 
            Map.empty ++ i.zip(v)
        
        def mapTo[V](v: Seq[V]): Map[Q,V] =
            Map.empty ++ i.zip(v.elements)
            
        def subsetOf(c: UtilIterator[Q]) =
            i.forall(c.i contains _)
            
        def intersects(c: UtilIterator[Q]) =
            i.exists(c.i contains _)
    }
    implicit def iterable2UtilIterator[Q](i: Iterable[Q]) = UtilIterator(i.elements)
    implicit def iterator2UtilIterator[Q](i: Iterator[Q]) = UtilIterator(i)
    
    case class UtilIterable[I](is: Iterable[I]) {
        def cross[J](js: UtilIterable[J]): Iterable.Projection[(I,J)] = 
            for(i <- is.projection; j <- js.is.projection) yield (i,j)
            
        def mkCommaString = is.mkString(", ")
            
        def mkEnglishString: String = {
                val list = is.toList
                val len = list.length
                if(len == 0) ""
                else if(len == 1) list.head.toString
                else if(len == 2) list.head + " and " + list.last
                else {
                        val (l, r) = list.splitAt(len - 1)
                        l.mkString(", ") + ", and " + r.head
                }
        }
    }
    implicit def iterable2UtilIterable[I](i: Iterable[I]) = UtilIterable(i)
    
    // cross(List()) => List()
    // cross(List(List(1,2,3))) => List(List(1), List(2), List(3))
    // cross(List(List(1,2,3), List(4))) => List(List(1, 4), List(2, 4), List(3, 4))
    def cross[I](is: List[Iterable[I]]): List[List[I]] = is match {
        case List() => List()
        case List(i) => i.map(List(_)).toList
        case i :: is_tl => cross(is_tl).flatMap(l => i.map(_ :: l))
    }
    
    // pairs(List()) = List()
    // pairs(List(1)) = List()
    // pairs(List(1, 2)) = List((1, 2))
    // pairs(List(1, 2, 3)) = List((1, 2), (1, 3), (2, 3))
    def pairs[I](lst: List[I]): List[(I, I)] = lst match {
        case List() | List(_) => List()
        case List(i, j) => List((i, j))
        case hd :: tl => tl.foldRight(pairs(tl)) { case (i, l) => (hd, i) :: l }
    }
    
    def firstlast[A](l: List[A]) = (l.first, l.last)
    
    // ____________________________________________________________
    // Extensions to and utilities for Map: We do a lot of mapping, after all.
    
    def makeMap[K,V](pairs: Iterable[(K,V)]): Map[K,V] = 
        Map.empty ++ pairs
        
    case class UtilMap[K,V](m0: Map[K, V]) {
        def mapValues[W](f: (V => W)): Map[K, W] =
            m0.transform { case (k, v) => f(v) }
        
        // Intersection of two maps (m, m1) is a map containing (k→v) only if 
        // (k→v)∈m & (k→v)∈m1.
        def **(m1: Map[K, V]) = {
            m0.elements.foldLeft(m0.empty[V]) { case (m, (k, v)) =>
                m1.get(k) match {
                    case Some(v1) if v == v1 => m + Pair(k, v)
                    case _ => m
                }
            }
        }
    }    
    implicit def map2UtilMap[K, V](m0: Map[K, V]): UtilMap[K, V] = UtilMap(m0)
    
    // ______________________________________________________________________
    // A subst is a map from T->T, where the default action is identity.

    type Subst[T] = Map[T,T]

    def emptySubst[A]: Subst[A] = Map.empty.withDefault(p => p)

    def makeSubst[A](pairs: Seq[(A,A)]): Subst[A] = 
        emptySubst ++ pairs

    def subst[A](key: A, value: A): Map[A,A] = 
        emptySubst + Pair(key, value)

    def nameMap[N, I <: { def name: N }](l: List[I]): Map[N, I] =
        makeMap(l.map(i => (i.name, i)))

    def longest[A](ls: Collection[List[A]]): List[A] =
        ls.foldLeft(List[A]())((r, l) => if(r.length > l.length) r else l)
        
        
    abstract class BaseMultiMap[K, V, S <: BaseMultiMap[K, V, S]](val size: Int, _m: Map[K, Set[V]])
    extends Function1[K, Set[V]] with Collection[(K, V)]
    {
        this: S =>
        
        protected def newMultiMap(size: Int, _m: Map[K, Set[V]]): S      
        protected def defaultValue(k: K): Set[V]

        def toMap = _m
        def keys = _m.keys      
        def keySet = _m.keySet
        
        def get(k: K) = _m.get(k)
        
        def apply(k: K) = get(k) match {
            case None => defaultValue(k)
            case Some(set) => set
        }
            
        def inv(v: V) = _m.keys.filter(k => contains(k, v))
            
        def contains(k: K, v: V) = this(k).contains(v)
        
        def mapKey(k: K, f: (Set[V] => Set[V])) = {
            val kset0 = this(k)
            val kset1 = f(kset0)
            newMultiMap(
                size - kset0.size + kset1.size,
                _m + Pair(k, kset1)
            )
        }
                            
        def ++*(pairs: Iterable[(K, Iterable[V])]) =
            pairs.foldLeft(this) { case (m, (k, v)) => m.plusMany(k, v) }
        def --*(pairs: Iterable[(K, Iterable[V])]) =
            pairs.foldLeft(this) { case (m, (k, v)) => m.subMany(k, v) }

        def plusMany(k: K, v: Iterable[V]) = mapKey(k, _ ++ v)
        def subMany(k: K, v: Iterable[V]) = mapKey(k, _ -- v)
        
        def +(pair: (K, V)): S = {
            val (k, v) = pair
            if(!contains(k, v))
                newMultiMap(size + 1, _m + Pair(k, this(k) + v))
            else
                this
        }

        def ++(pairs: Collection[(K,V)]): S =
            pairs.foldLeft(this)(_ + _)

        def ++(pairs: Iterator[(K,V)]): S =
            pairs.foldLeft(this)(_ + _)

        def -(k: K): S = {
            get(k) match {
                case None => this
                case Some(set) if set.isEmpty => this
                case Some(set) => newMultiMap(size - set.size, _m - k)
            }
        }

        def --(ks: Iterable[K]): S =
            ks.foldLeft(this)(_ - _)

        def -(pair: (K, V)): S = {
            val (k, v) = pair
            if(contains(k, v))
                newMultiMap(size - 1, _m + Pair(k, this(k) - v))
            else
                this
        }

        def --(pairs: Collection[(K, V)]) = 
            pairs.foldLeft(this)(_ - _)
            
        def filterKeys(f: (K => Boolean)) = {
            val m1 = _m.filter { case (k, v) => f(k) }
            val size = m1.values.map(_.size).foldLeft(0)(_ + _) // faster way?
            newMultiMap(size, m1)
        }
            
        def mapValueSets(f: ((K, Set[V]) => Set[V])) = {
            val m1 = _m.transform((k,v) => f(k, v))
            val size = m1.values.map(_.size).foldLeft(0)(_ + _) // faster way?
            newMultiMap(size, m1)            
        }
            
        def filterValueSetElems(f: (V => Boolean)) =
            mapValueSets((k, v) => v.filter(f))
        
        private def iterator =
            _m.elements.flatMap { case (k, v) => v.elements.map(e => (k, e)) }
            
        def elements: Iterator[(K, V)] = iterator
            
        def toSet: Set[(K, V)] = {
            val mm = this
            new Set[(K,V)] {
                def size = mm.size
                def elements = mm.iterator
                def empty[K] = Set.empty[K]
                def contains(pair: (K, V)) = mm(pair._1)(pair._2)
                def +(pair: (K, V)) = Set.empty ++ mm.iterator + pair
                def -(pair: (K, V)) = Set.empty ++ mm.iterator - pair
            }
        }
        
        override def toString =
            (for(k <- keys) yield k + " => " + this(k)).mkString("/")
    }
    
    class MultiMap[K,V](size: Int, _m: Map[K, Set[V]])
    extends BaseMultiMap[K,V,MultiMap[K,V]](size, _m) {
        override protected def newMultiMap(size: Int, _m: Map[K, Set[V]]) =
            new MultiMap[K, V](size, _m)
        override protected def defaultValue(k: K) =
            Set.empty
    }
    
    object MultiMap {
        def empty[K, V] = 
            new MultiMap[K, V](0, Map.empty)
    }

    class EquivMap[K](size: Int, _m: Map[K, Set[K]])
    extends BaseMultiMap[K,K,EquivMap[K]](size, _m) {
        override protected def newMultiMap(size: Int, _m: Map[K, Set[K]]) =
            new EquivMap[K](size, _m)
        override protected def defaultValue(k: K) =
            Set(k)
    }
    
    object EquivMap {
        def empty[K] = 
            new EquivMap[K](0, Map.empty)
    }

    // Returns transitive closure of 'in'
    def transitiveClosure[V](in: MultiMap[V, V]): MultiMap[V, V] = {
        var out = in
        for(from <- in.keySet) {
            val step = in(from).flatMap(in) // x -> p -> step
            out = out.plusMany(from, step)
        }
        if(out.size == in.size) in
        else transitiveClosure(out)
    }

    // ______________________________________________________________________
    
    def forallzip[A,B](as: List[A], bs: List[B])(f: Function2[A, B, Boolean]) =
        as.zip(bs).forall(p => f(p._1, p._2))
    def forallcross[A,B](as: Iterable[A], bs: Iterable[B])(f: Function2[A, B, Boolean]) =
        as.forall(a => bs.forall(b => f(a,b)))
    def existscross[A,B](as: Iterable[A], bs: Iterable[B])(f: Function2[A, B, Boolean]) =
        as.exists(a => bs.exists(b => f(a,b)))
    def foreachzip[A,B](as: List[A], bs: List[B])(f: Function2[A, B, Unit]) =
        as.zip(bs).foreach(p => f(p._1, p._2))
    def foreachcross[A,B](as: List[A], bs: List[B])(f: Function2[A, B, Unit]) =
        as.foreach(a => bs.foreach(b => f(a, b)))
        
    // ______________________________________________________________________

    def intersect[A](sets: Iterable[Set[A]]): Set[A] = {
        val iter = sets.elements
        if(iter.hasNext) {
            val first = iter.next
            iter.foldLeft(first)(_ ** _)
        } else {
            Set.empty
        }
    }
    
    def not(b: Boolean) = !b

    def nullopt[A <: AnyRef](a: A) =
        if(a == null) None
        else Some(a)
        
    def memoize[P,Q](f: (P => Q)) = {
        val tab = new HashMap[P,Q]()
        def g(p: P) = {
            tab.get(p) match {
                case Some(q) => q
                case None =>
                    val q = f(p)
                    tab(p) = q
                    q
            }
        }
        g _
    }
  
    // ____________________________________________________________
    
    def qualNameSb(sb: StringBuilder, elem: Element) {
        if(elem == null)
            sb.append("null")
        else
            elem.getKind match {
                case EK.PACKAGE =>          
                    val pkgElem = elem.asInstanceOf[PackageElement]
                    sb.append(pkgElem.getQualifiedName)
                case _ =>
                    qualNameSb(sb, elem.getEnclosingElement)
                    sb.append(".")
                    sb.append(elem.getSimpleName.toString)
                    if(elem.getKind == EK.METHOD)
                        sb.append("()")
            }
    }
    
    def qualName(elem: Element): String = {
        val sb = new StringBuilder()
        qualNameSb(sb, elem)
        sb.toString
    }    
    
    def prefix(s: String) = 
        if(s.length < 30)
            s
        else
            s.substring(0, 30)
            
    def nullToOption[A <: AnyRef](a: A) =
        if(a == null) None
        else Some(a)

    // ___ JCL Conversions __________________________________________________
    
    class JavaIterator[E](i: Iterator[E]) extends java.util.Iterator[E] {
        def hasNext = i.hasNext
        def next = i.next
        def remove = throw new UnsupportedOperationException()
    }
    
    class JavaIterable[E](i: Iterable[E]) extends java.lang.Iterable[E] {
        def iterator = new JavaIterator(i.elements)
    }
    
    class ScalaIterator[E](i: java.util.Iterator[E]) extends Iterator[E] {
        def hasNext = i.hasNext
        def next = i.next
    }
    
    class ScalaIterable[E](i: java.lang.Iterable[E]) extends Iterable[E] {
        def elements = new ScalaIterator(i.iterator)
    }
    
    implicit def scalaToJava[E](i: Iterable[E]): java.lang.Iterable[E] = new JavaIterable(i)
    implicit def javaToScala[E](i: java.lang.Iterable[E]): Iterable[E] = new ScalaIterable(i)
            
}

package ch.ethz.intervals

import scala.collection.IterableView
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable.HashMap

import com.sun.source.tree.Tree

import javax.lang.model.element.Element
import javax.lang.model.element.{ElementKind => EK}
import javax.lang.model.element.PackageElement
import javax.lang.model.element.TypeElement
import javax.lang.model.`type`.{TypeKind => TK}

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

    // Used to attach Javac trees and elements as positions:
    abstract class DummyPosition extends Position {
        def column = System.identityHashCode(reportObject) // just return something unique-ish
        def line = 1
        def lineContents = "dummy"        
        
        def rewrite(s: String): String
        
        def reportObject: Object
    }
    
    class DummyPositional(dpos: DummyPosition, tag: String) extends Positional {
        setPos(dpos)
        override def toString = "%s(%s)".format(tag, dpos)
    }
    
    // ____________________________________________________________
    // Extensions to String
    
    case class UtilString(s: String) {
        def join(itemPrefix: String, iterable: Iterable[Any], itemSuffix: String): String =
            join(itemPrefix, iterable.iterator, itemSuffix)
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
            
        // Simple "glob" style patterns: * == .*, everything else is normal.
        def glob(str: String) = {
            val replacements = List(
                ("""[?+.$\(\)\[\]\\\^]""" -> """\\$0"""),
                ("""\*""" -> """.*""")
            )
            val pat = replacements.foldLeft(s)((p,r) => p.replaceAll(r._1, r._2))
            java.util.regex.Pattern.matches("^%s$".format(pat.trim), str)
        }
    }
    
    implicit def string2UtilString(s: String) = UtilString(s)
    
    // ____________________________________________________________
    // Extensions to List
    
    case class UtilList[E](l: List[E]) {
        def foldRightOpt[V](v: V)(f: ((E,V) => Option[V])) =
            l.foldRight[Option[V]](Some(v)) {
                case (_, None) => None
                case (e, Some(v0)) => f(e, v0)
            }
            
        def endsWith(m: List[E]) =
                l.takeRight(m.length) == m
        
        def containsAll(m: Iterable[E]) =
                m.forall(l.contains)
                
        def dedup = {
            def helper(s: Set[E], lst: List[E]): List[E] = lst match {
                case Nil => Nil
                case hd :: tl if s(hd) => helper(s, tl)
                case hd :: tl => hd :: helper(s + hd, tl)
            }
            helper(Set(), l)
        }
    }
    implicit def toUtilList[Q](i: List[Q]) = UtilList(i)
    
    // ____________________________________________________________
    // Extensions to Iterator and Iterable
    
    case class UtilIterator[Q](i: Iterator[Q]) {
        def makeMap[K,V](f: Q => (K,V)): Map[K,V] =
            i.foldLeft(Map.empty[K,V]) { case (m, q) => m + f(q) }

        def makeFlatMap[K,V](f: Q => Iterable[(K,V)]): Map[K,V] =
            i.foldLeft(Map.empty[K,V]) { case (m, q) => f(q).foldLeft(m)(_ + _) }
            
        def mapTo[V](v: Iterator[V]): Map[Q,V] = // k.mapTo(v) makes a map from k to v 
            Map.empty ++ i.zip(v)
        
        def mapTo[V](v: Seq[V]): Map[Q,V] =
            Map.empty ++ i.zip(v.iterator)
            
        def subsetOf(c: UtilIterator[Q]) =
            i.forall(c.i contains _)
    }
    implicit def iterable2UtilIterator[Q](i: Iterable[Q]) = UtilIterator(i.iterator)
    implicit def iterator2UtilIterator[Q](i: Iterator[Q]) = UtilIterator(i)
    
    case class UtilIterable[I](is: Iterable[I]) {
        def cross[J](js: UtilIterable[J]) = 
            for(i <- is.view; j <- js.is.view) yield (i,j)
            
        def mkCommaString = is.mkString(", ")
        
        // Applies fn to the members of this list in turn,
        // returning the first Some(_) that results (or None
        // if no Some(_) ever results)
        def firstSomeReturned[F](fn: (I => Option[F])): Option[F] = {
            is.foldLeft[Option[F]](None) { 
                case (None, i) => fn(i)
                case (Some(f), _) => Some(f)
            }
        }
        
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
    
    // ____________________________________________________________
    // Extensions for lists and iterables of pairs
    
    case class UtilPairIterable[E,F](iterable: Iterable[(E,F)]) {
        def foreachPair(func: ((E,F) => Unit)) =
            iterable.foreach(pair => func(pair._1, pair._2))
            
        def forallPairs(func: ((E,F) => Boolean)) =
            iterable.forall(pair => func(pair._1, pair._2))
            
        def existsPair(func: ((E,F) => Boolean)) =
            iterable.exists(pair => func(pair._1, pair._2))
            
        def mapPair[G](func: ((E,F) => G)) =
            iterable.map(pair => func(pair._1, pair._2))
            
        def filterPairs(func: ((E,F) => Boolean)) =
            iterable.filter(pair => func(pair._1, pair._2))
    }
    implicit def toUtilPairIterable[E,F](i: Iterable[(E,F)]) = UtilPairIterable(i)
    
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
    
    def firstlast[A](l: List[A]) = (l.head, l.last)
    
    // ____________________________________________________________
    // Extensions to Set
    
    case class UtilSet[E](set: Set[E]) {
        def intersects(anotherSet: Set[E]) = {
            set.exists(anotherSet)
        }
    }
    implicit def set2UtilSet[E](set: Set[E]) = UtilSet(set)
    
    // ____________________________________________________________
    // Extensions to and utilities for Map: We do a lot of mapping, after all.
    
    def makeMap[K,V](pairs: Iterable[(K,V)]): Map[K,V] = 
        Map.empty ++ pairs
        
    case class UtilMap[K,V](m0: Map[K, V]) {
        def mapValues[W](f: (V => W)): Map[K, W] =
            m0.transform { case (k, v) => f(v) }
        
        // Intersection of two maps (m, m1) is a map containing (k→v) only if 
        // (k→v)∈m & (k→v)∈m1.
        def &(m1: Map[K, V]) = {
            m0.iterator.foldLeft(m0.empty) { case (m, (k, v)) =>
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

    def computeTransitiveClosure[X](func: (X => Set[X]), initial: Set[X]) = {
        def iterate(stale: Set[X], fresh: Set[X]): Set[X] = {
            if(fresh.isEmpty) stale
            else {
                val nextStale = stale ++ fresh
                val nextFresh = fresh.flatMap(func).filter(cp => !nextStale(cp))
                iterate(nextStale, nextFresh)
            }            
        }
        iterate(Set(), initial)
    }
    
    def intersect[A](sets: Iterable[Set[A]]): Set[A] = {
        val iter = sets.iterator
        if(iter.hasNext) {
            val first = iter.next
            iter.foldLeft(first)(_ & _)
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
            
    def treeToString(tree: Tree) = {
        "%s[%s]".format(tree.getKind, prefix(tree.toString))
    }
            
    // ___ JCL Conversions __________________________________________________
    
    //class JavaIterator[E](i: Iterator[E]) extends java.util.Iterator[E] {
    //    def hasNext = i.hasNext
    //    def next = i.next
    //    def remove = throw new UnsupportedOperationException()
    //}
    //
    //class JavaIterable[E](i: Iterable[E]) extends java.lang.Iterable[E] {
    //    def iterator = new JavaIterator(i.iterator)
    //}
    //
    //class ScalaIterator[E](i: java.util.Iterator[E]) extends Iterator[E] {
    //    def hasNext = i.hasNext
    //    def next = i.next
    //}
    //
    //class ScalaIterable[E](i: java.lang.Iterable[E]) extends Iterable[E] {
    //    def iterator = new ScalaIterator(i.iterator)
    //}
    //
    //implicit def scalaToJava[E](i: Iterable[E]): java.lang.Iterable[E] = new JavaIterable(i)
    //implicit def javaToScala[E](i: java.lang.Iterable[E]): Iterable[E] = new ScalaIterable(i)
            
}

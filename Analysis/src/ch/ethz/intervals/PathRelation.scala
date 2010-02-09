package ch.ethz.intervals

import scala.Collection

case class PathRelation(
    // True if this relation is transitive.
    transitive: Boolean,
    
    // The set of conditional relations.  That is,
    // an entry (a R b) in 'mmap' indicates that
    // (a ≠ null ⇒ b ≠ null ⇒ a R b).
    mmap: MultiMap[ir.Path, ir.Path]
) {
    
    private var cache: Option[(AnyRef, MultiMap[ir.Path, ir.Path])] = None
    def pairs(nonnull: (ir.Path => Boolean)) = {
        if(!transitive) {
            mmap
        } else {
            def tc(m0: MultiMap[ir.Path, ir.Path]): MultiMap[ir.Path, ir.Path] = {
                val m1 = m0.keys.foldLeft(m0) { case (m, p_from) =>
                    val qs_immed = m0.values(p_from).filter(nonnull) // p_from -> qs_immed [non-null] 
                    val qs_trans = qs_immed.flatMap(m0.values) // p_from -> qs_immed -> qs_trans
                    m.plusMany(p_from, qs_trans)
                }
                if(m0.size == m1.size) m0
                else tc(m1)
            }
            val m = cache match {
                case Some((nn, m)) if nn eq nonnull => m
                case _ => tc(mmap)
            }
            cache = Some((nonnull, m))
            m
        }
    }
    
    def +(pair: (ir.Path, ir.Path)) = {
        new PathRelation(transitive, mmap + pair)
    }
    
    def ++(rel: PathRelation) = {
        new PathRelation(transitive, mmap.plusMap(rel.mmap))
    }
    
    def values(nonnull: (ir.Path => Boolean))(p: ir.Path) = {
        pairs(nonnull).values(p)
    }
    
    def intersect(nonnull1: (ir.Path => Boolean))(nonnull2: (ir.Path => Boolean), rel2: PathRelation) = {
        val ap1 = pairs(nonnull1)
        val ap2 = pairs(nonnull2)
        new PathRelation(transitive, ap1.filter(ap2))
    }
    
    def mapFilter(
        nonnull: (ir.Path => Boolean)
    )(
        mfunc: (ir.Path => ir.Path),
        ffunc: (ir.Path => Boolean)
    ) = {
        val ap = pairs(nonnull)
        new PathRelation(
            transitive,
            ap.keys.foldLeft(MultiMap.empty[ir.Path, ir.Path]) { case (m, p) =>
                val p_map = mfunc(p)
                if(!ffunc(p_map)) {
                    m
                } else {
                    val qs = ap.values(p)
                    qs.elements.map(mfunc).filter(ffunc).foldLeft(m) { case (m, q_map) => 
                        m + Pair(p_map, q_map)
                    }
                }
            }
        )
    }
    
}

object PathRelation {
    val transitive = new PathRelation(true, MultiMap.empty)
    val intransitive = new PathRelation(false, MultiMap.empty)
}
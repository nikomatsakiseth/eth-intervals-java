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
    
    lazy val pairs = {
        if(!transitive) {
            mmap
        } else {
            def tc(m0: MultiMap[ir.Path, ir.Path]): MultiMap[ir.Path, ir.Path] = {
                val m1 = m0.keysIterator.foldLeft(m0) { case (m, p_from) =>
                    val qs_immed = m0.values(p_from) // p_from -> qs_immed
                    val qs_trans = qs_immed.flatMap(m0.values) // p_from -> qs_immed -> qs_trans
                    m.plusMany(p_from, qs_trans)
                }
                if(m0.size == m1.size) m0
                else tc(m1)
            }
            tc(mmap)
        }
    }
    
    def +(pair: (ir.Path, ir.Path)) = {
        new PathRelation(transitive, mmap + pair)
    }
    
    def ++(rel: PathRelation) = {
        new PathRelation(transitive, mmap.plusMap(rel.mmap))
    }
    
    def values(p: ir.Path) = {
        pairs.values(p)
    }
    
    def &(rel2: PathRelation) = {
        new PathRelation(transitive, pairs.filter(rel2.pairs))
    }
    
    def mapFilter(
        mfunc: (ir.Path => ir.Path),
        ffunc: (ir.Path => Boolean)
    ) = {
        new PathRelation(
            transitive,
            pairs.keysIterator.foldLeft(MultiMap.empty[ir.Path, ir.Path]) { case (m, p) =>
                val p_map = mfunc(p)
                if(!ffunc(p_map)) {
                    m
                } else {
                    val qs = pairs.values(p)
                    qs.iterator.map(mfunc).filter(ffunc).foldLeft(m) { case (m, q_map) => 
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
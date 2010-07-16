package harmonic.compiler.mock

import ch.ethz.intervals._
import harmonic.compiler._

class MockRoPoint(
    val path: Path.Ref,
    menv: MockEnv
) extends MockObject with RoPoint {
    
    override def getBound: RoPoint = null //bnd
    
    override lazy val getBounds: Array[RoPoint] = {
        if(getBound == null) Array(this)
        else getBound.getBounds :+ this
    }
    
    override def mutualBound(pnt: RoPoint): RoPoint = {
        if(getBounds.contains(pnt)) pnt
        else if(pnt.getBounds.contains(this)) this
        else getBounds.reverseIterator.find(pnt.isBoundedBy(_)).getOrElse(null)
    }
    
    override def isBoundedBy(pnt: RoPoint): Boolean = {
        (this != pnt) && getBounds.contains(pnt)
    }
    
    override def hb(pnt: RoPoint) = {
        pnt match {
            case pnt: MockRoPoint =>
                menv.contains(K.Hb(path, pnt.path))
            case _ => 
                false
        }
    }

    override def hbeq(pnt: RoPoint) = {
        (this == pnt) || hb(pnt)
    }
    
}
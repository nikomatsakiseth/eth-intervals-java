package harmonic.compiler

/** Lists the pre-defined intervals contained in 
  * the intervals array for a `ClassFromSource`. 
  * The members are created in `Global.createSymbols()` */
object Pass {
    private[this] var counter = 0
    private[this] def next = {
        val result = counter
        counter += 1
        result
    }
    
    val Header   = next
    val Body     = next
    val Lower    = next
    val Create   = next
    val Members  = next
    val Merge    = next
    val Gather   = next
    val ByteCode = next
}
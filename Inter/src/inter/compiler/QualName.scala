package inter.compiler

case class QualName(
    rev_components: List[String]
) {
    def components = rev_components.reverse
    def asRelPath = components.mkString("/")
    override def toString = components.mkString(".")
    
    def /(nm: String) = QualName(nm :: rev_components)
}
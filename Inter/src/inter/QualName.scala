package inter

case class QualName(
    rev_components: List[String]
) {
    def components = rev_components.reverse
    override def toString = components.mkString(".")
    
    def /(nm: String) = QualName(nm :: rev_components)
}
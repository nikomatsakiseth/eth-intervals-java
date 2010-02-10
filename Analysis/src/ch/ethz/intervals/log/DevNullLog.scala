package ch.ethz.intervals.log

object DevNullLog extends Log {
    def uri = ""
    def rawStart(open: Boolean, html: String) = ""
    def rawClose() { }
    def rawLinkTo(uri: String, html: String) { }
    def escape(s: String) = s
    def ifEnabled(f: => Unit): Unit = ()
    def log(name: String) = this
    def splitLog(name: String) = new SplitLog("", this, this)
}
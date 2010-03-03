package ch.ethz.intervals.log

object DevNullLog extends Log {
    def uri = ""
    def rawStart(open: Boolean, html: String) = ""
    def rawClose() { }
    def rawLinkTo(uri: String, html: String) { }
    def escape(s: String) = s
    def ifEnabled(f: => Unit): Unit = ()
    def inlineLog = this
    def splitLog(name: String) = SplitLog.devNullSplitLog
    def flush = ()
}
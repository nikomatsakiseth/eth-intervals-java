package ch.ethz.intervals.log

import java.io.File

class HtmlLog(
    logDirectory: LogDirectory, 
    val outFile: File,
    detailsLog: Option[Log]
) extends Log {
    def uri = outFile.toURI.toString
    
    val backgroundColors = List(
        "CCCCCC", // Gray
        "CC99CC", // Purple
        "BBBBBB", // Gray
        "CC9966", // Tan
        "AAAAAA", // Gray
        "88AAFF", // Blue
        "99FFCC", // Aquamarine
        "CC9999"  // Pink
    )
    var currentColor = -1
    
    def nextColor = {
        currentColor = (currentColor + 1) % backgroundColors.length
        backgroundColors(currentColor)
    }
    
    val outWriter = {
        val pw = new java.io.PrintWriter(outFile)
        
        pw.print("""
        <HTML>
        <HEAD>
            <TITLE>Debug log</TITLE>
            <SCRIPT type='text/javascript'>
                function preview(obj) {
                	if (obj.innerHTML)
                		closePreview(obj);

                	if (obj.href)
                		var src = obj.href;
                	else
                		return false;

                	var iframe = document.createElement("iframe");
                	iframe.src = src;
                	iframe.id = "iframe_" + obj.innerHTML;
                	iframe.style.height = "200px";
                	iframe.style.width = "400px";
                	iframe.style.whiteSpace = "nowrap";
                	iframe.style.position = "absolute";

                	// Next line takes the number value of link.style.bottom (decimals included) and converts it to a number
                	var botPad = /^(\d*[^(px)])*/.exec(obj.style.bottom)[0] / 1;
                	botPad += 20; // Amount of px above link
                	iframe.style.bottom = botPad + "px";
                	document.body.insertBefore(iframe, obj);
                }
                function closePreview(obj) {
                	if (obj.innerHTML)
                		var id = "iframe_" + obj.innerHTML;
                	else
                		return false;

                	if (document.getElementById(id)) {
                		var iframe = document.getElementById(id);
                		iframe.parentNode.removeChild(iframe);
                	}
                }            
                function toggleId(id)
                {
                    var target = document.getElementById(id);
                    var kids = target.childNodes;
                    var openedKids = false;
                    var closedKids = false;
                    for(var i = 0; (i < kids.length); i++) {
                        var kid = kids[i];
                        if(
                            kid.className == 'log initiallyOpen' ||
                            kid.className == 'log initiallyClosed'
                        ) {
                            if(kid.style.display == 'none') {
                                kid.style.display = 'block';
                                openedKids = true;
                            } else {
                                kid.style.display = 'none';
                                closedKids = true;
                            }
                        }
                    }
                    
                    if(openedKids) {
                        target.style.opacity = 1.0;
                    } else if (closedKids) {
                        target.style.opacity = 0.25;                            
                    }
                }
            </SCRIPT>
            <STYLE>
                DIV.log {
                    border-width: thin;
                    border-style: solid;
                    margin-top: .1cm;
                    margin-bottom: .1cm;
                    margin-left: .3cm;                    
                }
                .initiallyOpen {
                    opacity: 1.0;                    
                }
                .initiallyClosed {
                    opacity: 0.25;
                }
                A:hover {
                    text-decoration: underline;
                }
                A:link {
                    text-decoration: none;
                }
                A:visited {
                    text-decoration: none;
                }
            </STYLE>
        </HEAD>
        <BODY>
        <DIV id='id0'>            
        """)
        
        pw
    }
    
    private var ids = 0
    private var idStack = List(("id0", true))
            
    private def pushId(open: Boolean) {
        ids = ids + 1
        val id = ("id" + ids, open)
        idStack = id :: idStack            
    }
    
    private def popId() {
        idStack = idStack.tail
    }
    
    private def openDiv(open: Boolean, msg: String) = {
        pushId(open)            
        val (parentId, parentOpen) = idStack.tail.head
        val (id, _) = idStack.head
        val cls = 
            if(open) "log initiallyOpen" 
            else "log initiallyClosed"
        val display = 
            if(parentOpen) "block"
            else "none"
        
        outWriter.println(
            (
                "<DIV id='%s' class='%s' style='background-color: #%s; display: %s;'>"+
                "<A href='#%s'>&#8689;</A>&nbsp;"+
                //"<A href='#%s' class='collapse' onclick='toggleId(\"%s\")'>%s</A>"
                "<SPAN class='msg' onclick='toggleId(\"%s\")'>%s</SPAN>"
            ).format(
                id, cls, nextColor, display,
                parentId,
                id, msg
            ))
            
        id
    }
    
    private def closeDiv = {
        popId()
        outWriter.println("</DIV>")        
    }
    
    private def writeLink(uri: String, target: String) = {
        outWriter.println("<A href='%s' target='%s'>&rarr;</A>".format(uri, target))            
    }
    
    override def rawStart(open: Boolean, msg: String) = {
        val id = openDiv(open, msg)
        detailsLog.foreach { l =>
            val linkId = l.rawStart(open, "<I>%s</I>".format(msg))
            writeLink(l.uri + "#" + linkId, "details")
        }
        outWriter.flush
        id
    }
    
    def rawClose {
        closeDiv
        detailsLog.foreach { l =>
            l.rawClose()
        }
        outWriter.flush            
    }
    
    override def rawLinkTo(uri: String, msg: String) {
        val id = openDiv(true, msg)
        writeLink(uri, "_top")
        closeDiv
        outWriter.flush            
    }
    
    def escape(s0: String) = HtmlLog.escape(s0)

    def ifEnabled(f: => Unit): Unit = f
    
    def inlineLog = {
        val f = logDirectory.newFile(".html")
        new HtmlLog(logDirectory, f, None)
    }
    
    def splitLog(name: String) = {
        val sl = SplitLog.newFrameset(logDirectory, System.nanoTime.toString)
        linkTo(sl.uri, name)
        sl
    }
}

object HtmlLog {
    def escape(s0: String) = {
        var s = s0
        s = s.replace("&", "&amp;")
        s = s.replace("<", "&lt;")
        s        
    }
}
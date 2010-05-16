import re

COMPILE_CMD = ["./harmonize", "compile", "--no-localize", "--dump-bytecode", "-d", "bin-test", "@compile_opts", "$filenm"]
EXECUTE_CMD = ["env", "CLASSPATH=bin-test", "./harmonize", "run", "$classnm"]
EXTENSION = ".harm"

def format_error(outfilenm, linenum, msg):
    # Just support "Glob-Style", where "*" matches any set of characters.
    msg = re.escape(msg.strip())
    msg = msg.replace(r"\*", r".*")
    return r"%s:%d:[0-9]+: %s" % (outfilenm, linenum, msg)
import re

COMPILE_CMD = ["./harmc", "--no-localize", "--dump-bytecode", "-d", "bin-test", "-sourcepath", "test", "@compile_opts", "$filenm"]
EXECUTE_CMD = ["./harm", "$classnm"]
EXTENSION = ".harm"

def format_error(outfilenm, linenum, msg):
    # Just support "Glob-Style", where "*" matches any set of characters.
    msg = re.escape(msg.strip())
    msg = msg.replace(r"\*", r".*")
    return r"%s:%d:[0-9]+: %s" % (outfilenm, linenum, msg)
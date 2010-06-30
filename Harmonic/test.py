#!/usr/bin/env python

"""

A UnitTest runner for a compiler system.  Reasonably generic.  You
provide it with the name of test file(s).  The test file(s) fall into
two categories, COMPILE, and EXECUTE.  See the section CONFIGURATION
to figure out how to configure it for your specific executable.

EXECUTE TESTS

Execute tests are tests that should be compiled and then
executed.  The expected output is indicated at the end of the
file like so:

  class Foo
  }
  // EXECUTE
  // expected output goes here
  // there can be more than one line

COMPILE TESTS

Compile tests are files that we do not execute, merely compile.  If
the file includes any ERROR lines, then the compilation is expected to
fail; otherwise, it is expected to succeed.

Here is an example of a compilation that is expected to fail:

  class Foo {
      void method() {
        int i = "str"; // ERROR Type Mismatch
      }
  }
  // COMPILE

Note the "// ERROR" on line 3.  The runtime will look for an error
message on the appropriate line that begins with the text provided,
and no other error messages.

Note that each "// ERROR" can potentially match more than one error on
a single line.  In this example:

  class Foo {
      a *@ 12edcm2 111 "some line with many different problems"; // ERROR
  }

the single ERROR will match all errors on the line, since it has no
associated message.

You can specify options to the compilation by putting them as additional
comment lines after the compile tag.  For example:
  class Foo {
      void method() {
        int i = "str"; // ERROR Type Mismatch
      }
  }
  // COMPILE
  // --no-infer-effects

FRAGMENTS

All test files can also contain fragments.  This is generally only useful
for negative tests, but it actually works for any kind of test.  A fragment
causes the test file to be exploded into several different test files,
each with the same header and footer but different stuff in between.

Each file is given a name based on the name of the fragment, and is processed
in turn.  This allows you to test a lot of error conditions more compactly
than you would be able to otherwise.  

Fragment files look like:

  class $FNAME$ {
      String field;

      // FRAGMENT copy from local
      void method() {
        int i=1;
        field = i; // ERROR Type Mismatch
      }

      // FRAGMENT copy from constraint
      void method() {
        field = 1; // ERROR Type Mismatch
      }

      // TRAILER
  }
  // COMPILE

Everything from the beginning until the first FRAGMENT is the header,
and it will appear in every resulting file.  Everything after TRAILER
is the trailer, and it also appears in every resulting file.  Everything
after a FRAGMENT appears in only one of the resulting files.  The name
of the FRAGMENT comes after the FRAGMENT.

CONFIGURATION

To run your tests, test.py needs to know the following things:

    COMPILE_CMD: a command to execute the compiler on a given test file
    EXECUTE_CMD: a command to execute the output results 
    EXTENSION: the extension used for your test files

and can optionally be configured in the following ways:
    
    ERROR_FORMAT: a format string which, gives (filenm, linenum,
    message) as arguments, creates a regular expression which is
    matched against the actual errors observed (default: %s:%d: %s)
    See also format_error() below.
    
    ERROR_RE: a regular expression that matches an error message in
    the output (default: r'^[a-zA-Z/_0-9.$]+:[0-9]+:')
    
    COMMENT: the marker to use when looking for special comments
    (default: //)
    
    FAILED_COMPILATION: a function which, when invoked with the result
    code, stdout, and stderr of a compilation, returns True if the
    compilation resulted in an error.  The default checks if the
    result code is 0.

    format_error(filenm, linenum, msg): The "test_config.py" module
    discussed below can optionally define a function rather than
    the ERROR_FORMAT string described above.

Configuration is set by adding a module "test_config.py" which sets
various global values.  The COMPILE_CMD and EXECUTE_CMD variables take
the form of a list, where any one of the terms may be the special
variables $filenm or $classnm, in which case they are replaced with
the relative path of the test file, or the relative path converted to
a Java class name.  The variable "@compile_opts", when it is the entire
string, expands in-place to the list of options that appeared after //
COMPILE, separated by whitespace.

As an example, to use the java compiler, you would want settings like:

    COMPILE_CMD = ["javac", "$filenm"]
    EXECUTE_CMD = ["java", "$classnm"]
    EXTENSION = ".java"
"""

import sys, subprocess, os, os.path, difflib, re

# ----------------------------------------------------------------------
# Configurable Parameters
#
# You can set these in the environment if you like, or define a module
# test_config.py and define them there.

try:
    import test_config
except ImportFailure:
    print >> sys.stderr, "test_config.py not found!"
    sys.exit(1)

def define(varnm, defval):
    if hasattr(test_config, varnm):
        globals()[varnm] = getattr(test_config, varnm)
    else:
        globals()[varnm] = defval

define("COMPILE_CMD", None) # i.e., "javac"
define("EXECUTE_CMD", None) # i.e., "java"
define("EXTENSION", None)   # i.e., ".java"
define("COMMENT", '//')
define("ERROR_FORMAT", "%s:%d: %s")
define("ERROR_RE", r'^[a-zA-Z/_0-9.$]+:[0-9]+:')
define("FAILED_EXECUTION", lambda res, stdout, stderr: res != 0)

if hasattr(test_config, 'format_error'):
    format_error = test_config.format_error
else:
    def format_error(outfilenm, linenum, msg):
        return ERROR_FORMAT % (outfilenm, linenum, msg)

# Command line parameters:

# --preserve indicates that intermediate files should NOT be erased,
# even if the test passes.
preserve    = False 

# ----------------------------------------------------------------------
# Exceptions, constants, etc

class InvalidMetaData(Exception):
    pass

class CompilationFailed(Exception):
    pass

class NotExecutable(Exception):
    pass

COMPILE = 'COMPILE'
EXECUTE = 'EXECUTE'
SKIP = 'SKIP'
TEST_TYPES = (COMPILE, EXECUTE, SKIP)

# ------------------------------------------------------------------------
# Fragment Code
#
# Code to read in a file and break it into separate fragments.

FRAGMENT = COMMENT + " FRAGMENT"
TRAILER  = COMMENT + " TRAILER"
ERROR    = COMMENT + " ERROR"

class FragmentFile(object):
    def __init__(self, fname):
        self.fname = os.path.splitext(fname)
        self.header = []
        self.header_errors = []
        self.trailer = []
        self.fragment_names = []
        self.fragment_lines = []
        self.fragment_errors = []

    def fragment_count(self):
        """ Returns the number of fragments in this file """
        if not self.fragment_names:
            # If the user did not create any fragments, we'll treat the
            # whole file as one big fragment
            return 1
        else:
            return len(self.fragment_names)

    def assemble(self, fragnum, ):
        """ Creates an output file for fragment #'fragnum', returning a
        triple:

           (output_name, [expected_errors], [clean_names])

        output_name is a string, the name of the output file.

        expected_errors is a list of errors we expect to see when
        compiling this output file

        clean_names is a list of file names to be erased if the test
        is successful.  Usually contains output_name, but not necessarily. """

        if not self.fragment_names:
            # the synthetic fragment
            outfilenm = "".join(self.fname)
            clean_names = []
        else:
            # Generate the name of the file:
            outfilenm = "%s_frag%02d%s" % (
                self.fname[0], fragnum, self.fname[1])
            clean_names = [outfilenm]

            # Generate the text of the file:
            outfile = open(outfilenm, 'w')
            classnm = os.path.splitext(os.path.basename(outfilenm))[0]
            hdr = [s.replace('$FNAME$', classnm) for s in self.header]
            frag = [s.replace('$FNAME$', classnm) for s in self.fragment_lines[fragnum]]
            outfile.writelines(hdr)
            outfile.writelines(frag)
            outfile.writelines(self.trailer)
            outfile.close()

        # Generate the list of expected errors
        experrors = []
        for linenm, msg in self.header_errors:
            experrors.append(format_error(outfilenm, linenm, msg))
        if self.fragment_errors:
            for linenm, msg in self.fragment_errors[fragnum]:
                experrors.append(format_error(
                    outfilenm, linenm+len(self.header), msg))
            
        return (outfilenm, experrors, clean_names)

    # ------------------------------------------------------------
    # Routines used when building the fragment file:

def add_to_header(self, line):
    self.header.append(line)

def add_header_exp_error(self, rel, experror):
    self.header_errors.append(
        (len(self.header) + rel, experror))

def add_to_trailer(self, line):
    self.trailer.append(line)

def add_trailer_exp_error(self, res, experror):
    raise Exception("Trailer cannot have expected errors")

def start_fragment(self, name):
    self.fragment_names.append(name)
    self.fragment_lines.append([])
    self.fragment_errors.append([])

def add_to_fragment(self, line):
    self.fragment_lines[-1].append(line)

def add_frag_exp_error(self, rel, experror):
    self.fragment_errors[-1].append(
        (len(self.fragment_lines[-1]) + rel, experror))

    # ----------------------------------------------------------------------
    # Fragment parser

def extract_fragments(filenm):
    res = FragmentFile(filenm)
    fileobj = open(filenm)
    addfunc = add_to_header
    errorfunc = add_header_exp_error

    for line in fileobj.readlines():
        
        sline = line.strip()

        # Start of a new fragment?
        if sline.startswith(FRAGMENT):
            # Extract description of fragment as a list of words
            fragment_desc = sline[len(FRAGMENT):].split()
            start_fragment(res, fragment_desc)
            addfunc = add_to_fragment
            errorfunc = add_frag_exp_error

        # Start of the trailer?
        elif sline.startswith(TRAILER):
            addfunc = add_to_trailer
            errorfunc = add_trailer_exp_error

        # Otherwise, add the line and check for any expected errors
        else:
            addfunc(res, line)
            if ERROR in line:
                experror = line[line.index(ERROR)+len(ERROR):]
                rel = 0
                while experror.startswith("^"):
                    rel -= 1
                    experror = experror[1:]
                errorfunc(res, rel, experror)

    return res

# ----------------------------------------------------------------------
# Code for determing what to do with a test file

def extract_metadata(filenm):
    """ Extracts the skip state, compile options, and expected output
    from the comments at the end of the file. """
    
    filelines = open(filenm).readlines()
    lines = []
    for line in reversed(filelines):
        if line.startswith(COMMENT):
            line = line[len(COMMENT):].strip()
            lines.append(line)
        else:
            break

    lines = list(reversed(lines))

    provided = 0

    if EXECUTE in lines:
        execute_idx = lines.index(EXECUTE)
        execute_output = lines[execute_idx+1:]
        provided += 1
    else:
        execute_idx = len(lines)
        execute_output = None
        
    if SKIP in lines:
        skip_idx = lines.index(SKIP)
        skip = True
        provided += 1
    else:
        skip = False

    if COMPILE in lines:
        compile_idx = lines.index(COMPILE)
        compile_opts = lines[compile_idx+1:execute_idx]
        provided += 1
    else:
        compile_idx = len(lines)
        compile_opts = []

    if not provided:
        raise InvalidMetaData(
            "No meta-data provided, expected at least one of "+str(TEST_TYPES))

    return (skip, compile_opts, execute_output)

# ----------------------------------------------------------------------
# Code for logging the result of the test run

class Results(object):

    def __init__(self, filenm):
        assert filenm.endswith(EXTENSION)
        if filenm.startswith('./'): filenm = filenm[2:]
        self.compile_opts = []
        self.filenm = filenm
        self.classnm = filenm[:-len(EXTENSION)].replace('/','.')
        self.logfilenm = self.filenm[:-len(EXTENSION)]+".test"
        self.logfile = open(self.logfilenm, 'w')

    def add_remove_files(self, fnames):
        self.remove_files += fnames

    def set_compile_opts(self, opts):
        self.compile_opts = opts

    def specialize(self, cmd):
        def helper(str):
            if str[0] == "$":
                res.append(getattr(self, str[1:]))
            elif str[0] == "@":
                res.extend(getattr(self, str[1:]))
            else:
                res.append(str)
        res = []
        for s in cmd: helper(s)
        return res

    def command(self, cmd, retcode, stdout, stderr):
        """ Logs a command and its output to the log file """
        file = self.logfile
        self.divider()
        file.write('* Command: %r\n' % cmd)
        file.write('* Return code: %s\n' % retcode)
        file.write('* Standard Out:\n')
        file.write(stdout)
        file.write('* Standard Error:\n')
        file.write(stderr)
        file.write('\n')

    def diff(self, expected, diff):
        """ Logs the expected results, and diff from them, that we found """
        file = self.logfile
        if expected:
            file.write('* Expected:\n')
            file.write('\n'.join(expected))
            file.write('\n')
        if diff:
            file.write('* Diff/Not Found:\n')
            file.write('\n'.join(diff))
            file.write('\n')

    def divider(self):
        self.logfile.write("_"*70+"\n")

    def error(self, message):
        self.divider()
        print "  ERROR:", message
        self.logfile.write("ERROR: %s\n" % message)
        
    def log(self, text):
        """ Adds an entry to log file, and prints to stdout """
        print text
        self.logfile.write(text+"\n")
        
    def flush_all(self):
        """ Flushes log file """
        self.logfile.flush()

    def remove_all(self):
        """ Closes and removes the log file and any other files;
        normally used if no errors """
        self.close_all()
        remove(self.logfilenm)

    def close_all(self):
        """ Closes log file """
        self.logfile.close()

# ----------------------------------------------------------------------
# Code for running sub-commands and scanning the output

def remove(filenm):
    try:
        os.remove(filenm)
    except OSError: pass
    
def execute(res, args):
    args = res.specialize(args)
    def _execute():
        try:
            obj = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            (stdout, stderr) = obj.communicate()
            return (obj.returncode, stdout, stderr)
        except OSError, e:
            return (-1, "", "Error running command: %s" % e)
    ret = _execute()
    res.command(args, *ret)
    return ret

# ----------------------------------------------------------------------
# Code for negative tests

def create_error_list(stdout, stderr):
    """
    Given the output of the compiler, creates a list of error
    messages.  Expects that error messages begin with ERROR_RE.
    Concatenates subsequent lines that have more whitespace than the
    original.
    """
    #import pdb; pdb.set_trace()
    combined = stdout + "\n" + stderr
    experrors = []
    expwhitespace = False
    for line in combined.split('\n'):
        if re.match(ERROR_RE, line):
            # Looks like the beginning of a new error msg:
            experrors.append(line)
            expwhitespace = True
        elif re.match(r'\s+', line) and expwhitespace:
            # Line which continues an error msg:
            experrors[-1] += "\n"+line
        else:
            expwhitespace = False
    return experrors

def normalize_whitespace(str):
    return " ".join(str.split())

def compare_errors(expected, stdout, stderr):
    """ Compares the list of expected and actual errors,
    and returns a tuple (unexpected, notfound) containing any
    unexpected errors, and any errors that were not found. """
    actual = create_error_list(stdout, stderr)
    expected = [normalize_whitespace(e) for e in expected]
    found = [False for e in expected]
    unexpected = []
    for actual_error in actual:
        normalized = normalize_whitespace(actual_error)
        for idx, expected_error in enumerate(expected):
            if re.match(expected_error, normalized):
                found[idx] = True
                break
        else:
            # We encountered an unexpected error!
            unexpected.append(actual_error)

    # Create list of errors we expected but did not find:
    notfound = [err for fnd, err in zip(found, expected) if not fnd]
    
    return (unexpected, notfound)

# ----------------------------------------------------------------------
# Code for running different kinds of tests

def handle_negative_test(res, experrors, retcode, stdout, stderr):
    if not experrors:
        res.error("Negative test, but no expected errors indicated")
        return 1
    unexpected, notfound = compare_errors(experrors, stdout, stderr)
    if unexpected or notfound:
        res.error("%d unexpected error(s) found, %d expected error(s) missing"
                  % (len(unexpected), len(notfound)))
        res.diff(experrors,
                 ['- ' + err for err in notfound]+
                 ['+ ' + err for err in unexpected])
        return 1
    return 0

def handle_positive_test(res, exp_output, retcode, stdout, stderr):
    diff = list(difflib.unified_diff(
            exp_output, (stdout+stderr).strip().split('\n'), lineterm=""))
    if diff:
        res.error("expected output not found")
        res.diff(exp_output, diff)
        return 1
    return 0

# ----------------------------------------------------------------------
# Code that tests a file

def test_file(filenm):

    """ Returns 0 on success, 1 on error """

    def _run_test(filenm, experrors):
        try:
            try:
                # Analyze file:
                (skip, compile_opts, execute_output) = extract_metadata(filenm)            
            except InvalidMetaData, e:
                res.error("Invalid metadata (%s)" % str(e))
                return 1
            
            if skip:
                res.log("Skipping test")
                return 0

            # It's user error if errors are expected, but there is also
            # expected output.
            if experrors and execute_output is not None:
                res.error(
                    "Metadata says to execute the test, "+
                    "but compile errors are expected")
                return 1

            # Try to compile the file
            res.set_compile_opts(compile_opts)
            retcode, stdout, stderr = execute(res, COMPILE_CMD)

            # If the compile failed...
            #    (or if we don't TRUST_RETURN_CODEs and we expected it to fail)
            if FAILED_EXECUTION(retcode, stdout, stderr):

                # Was this a test where compilation was expected to fail?
                if experrors:
                    return handle_negative_test(
                        res, experrors, retcode, stdout, stderr)

                # Otherwise, the test failed
                res.error("Unexpected compilation failure.")
                return 1

            # Compilation succeeded: but we're errors expected?
            if experrors:
                res.error("Compilation succeeded, but errors were expected")
                res.diff(experrors, None)
                return 1

            # If we are not supposed to execute, we are done
            if execute_output is None:
                return 0

            # Otherwise, try to execute it
            retcode, stdout, stderr = execute(res, EXECUTE_CMD)
            return handle_positive_test(
                res, execute_output, retcode, stdout, stderr)
        except:
            res.log("  Interrupted!")
            res.flush_all()
            raise

    # Break the file into fragments.  If no fragments exists, this
    # still works due to fragtest API:
    ff = extract_fragments(filenm)
    failcnt = 0 # count number of failed tests
    for i in range(ff.fragment_count()):
        outputfilenm, experrors, rmfiles = ff.assemble(i)
        
        res = Results(outputfilenm)
        res.log("Testing %s..." % outputfilenm)
        if not _run_test(outputfilenm, experrors):
            # Test was successful, remove fragment files and log file (if any)
            if not preserve:
                res.remove_all()
                for f in rmfiles: remove(f)
            else:
                res.close_all()
        else:
            # Test failed, just close the log, and leave the
            # generated fragment file
            res.close_all()
            failcnt += 1            
        
    return failcnt

def main(args):

    global preserve
    
    # Check for preserve flag:
    if args and args[0] == "--preserve":
        preserve = True
        args = args[1:]

    # Automatically skip tests that contain _frag00, as these are
    # remnants of older tests:
    args = [a for a in args if not re.search('_frag[0-9]+.', a)]

    # Usage
    if (len(args) < 1
        or COMPILE_CMD is None
        or EXECUTE_CMD is None
        or EXTENSION is None):
        err = sys.stderr
        if EXTENSION is None: extension_text = ""
        else: extension_text = EXTENSION
        print >> err, "Usage: test [--preserve] somefile%s*" % (extension_text,)
        print >> err, ""
        print >> err, "Required environment variables:"
        print >> err, "    COMPILE_CMD: compiler command to run (ex: 'javac')"
        print >> err, "    EXECUTE_CMD: command to execute code (ex: 'java')"
        print >> err, "    EXTENSION: extension of test files   (ex: '.java')"
        print >> err, ""
        print >> err, "Optional environment variables:\n"
        print >> err, "    COMMENT: begin comment marker, default '//'"
        print >> err, ""
        print >> err, "If you need more guidance, read the comment in the"
        print >> err, "beginning of the source code."
        sys.exit(-1)

    # Run tests and count errors
    errors = 0
    for filenm in args:
        errors += test_file(filenm)

    # Cleanup:
    if not errors:
        print "Summary: all tests passed"
    else:
        print "Summary: %d TESTS FAILED" % errors
    sys.exit(errors)

if __name__ == "__main__":
    main(sys.argv[1:])

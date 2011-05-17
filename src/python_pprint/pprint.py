#!/usr/bin/env python
"""Python AST pretty-printer."""

import ast
import codegen
import sys
import getopt

def usage():
	print "Print Python AST"
	print "Usage   : " + __file__, "[option]", "<.py>"
	print "Option  :  -l = print line number & column info"

def pprint(filename, linenumber):
        src = "".join(file(filename).readlines())
        root = ast.parse(src)
        # Do not annotate field, but print line number & column info
        print ast.dump(root, False, linenumber)

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hl", ["help", "linenumber"])
    except getopt.GetoptError, err:
        # print help information and exit:
        print str(err) # will print something like "option -a not recognized"
        usage()
        sys.exit(2)
    linenumber = False
    if(len(args) != 1):
	usage()
	sys.exit()
    for o, a in opts:
        if o == "-l":
            linenumber = True
        elif o in ("-h", "--help"):
            usage()
            sys.exit()
        else:
            assert False, "unhandled option"
    pprint(args[0], linenumber)

if __name__ == "__main__":
    main()


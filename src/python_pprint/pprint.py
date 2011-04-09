#!/usr/bin/env python
"""Python AST pretty-printer."""

import ast
import codegen
import sys

def usage():
	print "Print Python AST:"
	print "Usage: ./" + __file__ + " <.py>"

if __name__ == '__main__':
    def main():
	if(len(sys.argv) != 2):
		usage()
	else:
            src = "".join(file(sys.argv[1]).readlines())
            root = ast.parse(src)
            # Do not annotate field, but print line number & column info
            print ast.dump(root, False, True)
    main()

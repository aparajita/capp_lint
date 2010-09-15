#!/usr/bin/env python

import fileinput
import os
import os.path
import re
import sys

VAR_RE = re.compile(r"^(\s*)var \w+.*([,;])\s*$")

errors = []

def checkfile(path):
    path = os.path.join(os.getcwd(), path.rstrip())

    try:
        with open(path) as f:
            lineNum = 0
            lastDeclarationLineNum = 0

            # Look far variable declarations
            for line in f:
                lineNum += 1
                match = VAR_RE.search(line)

                if match is not None:
                    indent = match.group(1)
                    lastTerminator = match.group(2)
                    blockHasSemicolon = lastTerminator == ";"
                    lastDeclarationLine = line
                    lastDeclarationLineNum = lineNum

                    # Now check the following lines for indented variable declarations.
                    followRE = re.compile(r"^" + indent + r"    (\w+.*([,;])\s*|//.*|/\*.*?\*/\s*)$")
                    # print "START"
                    # print "{0} {1}".format(lineNum, line)

                    while True:
                        line = f.next()
                        lineNum += 1

                        if not line:
                            break

                        if len(line.strip()) == 0:
                            continue

                        match = followRE.search(line)

                        if match is None:
                            if lastTerminator == ",":
                                errors.append(("unterminated block", lastDeclarationLineNum, lastDeclarationLine))
                                # print "DANGLING BLOCK! on line {0}".format(lastDeclarationLineNum)
                            else:
                                # print "END"
                                pass
                            break
                        elif match.group(1).startswith("//") or match.group(1).startswith("/*"):
                            # print "COMMENT"
                            continue
                        else:
                            lastTerminator = match.group(2)
                            lastDeclarationLine = line
                            lastDeclarationLineNum = lineNum

                            if blockHasSemicolon:
                                # If the block already has a semicolon, we have an inadvertent global declaration
                                errors.append(("inadvertent global", lineNum, line))
                                # print "GLOBAL! {0} {1}".format(lineNum, line)
                            else:
                                if (lastTerminator == ";"):
                                    blockHasSemicolon = True

                                # print "{0} {1}".format(lineNum, line)

    except Exception as ex:
        print ex
        pass


if __name__ == "__main__":
    for filepath in fileinput.input():
        checkfile(filepath)

    if errors:
        for error, lineNum, line in errors:
            print "{0}:{1}: {2}.\n+{3}".format(filepath.rstrip(), lineNum, error, line),

        sys.exit(1)
    else:
        sys.exit(0)

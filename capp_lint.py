#!/usr/bin/env python

from __future__ import with_statement
from optparse import OptionParser
from string import Template
import cgi
import os.path
import re
import sys
import unicodedata


def tabs2spaces(text, positions=None):
    while True:
        index = text.find(u'\t')

        if index < 0:
            return text

        spaces = u' ' * (4 - (index % 4))
        text = text[0:index] + spaces + text[index + 1:]

        if positions is not None:
            positions.append(index)

class LintChecker(object):

    VAR_BLOCK_START_RE = re.compile(ur'''(?x)
        (?P<indent>\s*)         # indent before a var keyword
        (?P<var>var\s+)         # var keyword and whitespace after
        (?P<identifier>[a-zA-Z_$]\w*)\s*
        (?:
            (?P<assignment>=)\s*
            (?P<expression>.*)
            |
            (?P<separator>[,;+\-/*%^&|=\\])
        )
    ''')

    SEPARATOR_RE = re.compile(ur'''(?x)
        (?P<expression>.*)              # Everything up to the line separator
        (?P<separator>[,;+\-/*%^&|=\\]) # The line separator
        \s*                             # Optional whitespace after
        $                               # End of expression
    ''')

    INDENTED_EXPRESSION_RE_TEMPLATE = ur'''(?x)
        [ ]{%d}                 # Placeholder for indent of first identifier that started block
        (?P<expression>.+)      # Expression
    '''

    VAR_BLOCK_RE_TEMPLATE = ur'''(?x)
        [ ]{%d}                 # Placeholder for indent of first identifier that started block
        (?P<indent>\s*)         # Capture any further indent
        (?:
            (?P<bracket>[\[\{].*)
            |
            (?P<identifier>[a-zA-Z_$]\w*)\s*
            (?:
                (?P<assignment>=)\s*
                (?P<expression>.*)
                |
                (?P<separator>[,;+\-/*%%^&|=\\])
            )
            |
            (?P<indented_expression>.+)
        )
    '''

    STRIP_LINE_COMMENT_RE = re.compile(ur'(.*)\s*(?://.*|/\*.*\*/\s*)$')
    LINE_COMMENT_RE = re.compile(ur'\s*(?:/\*.*\*/\s*|//.*)$')
    BLOCK_COMMENT_START_RE = re.compile(ur'\s*/\*.*(?!\*/\s*)$')
    BLOCK_COMMENT_END_RE = re.compile(ur'.*?\*/')
    FUNCTION_RE = re.compile(ur'\s*function\s*(?P<name>[a-zA-Z_$]\w*)?\(.*\)\s*\{?')
    STRING_LITERAL_RE = re.compile(ur'(?<!\\)(["\'])(.*?)(?<!\\)\1')
    EMPTY_STRING_LITERAL_FUNCTION = lambda match: match.group(1) + (len(match.group(2)) * ' ') + match.group(1)
    EMPTY_SELF_STRING_LITERAL_FUNCTION = lambda self, match: match.group(1) + (len(match.group(2)) * ' ') + match.group(1)

    LINE_CHECKLIST = (
        {
            'id': 'tabs',
            'regex': re.compile(ur'[\t]'),
            'error': 'line contains tabs',
        },
        {
            'regex': re.compile(ur'([^\t -~])'),
            'error': 'line contains non-ASCII characters',
            'showPositionForGroup': 1,
        },
        {
            'regex': re.compile(ur'^\s*(?:(?:else )?if|for|switch|while|with)(\()'),
            'error': 'missing space between control statement and parentheses',
            'showPositionForGroup': 1,
        },
        {
            'regex': re.compile(ur'^\s*(?:(?:else )?if|for|switch|while|with)\s*\(.+\)\s*(\{)\s*(?://.*|/\*.*\*/\s*)?$'),
            'error': 'braces should be on their own line',
            'showPositionForGroup': 1,
        },
        {
            'regex': re.compile(ur'^.*\s+$'),
            'error': 'trailing whitespace',
        },
        {
            # Filter out @import statements, method declarations, unary plus/minus/increment/decrement
            'filter': { 'regex': re.compile(ur'(^@import\b|^\s*[-+]\s*\([a-zA-Z_$]\w*\)\s*[a-zA-Z_$]\w*|[a-zA-Z_$]\w*(\+\+|--)|([ -+*/%^&|<>!]=?|&&|\|\||<<|>>>|={1,3}|!==?)\s*[-+][\w(\[])'), 'pass': False },

            # Replace the contents of literal strings with spaces so we don't get false matches within them
            'preprocess': { 'regex': STRING_LITERAL_RE, 'replace': EMPTY_STRING_LITERAL_FUNCTION },
            'regex':      re.compile(ur'(?<=[\w)\]"\']|([ ]))([-+*/%^]|&&?|\|\|?|<<|>>>?)(?=[\w({\["\']|(?(1)\b\b|[ ]))'),
            'error':      'binary operator without surrounding spaces',
            'showPositionForGroup': 2,
        },
        {
            # Filter out @import statements, method declarations
            'filter': { 'regex': re.compile(ur'(^@import\b|^\s*[-+]\s*\([a-zA-Z_$]\w*\)\s*[a-zA-Z_$]\w*)'), 'pass': False },

            # Replace the contents of literal strings with spaces so we don't get false matches within them
            'preprocess': { 'regex': STRING_LITERAL_RE, 'replace': EMPTY_STRING_LITERAL_FUNCTION },
            'regex':      re.compile(ur'(?:[-*/%^&|<>!]=?|&&|\|\||<<|>>>|={1,3}|!==?)\s*(?<!\+)(\+)[\w(\[]'),
            'error':      'useless unary + operator',
            'showPositionForGroup': 1,
        },
        {
            # Filter out possible = within @accessors
            'filter': { 'regex': re.compile(ur'^\s*(?:@outlet\s+)?[a-zA-Z_$]\w*\s+[a-zA-Z_$]\w*\s+@accessors\b'), 'pass': False },

            # Replace the contents of literal strings with spaces so we don't get false matches within them
            'preprocess': { 'regex': STRING_LITERAL_RE, 'replace': EMPTY_STRING_LITERAL_FUNCTION },
            'regex':      re.compile(ur'(?<=[\w)\]"\']|([ ]))(=|[-+*/%^&|]=|<<=|>>>?=)(?=[\w({\["\']|(?(1)\b\b|[ ]))'),
            'error':      'assignment operator without surrounding spaces',
            'showPositionForGroup': 2,
        },
        {
            # Filter out @import statements and @implementation/method declarations
            'filter': { 'regex': re.compile(ur'^(@import\b|@implementation\b|[-+]\s*\([a-zA-Z_$]\w*\)\s*[a-zA-Z_$]\w*)'), 'pass': False },

            # Replace the contents of literal strings with spaces so we don't get false matches within them
            'preprocess': { 'regex': STRING_LITERAL_RE, 'replace': EMPTY_STRING_LITERAL_FUNCTION },
            'regex':      re.compile(ur'(?<=[\w)\]"\']|([ ]))(===?|!==?|[<>]=?)(?=[\w({\["\']|(?(1)\b\b|[ ]))'),
            'error':      'comparison operator without surrounding spaces',
            'showPositionForGroup': 2,
        },
        {
            'regex': re.compile(ur'^(\s+)[-+]\s*\([a-zA-Z_$]\w*\)\s*[a-zA-Z_$]\w*|^\s*[-+](\()[a-zA-Z_$]\w*\)\s*[a-zA-Z_$]\w*|^\s*[-+]\s*\([a-zA-Z_$]\w*\)(\s+)[a-zA-Z_$]\w*'),
            'error': 'extra or missing space in a method declaration',
            'showPositionForGroup': 0,
        },
        {
            'regex': re.compile(ur'^\s*var\s+[a-zA-Z_$]\w*\s*=\s*function\s+([a-zA-Z_$]\w*)\s*\('),
            'error': 'function name is ignored',
            'showPositionForGroup': 1,
            'skip' : True
        },
    )

    VAR_DECLARATIONS        = ['none', 'single', 'strict']
    VAR_DECLARATIONS_NONE   = 0
    VAR_DECLARATIONS_SINGLE = 1
    VAR_DECLARATIONS_STRICT = 2


    def __init__(self, var_declarations=VAR_DECLARATIONS_SINGLE, verbose=False):
        self.errors = []
        self.varDeclarations = var_declarations
        self.verbose = verbose
        self.sourcefile = None
        self.filename = u''
        self.line = u''
        self.lineNum = 0
        self.varIndent = u''
        self.identifierIndent = u''

        self.fileChecklist = (
            { 'title': 'Check variable blocks', 'action': self.check_var_blocks },
        )


    def run_line_checks(self):
        for check in self.LINE_CHECKLIST:
            line = self.line
            lineFilter = check.get('filter')

            if lineFilter:
                match = lineFilter['regex'].search(line)

                if (match and not lineFilter['pass']) or (not match and lineFilter['pass']):
                    continue

            preprocess = check.get('preprocess')

            if preprocess:
                regex = preprocess.get('regex')

                if regex:
                    line = regex.sub(preprocess.get('replace', ''), line)

            regex = check.get('regex')

            if not regex:
                continue

            match = regex.search(line)

            if not match:
                continue

            positions = []
            group = check.get('showPositionForGroup')

            if (check.get('id') == 'tabs'):
                line = tabs2spaces(line, positions=positions)
            elif group is not None:
                line = tabs2spaces(line)

                for match in regex.finditer(line):
                    if group > 0:
                        positions.append(match.start(group))
                    else:
                        # group 0 means show the first non-empty match
                        for i in range(1, len(match.groups()) + 1):
                            if match.start(i) >= 0:
                                positions.append(match.start(i))
                                break

            self.error(check['error'], positions=positions)


    def next_statement(self, expect_line=False, check_line=True):
        try:
            while True:
                self.line = unicode(self.sourcefile.next()[:-1], 'utf-8', 'strict')  # strip EOL, convert to Unicode
                self.lineNum += 1

                if self.verbose:
                    print u'%d: %s' % (self.lineNum, tabs2spaces(self.line))

                if not self.is_statement():
                    continue

                if check_line:
                    self.run_line_checks()

                return True

        except StopIteration:
            if expect_line:
                self.error('unexpected EOF')
            raise


    def is_statement(self):
        # Skip empty lines
        if len(self.line.strip()) == 0:
            return False

        # See if we have a line comment, skip that
        match = self.LINE_COMMENT_RE.match(self.line)

        if match:
            return False

        # Match a block comment start next so we can find its end,
        # otherwise we might get false matches on the contents of the block comment.
        match = self.BLOCK_COMMENT_START_RE.match(self.line)

        if match:
            self.block_comment()
            return False

        return True


    def strip_comment(self):
        match = self.STRIP_LINE_COMMENT_RE.match(self.expression)

        if match:
            self.expression = match.group(1)


    def get_expression(self, lineMatch):
        groupdict = lineMatch.groupdict()

        self.expression = groupdict.get('expression')

        if self.expression is None:
            self.expression = groupdict.get('bracket')

        if self.expression is None:
            self.expression = groupdict.get('indented_expression')

        if self.expression is None:
            self.expression = ''
            return

        # Remove all quoted strings from the expression so that we don't
        # count unmatched pairs inside the strings.
        self.expression = self.STRING_LITERAL_RE.sub(self.EMPTY_SELF_STRING_LITERAL_FUNCTION, self.expression)

        self.strip_comment()
        self.expression = self.expression.strip()


    def block_comment(self):
        'Find the end of a block comment'

        commentOpenCount = self.line.count('/*')
        commentOpenCount -= self.line.count('*/')

        # If there is an open comment block, eat it
        if commentOpenCount:
            if self.verbose:
                print u'%d: BLOCK COMMENT START' % self.lineNum
        else:
            return

        match = None

        while not match and self.next_statement(expect_line=True, check_line=False):
            match = self.BLOCK_COMMENT_END_RE.match(self.line)

        if self.verbose:
            print u'%d: BLOCK COMMENT END' % self.lineNum


    def balance_pairs(self, squareOpenCount, curlyOpenCount, parenOpenCount):
        # The following lines have to be indented at least as much as the first identifier
        # after the var keyword at the start of the block.
        if self.verbose:
            print "%d: BALANCE BRACKETS: '['=%d, '{'=%d, '('=%d" % (self.lineNum, squareOpenCount, curlyOpenCount, parenOpenCount)

        lineRE = re.compile(self.INDENTED_EXPRESSION_RE_TEMPLATE % len(self.identifierIndent))

        while True:
            # If the expression has open brackets and is terminated, it's an error
            match = self.SEPARATOR_RE.match(self.expression)

            if match and match.group('separator') == ';':
                unterminated = []

                if squareOpenCount:
                    unterminated.append('[')

                if curlyOpenCount:
                    unterminated.append('{')

                if parenOpenCount:
                    unterminated.append('(')

                self.error('unbalanced %s' % ' and '.join(unterminated))
                return False

            self.next_statement(expect_line=True)
            match = lineRE.match(self.line)

            if not match:
                # If it doesn't match, the indent is wrong check the whole line
                self.error('incorrect indentation')
                self.expression = self.line
                self.strip_comment()
            else:
                # It matches, extract the expression
                self.get_expression(match)

            # Update the bracket counts
            squareOpenCount += self.expression.count('[')
            squareOpenCount -= self.expression.count(']')
            curlyOpenCount += self.expression.count('{')
            curlyOpenCount -= self.expression.count('}')
            parenOpenCount += self.expression.count('(')
            parenOpenCount -= self.expression.count(')')

            if squareOpenCount == 0 and curlyOpenCount == 0 and parenOpenCount == 0:
                if self.verbose:
                    print u'%d: BRACKETS BALANCED' % self.lineNum

                # The brackets are closed, this line must be separated
                match = self.SEPARATOR_RE.match(self.expression)

                if not match:
                    self.error('missing statement separator')
                    return False

                return True


    def pairs_balanced(self, lineMatchOrBlockMatch):

        groups = lineMatchOrBlockMatch.groupdict()

        if groups.has_key('assignment') or groups.has_key('bracket'):
            squareOpenCount = self.expression.count('[')
            squareOpenCount -= self.expression.count(']')

            curlyOpenCount = self.expression.count('{')
            curlyOpenCount -= self.expression.count('}')

            parenOpenCount = self.expression.count('(')
            parenOpenCount -= self.expression.count(')')

            if squareOpenCount or curlyOpenCount or parenOpenCount:
                # If the brackets were not properly closed or the statement was
                # missing a separator, skip the rest of the var block.
                if not self.balance_pairs(squareOpenCount, curlyOpenCount, parenOpenCount):
                    return False

        return True


    def var_block(self, blockMatch):
        # Keep track of whether this var block has multiple declarations
        isSingleVar = True

        # Keep track of the indent of the var keyword to compare with following lines
        self.varIndent = blockMatch.group('indent')

        # Keep track of how far the first variable name is indented to make sure
        # following lines line up with that
        self.identifierIndent = self.varIndent + blockMatch.group('var')

        # Check the expression to see if we have any open [ or { or /*
        self.get_expression(blockMatch)

        if not self.pairs_balanced(blockMatch):
            return (False, False)

        separator = ''

        if self.expression:
            match = self.SEPARATOR_RE.match(self.expression)

            if not match:
                self.error('missing statement separator')
            else:
                separator = match.group('separator')
        elif blockMatch.group('separator'):
            separator = blockMatch.group('separator')

        # If the block has a semicolon, there should be no more lines in the block
        blockHasSemicolon = separator == ';'

        # We may not catch an error till after the line that is wrong, so keep
        # the most recent declaration and its line number.
        lastBlockLine = self.line
        lastBlockLineNum = self.lineNum

        # Now construct an RE that will match any lines indented at least as much
        # as the var keyword that started the block.
        blockRE = re.compile(self.VAR_BLOCK_RE_TEMPLATE % len(self.identifierIndent))

        while self.next_statement(expect_line=not blockHasSemicolon):

            if not self.is_statement():
                continue

            # Is the line indented at least as much as the var keyword that started the block?
            match = blockRE.match(self.line)

            if match:
                lastBlockLine = self.line
                lastBlockLineNum = self.lineNum

                # If the line is indented farther than the first identifier in the block,
                # it is considered a formatting error.
                if match.group('indent') and not match.group('indented_expression'):
                    self.error('incorrect indentation')

                self.get_expression(match)

                if not self.pairs_balanced(match):
                    return (False, isSingleVar)

                if self.expression:
                    separatorMatch = self.SEPARATOR_RE.match(self.expression)

                    if separatorMatch is None:
                        # If the assignment does not have a separator, it's an error
                        self.error('missing statement separator')
                    else:
                        separator = separatorMatch.group('separator')

                        if blockHasSemicolon:
                            # If the block already has a semicolon, we have an inadvertent global declaration
                            self.error('inadvertent global variable')
                        elif (separator == ';'):
                            blockHasSemicolon = True
                elif match.group('separator'):
                    separator = match.group('separator')

                isSingleVar = False

            else:
                # If the line does not match, it is not an assignment or is outdented from the block.
                # In either case, the block is considered closed. If the most recent separator was not ';',
                # the block was not properly terminated.
                if separator != ';':
                    self.error('unterminated var block', lineNum=lastBlockLineNum, line=lastBlockLine)

                return (True, isSingleVar)


    def check_var_blocks(self):
        lastStatementWasVar = False
        lastVarWasSingle = False
        haveLine = True

        while True:
            if not haveLine:
                haveLine = self.next_statement()

            if not self.is_statement():
                haveLine = False
                continue

            match = self.VAR_BLOCK_START_RE.match(self.line)

            if match is None:
                lastStatementWasVar = False
                haveLine = False
                continue

            # It might be a function definition, in which case we continue
            expression = match.group('expression')

            if expression:
                functionMatch = self.FUNCTION_RE.match(expression)

                if functionMatch:
                    lastStatementWasVar = False
                    haveLine = False
                    continue

            # Now we have the start of a variable block
            if self.verbose:
                print u'%d: VAR BLOCK' % self.lineNum

            varLineNum = self.lineNum
            varLine = self.line

            haveLine, isSingleVar = self.var_block(match)

            if self.verbose:
                print u'%d: END VAR BLOCK:' % self.lineNum,

                if isSingleVar:
                    print u'SINGLE'
                else:
                    print u'MULTIPLE'

            if lastStatementWasVar and self.varDeclarations != self.VAR_DECLARATIONS_NONE:
                if (self.varDeclarations == self.VAR_DECLARATIONS_SINGLE and lastVarWasSingle and isSingleVar) or \
                   (self.varDeclarations == self.VAR_DECLARATIONS_STRICT and (lastVarWasSingle or isSingleVar)):
                    self.error('consecutive var declarations', lineNum=varLineNum, line=varLine)

            lastStatementWasVar = True
            lastVarWasSingle = isSingleVar


    def run_file_checks(self):
        for check in self.fileChecklist:
            self.sourcefile.seek(0)
            self.lineNum = 0

            if self.verbose:
                print u'%s: %s' % (check['title'], self.sourcefile.name)

            check['action']()


    def lint(self, filename):
        self.filename = unicode(filename, 'utf-8')
        fullpath = os.path.join(os.getcwd(), filename)

        try:
            with open(fullpath) as self.sourcefile:
                self.run_file_checks()

        except IOError, ex:
            self.lineNum = 0
            self.line = None
            self.error('file not found')

        except StopIteration:
            if self.verbose:
                print u'EOF\n'
            pass


    def error(self, message, **kwargs):
        line = kwargs.get('line', self.line)
        lineNum = kwargs.get('lineNum', self.lineNum)
        self.errors.append(
        {
            'filename':  self.filename,
            'lineNum':   lineNum,
            'line':      tabs2spaces(line),
            'message':   message,
            'positions': kwargs.get('positions'),
        })


    def errors(self):
        return self.errors


    def has_errors(self):
        return len(self.errors) != 0


    def print_errors(self, format='text'):
        if not self.errors:
            return

        if format == 'text':
            self.print_text_errors()
        elif format == 'textmate':
            self.print_textmate_errors()

    def print_text_errors(self):
        for error in self.errors:
            if 'lineNum' in error and 'line' in error:
                print Template(u'$filename:$lineNum: $message.\n+$line').substitute(error).encode('utf-8')

                if error.get('positions'):
                    markers = ' ' * len(error['line'])

                    for position in error['positions']:
                        markers = markers[:position] + '^' + markers[position + 1:]

                    # Add a space at the beginning of the markers to account for the '+' at the beginning
                    # of the source line.
                    print ' ' + markers
            else:
                print '%s: %s.' % (filename, error)

            print

    def print_textmate_errors(self):
        print """
<html>
    <head>
        <title>Cappuccino Lint Report</title>
    </head>
    <body>"""

        for error in self.errors:
            print "<div>"

            if 'lineNum' in error and 'line' in error:
                first_column = (error.get('positions') or [0])[0]
                error['filename'], error['lineNum'], first_column, error['message'], error['line']
                abs_filename = os.path.abspath(error['filename'])
                print '<a href="txmt://open/?url=file://%s&line=%d&column=%d">%s:%d:</a> %s.<br/><pre>+ %s</pre>' % (cgi.escape(abs_filename), error['lineNum'], first_column + 1, cgi.escape(error['filename']), error['lineNum'], cgi.escape(error['message']), cgi.escape(error['line']))

                if error.get('positions'):
                    print "</div><div><pre>",
                    markers = ' ' * len(error['line'])

                    for position in error['positions']:
                        markers = markers[:position] + '^' + markers[position + 1:]

                    # Add a space at the beginning of the markers to account for the '+' at the beginning
                    # of the source line.
                    print ' ' + markers

                    print "</pre>"
            else:
                print format_err(filename, error)

            print "</div>"

        print """
    </body>
</html>"""

if __name__ == '__main__':
    usage = 'usage: %prog [options] [file ... | -]'
    parser = OptionParser(usage=usage, version='1.02')
    parser.add_option('--var-declarations', action='store', type='string', dest='var_declarations', default='single', help='set the policy for flagging consecutive var declarations')
    parser.add_option('-v', '--verbose', action='store_true', dest='verbose', default=False, help='show what lint is doing')
    parser.add_option('-q', '--quiet', action='store_true', dest='quiet', default=False, help='do not display errors, only return an exit code')
    parser.add_option('-f', '--format', action='store', type='string', dest='format', default='text', help='the format to use for the report: text (default) or textmate (HTML in which errors can be clicked on to view in TextMate)')
    (options, args) = parser.parse_args()

    if options.var_declarations not in LintChecker.VAR_DECLARATIONS:
        parser.error('--var-declarations must be one of [' + ', '.join(LintChecker.VAR_DECLARATIONS) + ']')

    if options.verbose and options.quiet:
        parser.error('options -v/--verbose and -q/--quiet are mutually exclusive')

    options.format = options.format.lower()
    if not options.format in ('text', 'textmate'):
        parser.error('format must be either text or textmate')

    # We accept a list of filenames (relative to the cwd) either from the command line or from stdin
    filenames = args

    if args and args[0] == '-':
        filenames = [name.rstrip() for name in sys.stdin.readlines()]

    if not filenames:
        print usage.replace('%prog', os.path.basename(sys.argv[0]))
        sys.exit(0)

    checker = LintChecker(var_declarations=LintChecker.VAR_DECLARATIONS.index(options.var_declarations), verbose=options.verbose)

    for filename in filenames:
        if filename.endswith('.j'):
            checker.lint(filename)

    if checker.has_errors():
        if not options.quiet:
            checker.print_errors(options.format)
        sys.exit(1)
    else:
        sys.exit(0)

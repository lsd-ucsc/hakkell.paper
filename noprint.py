import re
import sys

x = sys.stdin.read()

pat = re.compile(r'''
\b                      # word boundary before
(putstr|putstrln|print) # print function
\b                      # word boundary after
\s+         # taking an argument
(           # argument option 1
    "           # begin string
    [^"]*
    "           # end string
|           # argument option 2
    \(          # begin parenthesized arg
    [^)]+
    \)          # end parenthesized arg
)
''', re.MULTILINE | re.VERBOSE | re.IGNORECASE)

matches = re.findall(pat, x)
print(len(matches), 'matches', file=sys.stderr)
for m in matches:
    print(' '.join(m), file=sys.stderr)

special_cases = [
        ('>>= print', '>> return ()'),
        ]
for case in special_cases:
    x = x.replace(*case)

sys.stdout.write(re.sub(pat, 'pure ()', x))

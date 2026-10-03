## Known failures

A test that we know we do not yet pass has, alongside its `.txt` file, an
`.expected` file containing the output we currently produce (which differs
from the test's own `RESULT` section).  Such a test counts as an expected
failure only if its output still matches that file exactly; any other
output is an unexpected failure and fails the suite.  A test that passes
but still has an `.expected` file is reported as an unexpected pass, and
the file should be deleted.

By default only unexpected results are reported.  Pass `--verbose` to also
report warnings, passes, and expected failures with their diffs.

`cabal test --test-options=--accept` rewrites the `.expected` file of every
failing test with its current output and removes the file of every test
that now passes.  Review the resulting diff before committing it.

## Notes on failures

### test/csl/bugreports_UnisaHarvardInitialization.txt

The expected output here includes a trailing space, which we delete.

### test/csl/number_PlainHyphenOrEnDashAlwaysPlural.txt

citeproc-js uses some heuristics to identify plurals,
but they aren't part of the spec and aren't entirely reliable.
"The logic will only set plurals where there is a numeric unit
on either side of a hyphen or en-dash. Numeric units are strings
ending in a number, or alphabetic strings consisting entirely of
characters appropriate to a roman numeral."  This won't catch
4a-5a or IIa-VIb.

### test/csl/variables_TitleShortOnShortTitleNoTitleCondition.txt

This test is contrary to the spec.  The whole group should
be suppressed because it contains variables but none are
called. See https://github.com/citation-style-language/test-suite/issues/29


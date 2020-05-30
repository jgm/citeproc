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


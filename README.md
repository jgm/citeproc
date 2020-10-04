# citeproc

[![BSD2 license](https://img.shields.io/badge/license-BSD2-blue.svg)](LICENSE)
[![CI
tests](https://github.com/jgm/citeproc/workflows/CI%20tests/badge.svg)](https://github.com/jgm/citeproc/actions)

<!--
[![Hackage](https://img.shields.io/hackage/v/citeproc.svg)](https://hackage.haskell.org/package/citeproc)
[![Stackage Lts](http://stackage.org/package/citeproc/badge/lts)](http://stackage.org/lts/package/citeproc)
[![Stackage Nightly](http://stackage.org/package/citeproc/badge/nightly)](http://stackage.org/nightly/package/citeproc)
-->

This library generates citations and bibliography formatted
according to a [CSL] style.  Currently version 1.0.2 of the CSL
spec is targeted.

This library is a successor to pandoc-citeproc, which was a fork
of Andrea Rossato's citeproc-hs.  I always found it difficult to
fix bugs in pandoc-citeproc and decided that implementing
citeproc from scratch would give me a better basis for
understanding.  This library has a number of other advantages
over pandoc-citeproc:

- it is much faster (as a rough benchmark, running the CSL
  test suite takes less than 4 seconds with this library,
  compared to 12 seconds with pandoc-citeproc)

- it interprets CSL more faithfully, passing more of the CSL
  tests

- it has fewer dependencies (in particular, it does not depend
  on pandoc)

- it is more flexible, not being tied to pandoc's types.

Unlike pandoc-citeproc, this library does not provide an
executable.  It will be used in pandoc itself to provide
integrated citation support and bibliography format conversion
(so the pandoc-citeproc filter will no longer be necessary).

[CSL]: https://docs.citationstyles.org/en/stable/specification.html

## How to use it

The main point of entry is the function `citeproc` from the
module `Citeproc`.  This takes as arguments:

- a `CiteprocOptions` structure (which currently just allows you
  to set whether citations are hyperlinked to the bibliography)

- a `Style`, which you will want to produce by parsing a CSL
  style file using `parseStyle` from `Citeproc.Style`.

- Optionally a `Lang`, which allows you to override a default locale,

- a list of `Reference`s, which you can produce from a CSL JSON
  bibliography using aeson's `decode`,

- a list of `Citation`s (each of which may have multiple
  `CitationItems`).

It yields a `Result`, which includes a list of formatted
citations and a formatted bibliography, as well any warnings
produced in evaluating the style.

The types are parameterized on a `CiteprocOutput` instance `a`,
which represents formatted content in your bibliographic
fields (e.g. the title).  If you want a classic CSL processor,
you can use `CslJson Text`.  But you can also use another type,
such as a pandoc `Inlines`.  All you need to do is define
an instance of `CiteprocOutput` for your type.

The signature of `parseStyle` may not be self-evident:
the first argument is a function that takes a URL and
retrieves the text from that URL.  This is used to fetch
the "indendent parent" of a dependent style.  You can supply
whatever function you like: it can search your local file
system or fetch the content via HTTP.  If you're not using
dependent styles, you can get by with `\_ -> return mempty`.

## The citeproc executable

If the package is compiled with the `executable` flag, an
executable `citeproc` will be built.  `citeproc` reads
a JSON-encoded `Inputs` object from `stdin` (or from
a file if a filename is provided) and writes
a JSON-encoded `Result` object to `stdout`.  This executable
can be used to add citation processing to non-Haskell projects.

`citeproc --help` will summarize usage information.

The input JSON should have the structure:

``` json
{ "citations":     [ ...list of citations... ],
  "references":    [ ...list of references... ],
  "style":         "<style>...</style>",
  "abbreviations": { ...abbreviations... },
  "lang":          "fr-FR" }
```

None of these fields is mandatory.  Instead of providing
`references` in the input JSON, one can specify a file
containing a CSL JSON bibliography, using the `--references`
option on the command line.  Instead of providing a CSL
stylesheet in the JSON, one can specify a file using
the `--style` option.  `--abbreviations` and `--lang`
may also be used on the command line to specify an
abbreviations file or a locale.  (Command-line options
always override any values given in the JSON file.)

A citation is structured like this:

``` json
{ "citationID": "foo",
  "citationItems": [ ...list of citationItems... ],
  "citationNoteNumber": 3 }
```

Only `citationItems` is necessary.  And, instead of

``` json
{ "citationItems": [ ... ] }
```

one can just specify an array of items directly:

``` json
[ ... ]
```

A citation item is structured like this:

``` json
{ "id":       "foo",
  "type":     "suppress-author",
  "label":    "page",
  "locator":  "45",
  "prefix":   "see ",
  "suffix":   " and others" }
```

Only `id` is mandatory.  If `type` is omitted, it will
be assumed to be `normal-cite` (other values are
`suppress-author` and `author-only`).

A reference is structured like this:

``` json
{
  "author": [
    {
      "family": "Aristotle"
    }
  ],
  "id": "aristotle:prior",
  "issued": {
    "date-parts": [
      [
        1989
      ]
    ]
  },
  "publisher": "Hackett",
  "publisher-place": "Indianapolis",
  "title": "Prior analytics",
  "translator": [
    {
      "family": "Smith",
      "given": "Robin"
    }
  ],
  "type": "book"
}
```

An abbreviations object has this form:

``` json
{ "default": {
    "container-title": {
            "Lloyd's Law Reports": "Lloyd's Rep",
            "Estates Gazette": "EG",
            "Scots Law Times": "SLT"
    }
  }
}
```

The abbreviations will be substituted in the output
only when the `form` attribute for the variable is
set to `short`.

The output JSON will have the structure:

``` json
{ "citations":    [ ...list of strings... ],
  "bibliography": [ ...list of arrays: item id and a string... ],
  "warnings":     [ ...list of warnings... ]
}
```

The contents of the entries will be HTML by default, but
if `--format=json` is specified, it will be a structured
representation of formatted text, e.g.

``` json
[
  "———. 1983b. “The Concept of Truth in Formalized Languages.” In ",
  {
    "format": "italics",
    "contents": [
      "Logic, Semantics, Metamathematics"
    ]
  },
  ", edited by John Corcoran, 152–278. Indianapolis: Hackett."
]
```

This representation can be used if you want to convert the
result to a format other than HTML.

## Known bugs and limitations

At this point, the library still fails some of the tests from the
CSL test suite (59/845).  However, most of the failures are on
minor corner cases.  This library is already much more accurate
in implementing the CSL spec than pandoc-citeproc was.


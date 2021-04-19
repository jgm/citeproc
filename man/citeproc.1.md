---
title: citeproc
section: 1
...

# NAME

citeproc - process citations using a CSL stylesheet.

# SYNOPSIS

`citeproc [options] [file]`

# DESCRIPTION

`citeproc` reads a JSON-encoded `Inputs` object from `stdin` (or
from a file if a filename is provided) and writes a JSON-encoded
`Result` object to `stdout`.  This executable can be used to add
citation processing to non-Haskell projects.


# OPTIONS

`-s` *FILE*, `--style=`*FILE*
:   Specify a CSL style to be used.

`-r` *FILE*, `--references=`*FILE*
:   Specify a CSL JSON bibliography to be used as a source for references.

`a` *FILE*, `--abbreviations=`*FILE*
:   Specify a CSL abbreviations file.

`l` *LANG*, `--lang=`*LANG*
:   Specify a locale to override the style's default.
    A BCP 47 language tag is expected:  for example, `en`,
    `de`, `en-US`, `fr-CA`, `ug-Cyrl`.  The unicode extension
    syntax (after `-u-`) may be used to specify options for
    collation. Here are some examples:

    - `zh-u-co-pinyin` -- Chinese with the Pinyin collation.
    - `es-u-co-trad` -- Spanish with the traditional collation
      (with `Ch` sorting after `C`).
    - `fr-u-kb` -- French with "backwards" accent sorting
      (with `coté` sorting after `côte`).
    - `en-US-u-kf-upper` -- English with uppercase letters sorting
       before lower (default is lower before upper).

`f` *html|json*, `--format=`*html|json*
:   Specify the format to be used for the entries.  `html` (the
    default) uses HTML tags and entities for formatting.  `json`
    creates a structured JSON representation of a formatted document.

`-h, --help`
:   Print usage information.

`-V, --version`
:   Print version.

# NOTES

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

Only `citationItems` is necessary.  Alternatively,
instead of

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
only when the `form` attribute for the style element that
renders the variable is set to `short`.

The output JSON will have the structure:

``` json
{ "citations":    [ ...list of strings... ],
  "bibliography": [ ...list of arrays: item id and a string... ],
  "warnings":     [ ...list of warnings... ]
}
```

The contents of the entries will be HTML by default, but
if `--format=json` is specified, it will be a structured
representation of formatted text.

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

# AUTHORS

John MacFarlane


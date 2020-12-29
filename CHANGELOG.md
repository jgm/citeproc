# citeproc changelog

## 0.3.0.3

  * Fix author-only citations (#43).  We got bad results with some
    styles when a reference had both an author and a translator.

## 0.3.0.2

  * Don't use cite-group delimiter if ANY citation in group has
    locator (#38).  This seems to be citeproc.js's behavior and it gives
    better results for chicago-author-date:  we want both
    `[@foo20; @foo21, p. 3]` and `[@foo20, p. 3; @foo21]` to produce
    a semicolon separator, rather than a comma.

## 0.3.0.1

  * Better handle `initialize-with` that ends in a nonbreaking space.
    In this case, citeproc should not add an additional space
    or strip the nonbreaking space.  Closes #37.


## 0.3

  * Change `makeReferenceMap` to return a cleaned-up list of
    references as well as a reference map.  The cleanup-up list
    removes references with duplicate ids.  When there are multiple
    references with the same id, the last one is included and
    the others discarded.  [API change]

## 0.2.0.1

  * FromJSON for Name: make straight quotes curly.
    Otherwise nothing will do this, when we are decoding
    JSON to (Reference a), a /= CslJson Text.
  * Remove redundant pragmas and imports (Albert Krewinkel).
  * Use custom prelude with GHC 8.6.* and older (Albert
    Krewinkel).  This adds support for GHC 8.0.x.

## 0.2

  * Remove `AfterOtherPunctuation` constructor from
    `CaseTransformState` [API change].
    This gave bad results with things like parentheses (#27).
  * Change `SortKeyValue` to include `Maybe Lang` [API change].
    This allows us to do locale-sensitive sorting (though this
    won't matter much unless the `icu` flag is used).
  * Add `Maybe Lang` parameter on `initialize` (since
    capitalization can be locale-dependent).
  * Add cabal.project.icu for building with icu lib.
  * Add (unexported) Citeproc.Unicode compatibility module.
    This allows us to use the same functions whether or not
    the `icu` flag is used.

## 0.1.1.1

  * Pay attention to citationNoteNumber in computing position.
    In calculating whether an item is alone in its citation,
    we need to take into account citationNoteNumber, since
    two citations may occur in the same note and they should
    not be ranked "alone." See jgm/pandoc#6813,
    citation-style-language/documentation#121

## 0.1.1

  * Ensure that uncited references are sorted last
    when it comes to assigning citation numbers (#22).
  * Remove "capitalize initial term" feature.  This is required by
    the test suite but not the spec.  It makes more sense for us to do
    this capitalization in the calling program, e.g. pandoc.  For some
    citations in note styles may already be in notes and thus not
    trigger separate footnotes.  If initial terms had been capitalized,
    we'd need to uncapitalize, and that is hard to do reliably.
  * Treat empty `FancyVal` as an empty value.
  * Derive Functor, Traversable, Foldable for Result [API change].

## 0.1.0.3

  * Better handling of author-only/suppress-author.
    Previously all results of "names" elements were treated
    as authors.  But only the first should be (generally this
    is the author, but it could be the editor of an edited
    volume with no author).  See jgm/pandoc#6765.

## 0.1.0.2

  * Don't enclose contents of e:choose in a Formatted element (#19).
    The e:choose element is "transparent" and the delimiter
    controlling its formatting should be inserted between
    the items it returns.

## 0.1.0.1

  * Fix sorting when no `<sorting>` element given. The spec says:
    "In the absence of cs:sort, cites and bibliographic entries appear in
    the order in which they are cited." This affects IEEE in particular.  See
    jgm/pandoc#6741.

  * Improve `sameNames` and citation grouping.  Preivously if a citation
    item had a prefix, it would not be grouped with following citations.
    See jgm/pandoc#6722 for discussion.

  * Remove unneeded `hasNoSuffix` check in `sameNames`.

  * Remove unneeded import

  * `citeproc` executable: strip BOM before parsing style (#18).

## 0.1

  * Initial release.


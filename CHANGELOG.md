# citeproc changelog

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


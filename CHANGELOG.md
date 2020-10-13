# citeproc changelog

## 0.1

* Initial release.

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


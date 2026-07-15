## Reason for this submission

This is an urgent maintenance release requested by Yves Rosseel (lavaan
maintainer). A new lavaan feature (`optim.fix.saturated`, default `TRUE` in
the upcoming lavaan 0.7-1) causes 4 of mlstats' `method = "sem"` tests to
fail, because those tests depended on lavaan converging on an inadmissible
(out-of-range) standardized solution for certain degenerate/small-sample
models. lavaan 0.7-1 now converges on an admissible solution instead for
those same models. This is currently blocking lavaan 0.7-1's own submission
to CRAN. The relevant tests have been adjusted accordingly (see NEWS.md);
no user-facing behavior of mlstats itself has changed. The package's test
suite passes with both lavaan 0.6-21 (current CRAN release) and lavaan
0.7-1 (verified locally against the pre-release version from the
maintainer's r-universe).

This release also bundles a handful of unrelated bug fixes and
documentation/output improvements accumulated since 0.1.0 (see NEWS.md).

## R CMD check results

0 errors | 0 warnings | 2 notes

* `checking CRAN incoming feasibility ... NOTE`: "Days since last update: 4".
  Expected, and the reason for the rush is explained above.
* `checking for future file timestamps ... NOTE`: "unable to verify current
  time". This is a network-reachability artifact of the local check
  environment (it cannot reach the time-verification service) and is
  unrelated to package content; it also appeared on the 0.1.0 submission.

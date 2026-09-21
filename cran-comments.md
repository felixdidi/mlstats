## Reason for this submission

This is a minor feature release. `mldesc()` and `within_between_correlations()`
now report group sizes (number of groups, total observations, observations
per group) in a note, and the bundled `media_diary` example dataset now has
unequal group sizes to illustrate this. See NEWS.md for details.

## R CMD check results

0 errors | 0 warnings | 1 note

* `checking for future file timestamps ... NOTE`: "unable to verify current
  time". This only occurs in the local check environment, which cannot reach
  the time-verification services (worldtimeapi.org / worldclockapi.com). It
  is unrelated to package content.

## Reverse dependencies

There are currently no reverse dependencies on CRAN.

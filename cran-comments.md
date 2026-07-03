## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes

`devtools::check(cran = TRUE)` reports "unable to verify current time" in
some local runs; this is a network-reachability artifact of the local check
environment (it cannot reach the time-verification service) and is unrelated
to package content.

# mlstats 0.1.0

* Initial CRAN release.

* Added `mldesc()` for computing multilevel descriptive statistics including
  means, standard deviations, within-group and between-group correlations,
  and intraclass correlation coefficients (ICCs).

* Added `within_between_correlations()` for computing decomposed within-group
  and between-group correlation matrices.

* Added `decompose_within_between()` for decomposing variables into within-group
  and between-group components for use in Random Effects Within-Between (REWB)
  models.

* Added `bayes_mldesc()` and `bayes_within_between_correlations()` for Bayesian
  estimation of multilevel descriptives and correlations via brms.

* Support for `gt` and `tinytable` table output formats via `print()` methods.

# mlstats 0.1.0

Initial CRAN release.

* `within_between_correlations()` computes within-group and between-group
  correlations for nested data (e.g., repeated measurements per person, or
  students nested within schools), using one of three methods: variance
  decomposition (default), two-level structural equation modeling (via
  `lavaan`), or Bayesian multilevel modeling (via `brms`).

* `mldesc()` creates publication-ready descriptive statistics tables that
  combine means, SDs, ranges, intraclass correlation coefficients (ICCs),
  and within-/between-group correlations in a single table.

* `decompose_within_between()` decomposes variables into within-group and
  between-group components for use in Random Effects Within-Between (REWB)
  models.

* Result tables print as tibbles by default and can be exported as `gt` or
  `tinytable` objects via `print(result, format = "gt")` or
  `print(result, format = "tt")`.

* Includes the `media_diary` example dataset, a simulated daily-diary study
  used throughout the documentation and vignettes to illustrate within-person
  vs. between-person relationships.

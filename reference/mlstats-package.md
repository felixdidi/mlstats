# mlstats: Multilevel Descriptive Statistics and Data Preparation

The **mlstats** package provides tools for multilevel descriptive
statistics and data preparation (e.g., repeated measurements per person,
or students nested within schools). It supports:

- Computing within-group and between-group correlations
  ([`within_between_correlations()`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md))

- Creating publication-ready descriptive statistics tables with ICCs and
  within-/between-group correlations
  ([`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md))

- Decomposing variables into within-group and between-group components
  for Random Effects Within-Between (REWB) models
  ([`decompose_within_between()`](https://felixdidi.github.io/mlstats/reference/decompose_within_between.md))

- Three estimation methods, selectable via the `method` argument of
  [`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md)
  and
  [`within_between_correlations()`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md):
  variance decomposition (default), two-level structural equation
  modeling (via lavaan), and Bayesian multilevel modeling (via brms)

- Exporting result tables as 'gt' or 'tinytable' objects via
  `print(result, format = "gt")` or `print(result, format = "tt")`

See
[`vignette("correlation-methods")`](https://felixdidi.github.io/mlstats/articles/correlation-methods.md)
for the statistical background, and
[media_diary](https://felixdidi.github.io/mlstats/reference/media_diary.md)
for an example dataset.

## See also

Useful links:

- <https://felixdidi.github.io/mlstats/>

- <https://github.com/felixdidi/mlstats>

- Report bugs at <https://github.com/felixdidi/mlstats/issues>

## Author

**Maintainer**: Felix Dietrich <mail@felix-dietrich.de> \[copyright
holder\]

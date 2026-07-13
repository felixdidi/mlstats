# Changelog

## mlstats 0.1.0.9000 (development version)

- [`decompose_within_between()`](https://felixdidi.github.io/mlstats/reference/decompose_within_between.md)
  now defaults to `components = c("between", "within")`, so
  grand-mean-centered scores are no longer returned by default. Grand
  mean centering is rarely needed for REWB models (the within and
  between components are the actual predictors), so this avoids adding
  an extra column most callers don’t use. Pass
  `components = c("gmc", "between", "within")` (or any subset including
  `"gmc"`) to restore the previous output.

- Fixed `print(..., format = "tt")` on
  [`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md)
  output so that the *N*_(obs) column header renders “obs” as a proper
  subscript in Word/docx output, not just HTML. The header markup used a
  raw HTML `<sub>obs</sub>` tag, which `tinytable` silently drops when
  going through its markdown-to-docx (Pandoc) conversion path; it is now
  written with `tinytable`’s markdown subscript syntax (`~obs~`),
  consistent with the markdown italics (`*N*`) already used on the same
  label and with how the “a”/“b” correlation-note superscripts are
  marked elsewhere in the table.

  - [`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md)
    now reports the observed minimum and maximum in the `range` column
    with two decimals instead of rounding them to whole numbers
    (decimals are dropped when both the minimum and the maximum are
    whole numbers, e.g., for integer scales). Previously, a variable
    observed between 1.5 and 6.9 was reported as “2–7”, wrongly implying
    that the scale endpoints were observed.

- [`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md)
  now counts variables that are constant within every group (e.g., a
  trait measured once per person but repeated across that person’s rows)
  once per group in `n_obs`, reporting the number of groups that
  provided a value instead of the number of rows the value was
  replicated across.

- [`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md),
  [`within_between_correlations()`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md),
  and
  [`decompose_within_between()`](https://felixdidi.github.io/mlstats/reference/decompose_within_between.md)
  now error informatively when a variable in `vars` contains only
  missing values (previously an uninformative low-level error could be
  triggered).

- Observations with a missing value on the grouping variable are no
  longer silently treated as a group of their own.
  [`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md)
  and
  [`within_between_correlations()`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
  now warn and exclude them (previously they formed a spurious extra
  group in the correlations while being dropped from the ICC models);
  [`decompose_within_between()`](https://felixdidi.github.io/mlstats/reference/decompose_within_between.md)
  keeps the rows but sets their between- and within-group components to
  `NA`, with a warning.

## mlstats 0.1.0

CRAN release: 2026-07-11

Initial CRAN release.

- [`within_between_correlations()`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
  computes within-group and between-group correlations for nested data
  (e.g., repeated measurements per person, or students nested within
  schools), using one of three methods: variance decomposition
  (default), two-level structural equation modeling (via `lavaan`), or
  Bayesian multilevel modeling (via `brms`).

- [`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md)
  creates publication-ready descriptive statistics tables that combine
  means, SDs, ranges, intraclass correlation coefficients (ICCs), and
  within-/between-group correlations in a single table.

- [`decompose_within_between()`](https://felixdidi.github.io/mlstats/reference/decompose_within_between.md)
  decomposes variables into within-group and between-group components
  for use in Random Effects Within-Between (REWB) models.

- Result tables print as tibbles by default and can be exported as `gt`
  or `tinytable` objects via `print(result, format = "gt")` or
  `print(result, format = "tt")`.

- Includes the `media_diary` example dataset, a simulated daily-diary
  study used throughout the documentation and vignettes to illustrate
  within-person vs. between-person relationships.

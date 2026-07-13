# Changelog

## mlstats (development version)

## mlstats 0.1.1

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

# Decompose Variables into Within-Group and Between-Group Components

This function performs a multilevel decomposition of variables by
computing:

- Grand mean centered scores (deviations from overall mean)

- Between-group scores (group means)

- Within-group scores (deviations from group means)

## Usage

``` r
decompose_within_between(
  data,
  group,
  vars,
  components = c("gmc", "between", "within"),
  gmc_pattern = "{col}_grand_mean_centered",
  between_pattern = "{col}_between_{group}",
  within_pattern = "{col}_within_{group}"
)
```

## Arguments

- data:

  A data frame containing the variables to decompose.

- group:

  A character string specifying the name of the grouping variable.

- vars:

  A character vector specifying the names of variables to decompose.

- components:

  A character vector specifying which components to compute. Any subset
  of `c("gmc", "between", "within")` (default: all three). `"gmc"` =
  grand mean centering, `"between"` = group means, `"within"` =
  within-group deviations. If `"within"` is requested without
  `"between"`, the between component is computed internally as an
  intermediate step and not included in the output.

- gmc_pattern:

  A glue-style naming pattern for grand-mean-centered columns. Use
  `{col}` for the variable name. Default: `"{col}_grand_mean_centered"`.

- between_pattern:

  A glue-style naming pattern for between-group (group mean) columns.
  Use `{col}` for the variable name and `{group}` for the grouping
  variable name. Default: `"{col}_between_{group}"`.

- within_pattern:

  A glue-style naming pattern for within-group deviation columns. Use
  `{col}` for the variable name and `{group}` for the grouping variable
  name. Default: `"{col}_within_{group}"`.

## Value

A data frame containing:

- All original variables from `data`

- Grand mean centered versions (named by `gmc_pattern`), if `"gmc"` in
  `components`

- Between-group means (named by `between_pattern`), if `"between"` in
  `components`

- Within-group deviations (named by `within_pattern`), if `"within"` in
  `components`

## Details

This decomposition is commonly used in multilevel modeling to separate
within-group and between-group variance components (Enders & Tofighi,
2007). The decomposed variables are particularly useful for Random
Effects Within-Between (REWB) models (Bell et al., 2019), which allow
the estimation of distinct within-group and between-group effects.

The function performs three centering operations:

**1. Grand mean centering:** Each value is expressed as a deviation from
the overall sample mean. This centers the entire distribution at zero.

**2. Between-group component:** For each observation, this equals the
mean of their group. These values are constant within groups and vary
between groups. In REWB models, this represents the between-group effect
of the predictor.

**3. Within-group component:** Each value is expressed as a deviation
from their group mean. This removes all between-group variance and
represents the within-group effect of the predictor in REWB models.

## References

Bell, A., Fairbrother, M., & Jones, K. (2019). Fixed and random effects
models: making an informed choice. *Quality & Quantity*, 53(2),
1051-1074.

Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in
cross-sectional multilevel models: A new look at an old issue.
*Psychological Methods*, 12(2), 121-138.

## See also

[`within_between_correlations`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md),
which uses this function internally to perform the within/between
decomposition.

## Examples

``` r
data("media_diary")

# Decompose all three components (default)
result <- decompose_within_between(
  data = media_diary,
  group = "person",
  vars = c("stress", "screen_time")
)

# Only between and within (no grand mean centering)
result_wb <- decompose_within_between(
  data = media_diary,
  group = "person",
  vars = c("stress", "screen_time"),
  components = c("between", "within")
)

# Custom column naming: flat suffixes without the group name
result_flat <- decompose_within_between(
  data = media_diary,
  group = "person",
  vars = c("stress", "screen_time"),
  components = c("between", "within"),
  between_pattern = "{col}_between",
  within_pattern = "{col}_within"
)
```

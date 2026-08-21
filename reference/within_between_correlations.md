# Compute Within-Group and Between-Group Correlations

In data with a grouping structure (e.g., repeated measurements per
person, or students nested within schools), a single correlation between
two variables can be misleading, because it mixes two different
relationships: how the variables relate *within* each group (e.g., do a
person's good days also tend to be their productive days?), and how they
relate *between* groups (e.g., do people who are generally happier also
tend to be generally more productive?). This function estimates both
relationships separately, using one of three methods (see Details and
[`vignette("correlation-methods")`](https://felixdidi.github.io/mlstats/articles/correlation-methods.md)
for the full statistical background).

## Usage

``` r
within_between_correlations(
  data,
  group,
  vars,
  method = c("decomposition", "sem", "bayes"),
  weight = TRUE,
  flip = FALSE,
  significance = c("basic", "detailed"),
  ci = 0.9,
  folder = NULL
)
```

## Arguments

- data:

  A data frame containing the variables to analyze.

- group:

  A character string specifying the name of the grouping variable.

- vars:

  A character vector specifying the names of variables to correlate.

- method:

  Character string specifying the estimation method: `"decomposition"`
  (default), `"sem"`, or `"bayes"`. See Details.

- weight:

  Logical. Used when `method = "decomposition"` or `method = "bayes"`.
  If TRUE (default), the between-group correlation gives more weight to
  larger groups; significance/credible intervals, however, are always
  based on the unweighted correlation of group means. If FALSE, every
  group counts equally regardless of size. Ignored (with a message) when
  `method = "sem"`, because that method handles unequal group sizes
  automatically.

- flip:

  Logical. If TRUE, between-group correlations are shown in the upper
  triangle and within-group correlations in the lower triangle. Default
  is FALSE.

- significance:

  Character string specifying the significance marking style. Either
  "basic" (default) or "detailed". If "basic", correlations with p \<
  .05 are marked with a star. If "detailed", correlations are marked
  with 1-3 stars for p \< .05, p \< .01, or p \< .001, respectively.
  Ignored (with a message) when `method = "bayes"`, which always marks
  correlations whose credible interval (see `ci`) excludes zero with a
  single star.

- ci:

  Numeric value strictly between 0 and 1 specifying the credible
  interval width used to decide whether a correlation is starred. Only
  applicable when `method = "bayes"`; default is 0.9 (90% CI). Ignored
  (with a message) for other methods.

- folder:

  Character string specifying the directory path where `brms` models
  should be saved. Required when `method = "bayes"`; ignored (with a
  message) otherwise. Default is `NULL`.

## Value

A tibble containing a correlation matrix where:

- The upper triangle contains within-group correlations

- The lower triangle contains between-group correlations

- Diagonal elements are marked with "–"

- Significant correlations are marked with asterisks (see `significance`
  parameter, or `ci` when `method = "bayes"`)

The tibble can be returned as a gt object using
`print(result, format = "gt")` and as a tinytable object using
`print(result, format = "tt")`.

## Details

**Method `"decomposition"`** (the default) computes the within-group
correlation by first subtracting each group's mean from every
observation, then correlating the resulting deviation scores. It
computes the between-group correlation by correlating the group means
with one another (optionally weighted by group size; see `weight`). This
approach follows Pedhazur (1997, ch. 16), and the significance tests
account for the fact that subtracting group means uses up degrees of
freedom, following the general testing principle in Snijders and Bosker
(2012, sec. 6.1). This method is fast and easy to interpret, and works
well for most data sets, but is less suited to data with very unequal
group sizes.

**Method `"sem"`** fits a two-level structural equation model (via
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)) that
estimates the within-group and between-group covariance matrices
simultaneously using maximum likelihood. Significance is based on the
resulting z-tests. Because groups are weighted implicitly through
maximum likelihood estimation rather than through the `weight` argument,
this method is the more principled choice for data with very unequal
group sizes. It is slower than `"decomposition"` and can occasionally
fail to converge for small or collinear data sets.

For `method = "sem"`, variables that never vary within a group (e.g.,
time-invariant traits) are modeled only at the between-group level, and
variables with almost no between-group variance (intraclass correlation
near zero) are modeled only at the within-group level; the corresponding
cells of the unused level are reported as `NA`.

**Method `"bayes"`** mirrors `"decomposition"`, but estimates both
correlations via Bayesian multivariate models fit with
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)
(requires the brms package) instead of closed-form formulas, reporting
posterior medians and credible intervals (via `ci`) in place of point
estimates and p-values. It requires a `folder` argument to cache fitted
models, can take considerably longer than the other two methods, and is
most useful when the number of groups is small or when communicating
uncertainty via credible intervals is a priority. See
[`vignette("correlation-methods")`](https://felixdidi.github.io/mlstats/articles/correlation-methods.md)
for details on the number of models fit and caching behavior.

## References

Bürkner, P.-C. (2017). brms: An R package for Bayesian multilevel models
using Stan. *Journal of Statistical Software, 80*(1), 1–28.
[doi:10.18637/jss.v080.i01](https://doi.org/10.18637/jss.v080.i01)

Hox, J., Moerbeek, M., & van de Schoot, R. (2018). *Multilevel analysis:
Techniques and applications* (3rd ed.). Routledge.

Pedhazur, E. J. (1997). *Multiple regression in behavioral research:
Explanation and prediction*. Harcourt Brace.

Snijders, T. A. B., & Bosker, R. J. (2012). *Multilevel analysis: An
introduction to basic and advanced multilevel modeling* (2nd ed.). Sage
Publishers.

## See also

[`mldesc`](https://felixdidi.github.io/mlstats/reference/mldesc.md),
which combines this function's output with descriptive statistics and
ICCs in a single table. See
[`vignette("correlation-methods")`](https://felixdidi.github.io/mlstats/articles/correlation-methods.md)
for a detailed statistical description of all three methods.

## Examples

``` r
data("media_diary")

# Compute weighted between-group correlations (default, decomposition method)
result_weighted <- within_between_correlations(
  data = media_diary,
  group = "person",
  vars = c("wellbeing", "screen_time")
)

# Compute unweighted between-group correlations
result_unweighted <- within_between_correlations(
  data = media_diary,
  group = "person",
  vars = c("wellbeing", "screen_time"),
  weight = FALSE
)

# Use SEM-based estimation (on similarly-scaled variables; SEM is
# sensitive to large scale differences, unlike "decomposition")
# \donttest{
result_sem <- within_between_correlations(
  data = media_diary,
  group = "person",
  vars = c("wellbeing", "stress"),
  method = "sem"
)
# }

# Use detailed significance marking
result_detailed <- within_between_correlations(
  data = media_diary,
  group = "person",
  vars = c("wellbeing", "screen_time"),
  significance = "detailed"
)

# Use Bayesian estimation (requires the brms package)
# \donttest{
result_bayes <- within_between_correlations(
  data = media_diary,
  group = "person",
  vars = c("wellbeing", "screen_time"),
  method = "bayes",
  folder = tempdir()
)
#> Error in .fun(model_code = .x1) : 
#>   Boost not found; call install.packages('BH')
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
# }
```

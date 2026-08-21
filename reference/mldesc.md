# Compute Multilevel Descriptive Statistics

Creates a publication-ready descriptive statistics table for multilevel
data (e.g., repeated measurements per person, or students nested within
schools). For each variable, the table reports basic descriptives, the
proportion of variance that lies between groups (the intraclass
correlation, ICC), and how each pair of variables relates both within
and between groups (see
[`within_between_correlations`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
and
[`vignette("correlation-methods")`](https://felixdidi.github.io/mlstats/articles/correlation-methods.md)
for the statistical background on the latter).

## Usage

``` r
mldesc(
  data,
  group,
  vars,
  method = c("decomposition", "sem", "bayes"),
  weight = TRUE,
  flip = FALSE,
  significance = c("basic", "detailed"),
  ci = 0.9,
  folder = NULL,
  remove_leading_zero = TRUE
)
```

## Arguments

- data:

  A data frame containing the variables to analyze.

- group:

  A character string specifying the name of the grouping variable.

- vars:

  A character vector specifying the names of variables to describe.

- method:

  Character string specifying the estimation method for correlations and
  the ICC: `"decomposition"` (default), `"sem"`, or `"bayes"`. See
  [`within_between_correlations`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
  for details on the correlation methods. With `method = "bayes"`, the
  ICC is also estimated with a Bayesian intercept-only model (via
  [`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html))
  instead of [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html),
  reporting the posterior median.

- weight:

  Logical. If TRUE (default), the mean and SD are calculated across all
  observations (so larger groups contribute more), and the between-group
  correlation gives more weight to larger groups. If FALSE, every group
  counts equally: the mean and SD are calculated on group means, and the
  between-group correlation is unweighted. For correlations, this is
  only used when `method = "decomposition"` or `method = "bayes"`.

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
  correlations whose credible interval excludes zero with a single star.

- ci:

  Numeric value strictly between 0 and 1 specifying the credible
  interval width used for the within-group and between-group
  correlations when `method = "bayes"`. Default is 0.9 (90% CI). The ICC
  always reports the posterior median only and is not affected by this
  argument. Ignored (with a message) for other methods.

- folder:

  Character string specifying the directory path where `brms` models
  should be saved. Required when `method = "bayes"`; ignored (with a
  message) otherwise. Default is `NULL`.

- remove_leading_zero:

  Logical. If TRUE (default), removes leading zeros from decimal values
  in correlation and ICC columns according to APA standards.

## Value

A tibble of class `mlstats_desc_tibble` containing:

- `variable`: Variable name

- `n_obs`: Number of observations. For variables that are constant
  within every group (e.g., a trait measured once per person but
  repeated across that person's rows), this is the number of groups that
  provided a value, not the number of rows it was replicated across.

- `m`: Mean (rounded to two decimals)

- `sd`: Standard deviation (rounded to two decimals)

- `range`: Range from observed minimum to maximum, rounded to two
  decimals unless both are whole numbers (e.g., integer scales), in
  which case decimals are dropped

- One column per variable in `vars` containing correlations

- `icc`: Intraclass correlation coefficient

The tibble can be returned as a gt object using
`print(result, format = "gt")` and as a tinytable object using
`print(result, format = "tt")`.

## Details

The function combines three types of information:

**Descriptive statistics:** Basic summary statistics for each variable.
When `weight = TRUE` (default), statistics are calculated across all
observations. When `weight = FALSE`, the mean is the mean of group
means, and the SD is the standard deviation of group means, representing
between-group variability.

**Correlations:** Within-group correlations (upper triangle) and
between-group correlations (lower triangle), computed using
[`within_between_correlations`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md).
See that function's documentation and the package vignette for how each
method estimates these correlations and tests them for significance.

**ICC:** The intraclass correlation coefficient, computed from an
unconditional (intercept-only) multilevel model using
[`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) (or
[`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html) when
`method = "bayes"`). The ICC represents the proportion of variance in
each variable that lies between groups, with values close to 1
indicating a variable that barely varies within groups (e.g., a stable
trait), and values close to 0 indicating a variable that barely varies
between groups (e.g., a fast-changing state).

The ICC is always computed from a linear (Gaussian) model, regardless of
a variable's measurement scale. For binary, ordinal, or count variables
this yields a linear-probability-style ICC rather than a latent-scale
ICC from a generalized linear mixed model. A warning is emitted if any
`vars` look binary, ordinal, or count-like (few, whole-number values).

With `method = "bayes"`, the function fits one `brms` model per variable
for the ICCs, plus all the models described in
[`within_between_correlations`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
for the correlations — for `p` variables, `p` ICC fits in addition to
the within/between-group correlation fits. This can take a long time for
larger numbers of variables; see
[`vignette("correlation-methods")`](https://felixdidi.github.io/mlstats/articles/correlation-methods.md)
for details.

## References

Bürkner, P.-C. (2017). brms: An R package for Bayesian multilevel models
using Stan. *Journal of Statistical Software, 80*(1), 1–28.
[doi:10.18637/jss.v080.i01](https://doi.org/10.18637/jss.v080.i01)

Pedhazur, E. J. (1997). *Multiple regression in behavioral research:
Explanation and prediction*. Harcourt Brace.

Snijders, T. A. B., & Bosker, R. J. (2012). *Multilevel analysis: An
introduction to basic and advanced multilevel modeling* (2nd ed.). Sage
Publishers.

## See also

[`within_between_correlations`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
for details on how within-group and between-group correlations are
estimated and tested.

## Examples

``` r
data("media_diary")
vars <- c("self_control", "wellbeing", "screen_time", "stress")

# Compute multilevel descriptives (default: decomposition method)
result <- mldesc(
  data = media_diary,
  group = "person",
  vars = vars
)

result
#> # Multilevel Descriptive Statistics
#>   ============ ===== ====== ===== ========= ===== ===== ===== ===== =====
#>   variable     n_obs      m    sd     range   `1`   `2`   `3`   `4`   icc
#>   ------------ ----- ------ ----- --------- ----- ----- ----- ----- -----
#> 1 Self control   100   3.93  0.70 2.40–6.20     –    NA    NA    NA  1.00
#> 2 Wellbeing    1,184   4.42  0.89 1.50–7.00  .47*     –  .42* -.40*   .50
#> 3 Screen time  1,184 132.05 39.01    15–246 -.66* -.32*     –  .29*   .34
#> 4 Stress       1,184   3.77  0.95       1–7 -.55* -.27*  .46*     –   .39
#>   ============ ===== ====== ===== ========= ===== ===== ===== ===== =====
#> # ℹ Within-person correlations above, between-person correlations below the
#> #   diagonal.
#> # ℹ All correlations marked with a star are significant at p < .05.
#> # ℹ Based on 100 persons and 1,184 observations (median 12 per person; range:
#> #   5–14).
#> # ℹ Correlations estimated via variance decomposition.
#> # ℹ Group-weighted multilevel descriptive statistics computed with mlstats.

# Compute with unweighted between-group correlations
result_unweighted <- mldesc(
  data = media_diary,
  group = "person",
  vars = vars,
  weight = FALSE
)

# Use SEM-based estimation for correlations (on similarly-scaled variables;
# SEM is sensitive to large scale differences, unlike "decomposition")
# \donttest{
result_sem <- mldesc(
  data = media_diary,
  group = "person",
  vars = c("self_control", "wellbeing", "stress"),
  method = "sem"
)
# }

# Use detailed significance marking
result_detailed <- mldesc(
  data = media_diary,
  group = "person",
  vars = vars,
  significance = "detailed"
)

# Use Bayesian estimation for correlations and the ICC (requires brms)
# \donttest{
result_bayes <- mldesc(
  data = media_diary,
  group = "person",
  vars = c("self_control", "wellbeing", "screen_time"),
  method = "bayes",
  folder = tempdir()
)
#> Error in .fun(model_code = .x1) : 
#>   Boost not found; call install.packages('BH')
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
# }
```

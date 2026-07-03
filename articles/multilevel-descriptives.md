# Multilevel Descriptive Statistics

``` r

library(mlstats)
library(dplyr)
```

This vignette covers
[`within_between_correlations()`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
and
[`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md) in
detail, including all available options and guidance on interpreting the
results. For a brief introduction to both functions see
[`vignette("mlstats")`](https://felixdidi.github.io/mlstats/articles/mlstats.md).

## Example Data

We use `media_diary`, a simulated daily diary dataset included with
**mlstats** (100 participants over 14 days; *N* = 100 persons, *T* =
1,400 daily observations). The variables are:

- **`person`**: person identifier
- **`self_control`**: trait self-control (stable between-person
  characteristic; ICC ≈ 1)
- **`wellbeing`**: daily positive wellbeing (1–7)
- **`screen_time`**: minutes of entertainment media consumed that day
- **`stress`**: daily perceived stress (1–7)
- **`enjoyment`**: enjoyment of the media watched that day (1–7)

``` r

data("media_diary")
vars <- c("self_control", "wellbeing", "screen_time", "stress")
```

## Why a Single Correlation Is Not Enough

When observations are nested within groups, the overall (pooled)
correlation between two variables is a weighted mix of two conceptually
different relationships: how variables co-vary within groups over time,
and whether groups that score higher on one variable also score higher
on the other. These two relationships can differ substantially in
magnitude or can even point in opposite directions.

## Within- and Between-Group Correlations

[`within_between_correlations()`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
returns a single correlation matrix in which within-group correlations
appear **above** the diagonal and between-group correlations appear
**below**. In our example dataset, observations are nested within
persons, so the function computes within-person correlations (upper
triangle) and between-person correlations (lower triangle).

``` r

within_between_correlations(
  data  = media_diary,
  group = "person",
  vars  = vars
)
#> # Within- and Between-Person Correlations
#>   ============ ====== ====== ===== ======
#>   variable        `1`    `2`   `3`    `4`
#>   ------------ ------ ------ ----- ------
#> 1 self_control      –     NA    NA     NA
#> 2 wellbeing     0.61*      – 0.42* -0.43*
#> 3 screen_time  -0.67* -0.34*     –  0.29*
#> 4 stress       -0.53* -0.38* 0.38*      –
#>   ============ ====== ====== ===== ======
#> # ℹ Within-person correlations above, between-person correlations below the
#> #   diagonal.
#> # ℹ All correlations marked with a star are significant at p < .05.
#> # ℹ Correlations estimated via variance decomposition.
```

### Reading the Matrix

**Upper triangle — within-person correlations:** Each coefficient
answers: *for a given person, on days when they score higher than their
own average on variable X, do they also tend to score higher than their
own average on variable Y?* For example, the cell where `stress` meets
`wellbeing` in the upper triangle tells us that days with higher stress
than usual also tend to be days with lower wellbeing for the same
person. For time-invariant variables (e.g., trait self-control),
within-group correlations cannot be estimated and appear as `NA` in the
upper triangle.

**Lower triangle — between-person correlations:** Each coefficient
answers: *do persons who score higher on variable X on average also tend
to score higher on variable Y on average?* For example, the cell where
`stress` meets `wellbeing` in the lower triangle tells us that people
who are more stressed on average also tend to have worse wellbeing
overall.

### When Within and Between Diverge

Importantly, within-group and between-group correlations can sometimes
differ in direction:

``` r

within_between_correlations(
  data  = media_diary,
  group = "person",
  vars  = c("wellbeing", "screen_time")
)
#> # Within- and Between-Person Correlations
#>   =========== ====== =====
#>   variable       `1`   `2`
#>   ----------- ------ -----
#> 1 wellbeing        – 0.42*
#> 2 screen_time -0.34*     –
#>   =========== ====== =====
#> # ℹ Within-person correlations above, between-person correlations below the
#> #   diagonal.
#> # ℹ All correlations marked with a star are significant at p < .05.
#> # ℹ Correlations estimated via variance decomposition.
```

The two correlations for `screen_time` and `wellbeing` tell different
stories:

- **Within-person (upper triangle):** Positive. On days when a person
  watches more entertainment media than they typically do, their
  wellbeing tends to be slightly higher than their own average —
  consistent with short-term escapism or mood repair.
- **Between-person (lower triangle):** Negative. People who watch more
  entertainment media on average tend to have lower average wellbeing —
  perhaps because chronic heavy media use reflects lower trait
  self-control.

Now look at what a naive pooled correlation gives:

``` r

cor(media_diary$screen_time, media_diary$wellbeing)
#> [1] 0.04597101
```

The pooled correlation is near zero. A researcher relying only on this
number would conclude that entertainment media use has no relationship
with wellbeing — missing a real positive within-person effect *and* a
real negative between-person association that point in opposite
directions.

### Estimation Method and Weighting

Three estimation methods for within- and between-group correlations are
available via the `method` argument:

- **`method = "decomposition"` (default)**: A fast, closed-form
  variance-decomposition approach (Pedhazur, 1997). Within-group
  correlations are computed from group-mean-centred scores;
  between-group correlations are computed from the group means.
- **`method = "sem"`**: Fits a two-level structural equation model using
  robust maximum likelihood. This handles very unequal group sizes more
  rigorously for significance testing.
- **`method = "bayes"`**: Fits Bayesian multivariate models via `brms`,
  reporting credible intervals instead of p-values. See
  [`vignette("correlation-methods")`](https://felixdidi.github.io/mlstats/articles/correlation-methods.md)
  for the statistical rationale and “Bayesian Estimation” below for the
  additional arguments it requires.

``` r

within_between_correlations(
  data   = media_diary,
  group  = "person",
  vars   = vars,
  method = "sem" # or "bayes"
)
```

The `weight` argument (decomposition method only) controls how much
influence each group has on the between-group estimate:

- **`weight = TRUE` (default)**: Between-group correlations and
  descriptive statistics are weighted by group size. Recommended for
  unbalanced data to recover the correct population-level estimate
  (Snijders & Bosker, 2012).
- **`weight = FALSE`**: Every group counts equally regardless of size.

Note that significance tests for between-group correlations always use
unweighted group means regardless of the `weight` setting — see
[`vignette("correlation-methods")`](https://felixdidi.github.io/mlstats/articles/correlation-methods.md)
for the statistical rationale.

``` r

within_between_correlations(
  data   = media_diary,
  group  = "person",
  vars   = vars,
  weight = FALSE
)
```

## Multilevel Descriptive Statistics

[`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md)
extends
[`within_between_correlations()`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
by adding means, standard deviations, ranges, and ICCs to the output:

``` r

result <- mldesc(
  data  = media_diary,
  group = "person",
  vars  = vars
)

result
#> # Multilevel Descriptive Statistics
#>   ============ ===== ====== ===== ===== ===== ===== ===== ===== =====
#>   variable     n_obs      m    sd range   `1`   `2`   `3`   `4`   icc
#>   ------------ ----- ------ ----- ----- ----- ----- ----- ----- -----
#> 1 Self control 1,400   4.03  0.83   2–6     –    NA    NA    NA  1.00
#> 2 Wellbeing    1,400   4.45  0.87   2–7  .61*     –  .42* -.43*   .46
#> 3 Screen time  1,400 128.66 42.29 0–272 -.67* -.34*     –  .29*   .45
#> 4 Stress       1,400   3.81  0.91   1–7 -.53* -.38*  .38*     –   .33
#>   ============ ===== ====== ===== ===== ===== ===== ===== ===== =====
#> # ℹ Within-person correlations above, between-person correlations below the
#> #   diagonal.
#> # ℹ All correlations marked with a star are significant at p < .05.
#> # ℹ Correlations estimated via variance decomposition.
#> # ℹ Group-weighted multilevel descriptive statistics computed with mlstats.
```

### Customising the Output

Several options control the appearance:

``` r

mldesc(
  data                = media_diary,
  group               = "person",
  vars                = vars,
  significance        = "detailed",  # *, **, *** for p < .05, .01, .001
  flip                = TRUE,        # between above diagonal, within below
  remove_leading_zero = FALSE        # keep "0.45" instead of ".45"
)
#> # Multilevel Descriptive Statistics
#>   ============ ===== ====== ===== ===== ===== ======== ======== ======== =====
#>   variable     n_obs      m    sd range   `1`      `2`      `3`      `4`   icc
#>   ------------ ----- ------ ----- ----- ----- -------- -------- -------- -----
#> 1 Self control 1,400   4.03  0.83   2–6     –  0.61*** -0.67*** -0.53***  1.00
#> 2 Wellbeing    1,400   4.45  0.87   2–7    NA        – -0.34*** -0.38***  0.46
#> 3 Screen time  1,400 128.66 42.29 0–272    NA  0.42***        –  0.38***  0.45
#> 4 Stress       1,400   3.81  0.91   1–7    NA -0.43***  0.29***        –  0.33
#>   ============ ===== ====== ===== ===== ===== ======== ======== ======== =====
#> # ℹ Between-person correlations above, within-person correlations below the
#> #   diagonal.
#> # ℹ Correlations marked with * are significant at p < .05, ** at p < .01, and
#> #   *** at p < .001.
#> # ℹ Correlations estimated via variance decomposition.
#> # ℹ Group-weighted multilevel descriptive statistics computed with mlstats.
```

- **`significance = "detailed"`**: Adds one star for *p* \< .05, two for
  *p* \< .01, and three for *p* \< .001. The default (`"basic"`) marks
  only *p* \< .05.
- **`flip = TRUE`**: Swaps the positions of within- and between-person
  correlations (between above the diagonal, within below).
- **`remove_leading_zero = FALSE`**: Keeps the leading zero in decimal
  numbers. The default removes it for APA formatting (`.45` instead of
  `0.45`).

The `method` and `weight` arguments can also be used and work exactly as
for
[`within_between_correlations()`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
(see above).

### Pipe-Friendly Output

Although the main purpose is to produce publication-ready tables, the
output of both
[`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md)
and
[`within_between_correlations()`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
are ordinary tibbles. The correlation and ICC columns use the class
`mlstats_stat` (abbreviated `mls`) to store formatted strings. To use
these values in further calculations, cast them to `numeric`, which
strips formatting and significance stars:

``` r

result_num <- result[c(1, 6:10)]
result_num[-1] <- lapply(result[6:10], as.numeric)
as_tibble(result_num)
#> # A tibble: 4 × 6
#>   variable       `1`   `2`   `3`   `4`   icc
#>   <chr>        <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Self control NA    NA    NA    NA     1   
#> 2 Wellbeing     0.61 NA     0.42 -0.43  0.46
#> 3 Screen time  -0.67 -0.34 NA     0.29  0.45
#> 4 Stress       -0.53 -0.38  0.38 NA     0.33
```

## Bayesian Estimation

`method = "bayes"` (available for both
[`within_between_correlations()`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
and
[`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md))
requires the **brms** package, which is not installed with **mlstats**
by default (`install.packages("brms")`), and two additional arguments:

- **`folder`**: a directory where fitted `brms` models are cached.
  Re-running the same call with the same data and settings reloads the
  cached model instead of refitting.
- **`ci`**: the width of the credible interval used to mark correlations
  (default `0.9`).

``` r

mldesc(
  data   = media_diary,
  group  = "person",
  vars   = vars,
  method = "bayes",
  folder = "brms_models",
  ci     = 0.95
)
```

MCMC sampling settings (iterations, chains) can be adjusted globally via
`options(mlstats.brms_iter = ..., mlstats.brms_chains = ...)`. See
[`vignette("correlation-methods")`](https://felixdidi.github.io/mlstats/articles/correlation-methods.md)
for the statistical rationale, the weighting/credible-interval
mechanics, and how many models a given call fits.

## References

Bürkner, P.-C. (2017). brms: An R package for Bayesian multilevel models
using Stan. *Journal of Statistical Software, 80*(1), 1–28.
<https://doi.org/10.18637/jss.v080.i01>

Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in
cross-sectional multilevel models: A new look at an old issue.
*Psychological Methods, 12*(2), 121–138.
<https://doi.org/10.1037/1082-989X.12.2.121>

Pedhazur, E. J. (1997). *Multiple regression in behavioral research:
Explanation and prediction* (3rd ed.). Harcourt Brace.

Snijders, T. A. B., & Bosker, R. J. (2012). *Multilevel analysis: An
introduction to basic and advanced multilevel modelling* (2nd ed.).
SAGE.

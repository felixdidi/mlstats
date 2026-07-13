# Getting Started with mlstats

``` r

library(mlstats)
library(dplyr)
```

The **mlstats** package provides tools for multilevel descriptive
statistics and data preparation. It is designed for data where
observations are nested within groups — for example, repeated daily
measurements per person, students within classrooms, or employees within
teams.

## Example Data

To demonstrate, we use `media_diary`, a simulated daily diary dataset
included with **mlstats**. It mimics a study in which 100 participants
completed brief daily surveys for 14 consecutive days (*N* = 100
persons, *T* = 1,400 daily observations). The variables are:

- **`person`**: person identifier
- **`self_control`**: trait self-control, measured once at study entry
  (stable, between-person characteristic; ICC ≈ 1)
- **`wellbeing`**: daily positive wellbeing (1–7)
- **`screen_time`**: minutes of entertainment media consumed that day
- **`stress`**: daily perceived stress (1–7)
- **`enjoyment`**: enjoyment of the media watched that day (1–7)

``` r

data("media_diary")
media_diary
#> # A tibble: 1,400 × 6
#>    person self_control wellbeing screen_time stress enjoyment
#>     <int>        <dbl>     <dbl>       <dbl>  <dbl>     <dbl>
#>  1      1          5.1       4.9          80    4.1       5.7
#>  2      1          5.1       5.4         120    4         5.6
#>  3      1          5.1       5.8          98    3.4       5  
#>  4      1          5.1       6.5         112    3.5       6.2
#>  5      1          5.1       5.5          21    2.8       4.4
#>  6      1          5.1       5.9          84    3.2       6  
#>  7      1          5.1       6.4          48    2.6       5.1
#>  8      1          5.1       5.7          58    2.6       4.5
#>  9      1          5.1       6.5          39    2.4       5.3
#> 10      1          5.1       4.8          82    4         5.4
#> # ℹ 1,390 more rows
```

The data are in long format: each row is one diary entry (one person on
one day). The `person` column identifies which person a row belongs to.

## Multilevel Descriptive Statistics

[`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md)
produces a publication-ready descriptive statistics table that combines
means, standard deviations, ranges, ICCs, and a within-/between-group
correlation matrix in a single object:

``` r

vars <- c("self_control", "wellbeing", "screen_time", "stress")

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

### Estimation Method

Three estimation methods are available via the `method` argument:

- **`method = "decomposition"`** (default): Uses the
  variance-decomposition approach to estimate within- and between-group
  correlations. Between-group correlations and descriptive statistics
  are weighted by group size when `weight = TRUE` (the default). Set
  `weight = FALSE` to give every group equal influence.
- **`method = "sem"`**: Fits a two-level structural equation model via
  `lavaan` using robust maximum likelihood. This handles very unequal
  group sizes more rigorously.
- **`method = "bayes"`**: Fits Bayesian multilevel models via `brms`,
  reporting credible intervals instead of p-values. Requires the
  additional `ci` and `folder` arguments; see
  [`vignette("multilevel-descriptives")`](https://felixdidi.github.io/mlstats/articles/multilevel-descriptives.md).
- See
  [`vignette("correlation-methods")`](https://felixdidi.github.io/mlstats/articles/correlation-methods.md)
  for a detailed comparison.

### Customising the Output

Several options control the appearance of the output:

- **`significance = "detailed"`**: Adds stars for *p* \< .05, *p* \<
  .01, and *p* \< .001. The default (`"basic"`) marks only *p* \< .05.
- **`flip = TRUE`**: Swaps the correlation matrix (between above, within
  below).
- **`remove_leading_zero = FALSE`**: Keeps the leading zero in decimal
  numbers. The default removes it for APA formatting (`.45` instead of
  `0.45`).

### Pretty Printing

The result can be formatted for publication via
[`print()`](https://rdrr.io/r/base/print.html). All print methods accept
optional arguments `table_title`, `correlation_note`,
`significance_note`, and `note_text`.

**tinytable** is included with **mlstats** (no extra installation
needed):

``` r

result |>
  print(format = "tt")
```

|  |  | Descriptives |  |  |  | Correlations^(a,b) |  |  |  | ICC |
|----|----|----|----|----|----|----|----|----|----|----|
|  | Variable | *N*_(obs) | *M* | *SD* | Range | 1 | 2 | 3 | 4 |  |
| 1 | Self control | 1,400 | 4.03 | 0.83 | 2–6 | – | NA | NA | NA | 1.00 |
| 2 | Wellbeing | 1,400 | 4.45 | 0.87 | 2–7 | .61\* | – | .42\* | -.43\* | .46 |
| 3 | Screen time | 1,400 | 128.66 | 42.29 | 0–272 | -.67\* | -.34\* | – | .29\* | .45 |
| 4 | Stress | 1,400 | 3.81 | 0.91 | 1–7 | -.53\* | -.38\* | .38\* | – | .33 |
| *Note.* Group-weighted multilevel descriptive statistics computed with mlstats. |  |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person correlations below the diagonal. |  |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |  |

Multilevel Descriptive Statistics {#tinytable_ya08tpxs7fzqv0x2gd4b
.table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

If more customization is needed, **gt** produces richly formatted HTML
tables. It must be installed separately:

``` r

install.packages("gt")
```

``` r

result |>
  print(format = "gt")
```

[TABLE]

Both `tt` and `gt` smoothly render to HTML, PDF, or Word via R Markdown
or Quarto.

For details on customising printed tables — including custom titles,
notes, variable labels, and column selection — see
[`vignette("tables")`](https://felixdidi.github.io/mlstats/articles/tables.md).

For detailed coverage of all
[`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md)
options and
[`within_between_correlations()`](https://felixdidi.github.io/mlstats/reference/within_between_correlations.md)
(the underlying function), including ICC and correlation matrix
interpretation, see
[`vignette("multilevel-descriptives")`](https://felixdidi.github.io/mlstats/articles/multilevel-descriptives.md).

## Decomposing Variables into Within- and Between-Person Components

Before fitting multilevel models, time-varying predictors are typically
decomposed into their within-group and between-group components.
[`decompose_within_between()`](https://felixdidi.github.io/mlstats/reference/decompose_within_between.md)
makes this easy by adding, by default, two new columns per variable:

- **`_between_{group}`**: group mean (stable between-group component)
- **`_within_{group}`**: deviation from the group mean (within-group
  fluctuation)

A third, optional column can be requested via `components`:

- **`_grand_mean_centered`**: grand-mean-centered value

``` r

media_diary |>
  decompose_within_between(
    group = "person",
    vars  = c("stress", "screen_time")
  ) |>
  select(starts_with("stress"))
#> # A tibble: 1,400 × 3
#>    stress stress_between_person stress_within_person
#>     <dbl>                 <dbl>                <dbl>
#>  1    4.1                  3.26               0.843 
#>  2    4                    3.26               0.743 
#>  3    3.4                  3.26               0.143 
#>  4    3.5                  3.26               0.243 
#>  5    2.8                  3.26              -0.457 
#>  6    3.2                  3.26              -0.0571
#>  7    2.6                  3.26              -0.657 
#>  8    2.6                  3.26              -0.657 
#>  9    2.4                  3.26              -0.857 
#> 10    4                    3.26               0.743 
#> # ℹ 1,390 more rows
```

The within and between components serve as separate predictors in Random
Effects Within-Between (REWB) models, which estimate distinct
within-group and between-group effects. See
[`vignette("rewb-models")`](https://felixdidi.github.io/mlstats/articles/rewb-models.md)
for a full guide to data preparation and REWB model fitting with
mlstats, including all options of
[`decompose_within_between()`](https://felixdidi.github.io/mlstats/reference/decompose_within_between.md).

## References

Bell, A., Fairbrother, M., & Jones, K. (2019). Fixed and random effects
models: Making an informed choice. *Quality & Quantity, 53*(2),
1051–1074. <https://doi.org/10.1007/s11135-018-0802-x>

Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in
cross-sectional multilevel models: A new look at an old issue.
*Psychological Methods, 12*(2), 121–138.
<https://doi.org/10.1037/1082-989X.12.2.121>

Pedhazur, E. J. (1997). *Multiple regression in behavioral research:
Explanation and prediction* (3rd ed.). Harcourt Brace.

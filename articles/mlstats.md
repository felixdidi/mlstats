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
were asked to complete a brief daily survey for up to 14 consecutive
days; as in most real mobile diary studies, not everyone completed every
day (*N* = 100 persons, *T* = 5–14 daily observations per person, 1,184
total). The variables are:

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
#> # A tibble: 1,184 × 6
#>    person self_control wellbeing screen_time stress enjoyment
#>     <int>        <dbl>     <dbl>       <dbl>  <dbl>     <dbl>
#>  1      1            5       3.5          83    3.9       4.5
#>  2      1            5       4            82    4.4       3.9
#>  3      1            5       3.4         103    4.8       3.8
#>  4      1            5       3.7         105    4.7       4.6
#>  5      1            5       3.9          68    3.6       3.7
#>  6      1            5       4.3         143    5.3       5  
#>  7      1            5       5.3         139    2.9       5.4
#>  8      1            5       3.5         105    4.7       4.2
#>  9      1            5       3.1          75    3.7       3.9
#> 10      1            5       3.9          55    2.6       3.8
#> # ℹ 1,174 more rows
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
`significance_note`, `group_size_note`, and `note_text`.

**tinytable** is included with **mlstats** (no extra installation
needed):

``` r

result |>
  print(format = "tt")
```

|  |  | Descriptives |  |  |  | Correlations^(a,b) |  |  |  | ICC |
|----|----|----|----|----|----|----|----|----|----|----|
|  | Variable | *N*_(obs) | *M* | *SD* | Range | 1 | 2 | 3 | 4 |  |
| 1 | Self control | 100 | 3.93 | 0.70 | 2.40–6.20 | – | NA | NA | NA | 1.00 |
| 2 | Wellbeing | 1,184 | 4.42 | 0.89 | 1.50–7.00 | .47\* | – | .42\* | -.40\* | .50 |
| 3 | Screen time | 1,184 | 132.05 | 39.01 | 15–246 | -.66\* | -.32\* | – | .29\* | .34 |
| 4 | Stress | 1,184 | 3.77 | 0.95 | 1–7 | -.55\* | -.27\* | .46\* | – | .39 |
| *Note.* Group-weighted multilevel descriptive statistics computed with mlstats. |  |  |  |  |  |  |  |  |  |  |
|  Based on 100 persons and 1,184 observations (median 12 per person; range: 5–14). |  |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person correlations below the diagonal. |  |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |  |

Multilevel Descriptive Statistics {#tinytable_um962jdg6gn0l9g3g3t6
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
#> # A tibble: 1,184 × 3
#>    stress stress_between_person stress_within_person
#>     <dbl>                 <dbl>                <dbl>
#>  1    3.9                  4.12               -0.221
#>  2    4.4                  4.12                0.279
#>  3    4.8                  4.12                0.679
#>  4    4.7                  4.12                0.579
#>  5    3.6                  4.12               -0.521
#>  6    5.3                  4.12                1.18 
#>  7    2.9                  4.12               -1.22 
#>  8    4.7                  4.12                0.579
#>  9    3.7                  4.12               -0.421
#> 10    2.6                  4.12               -1.52 
#> # ℹ 1,174 more rows
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

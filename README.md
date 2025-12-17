
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlstats <img src="man/img/sticker.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/felixdidi/mlstats/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/felixdidi/mlstats/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The **mlstats** package provides tools for conducting multilevel
analyses, such as centering variables to compute random effects
within-between (REWB) models or creating publication-ready descriptive
tables, including within-group and between-group correlations, as well
as intraclass correlation coefficients (ICCs). The package supports both
frequentist (using `lme4`) and Bayesian (using `brms`) estimation
methods.

## Installation

You can install the development version of mlstats from GitHub:

``` r
# install.packages("pak")
pak::pak("felixdidi/mlstats")
```

## Multilevel Descriptives

### Easy Defaults

The `mldesc()` function makes it easy to compute descriptive statistics
for multilevel data, including basic descriptives (mean, SD, range),
within-group and between-group correlations, as well as intraclass
correlation coefficients (ICCs). Let’s attempt to recreate Table 1 from
Klingelhoefer et al. (2024):

``` r
library(mlstats)

vars <- c(
  "disconnective_behavior",
  "distraction_motivation",
  "wellbeing_motivation",
  "presence_motivation"
)

data |>
  mldesc(
    group = "person",
    vars = vars
  )
#> # Multilevel descriptive statistics
#>   ====================== ====== ===== ===== ===== ===== ===== ===== ===== =====
#>   variable                n_obs     m    sd range   `1`   `2`   `3`   `4`   icc
#>   ---------------------- ------ ----- ----- ----- ----- ----- ----- ----- -----
#> 1 Disconnective behavior 12,407  0.38  0.39   0-1     –  .12*   .02  .15*   .45
#> 2 Distraction motivation  7,360  5.11  2.02   1-7   .10     –  .13*  .20*   .27
#> 3 Wellbeing motivation    7,360  3.59  2.20   1-7  .23*  .41*     –  .10*   .50
#> 4 Presence motivation     7,360  4.61  2.21   1-7  .18*  .39*  .32*     –   .31
#>   ====================== ====== ===== ===== ===== ===== ===== ===== ===== =====
#> # Within-group correlations above, between-group correlations below the diagonal.
#> # All correlations marked with a star are significant at p < .05.
#> # Group-weighted multilevel descriptive statistics computed with mlstats.
```

### Customizable Options

The `mldesc()` function offers several options to customize the output:

- **Weight by group size**: By default, `mldesc()` weights the group
  means, standard deviations, and between-group correlations by group
  size. This can be disabled by setting `weight = FALSE` (e.g., so that
  each person in the sample contributes equally to the overall mean and
  the between-person correlations).
- **Remove leading zeros**: By default, `mldesc()` removes leading zeros
  from decimal numbers to comply with APA formatting guidelines. This
  can be disabled by setting `remove_leading_zero = FALSE`.
- **Flip correlation matrix**: By default, `mldesc()` displays
  within-group correlations above the diagonal and between-group
  correlations below the diagonal. This can be changed by setting
  `flip = TRUE`.

``` r
data |>
  mldesc(
    group = "person",
    vars = vars,
    weight = FALSE,
    remove_leading_zero = FALSE,
    flip = TRUE
  )
#> # Multilevel descriptive statistics
#>   ====================== ====== ===== ===== ===== ===== ===== ===== ===== =====
#>   variable                n_obs     m    sd range   `1`   `2`   `3`   `4`   icc
#>   ---------------------- ------ ----- ----- ----- ----- ----- ----- ----- -----
#> 1 Disconnective behavior 12,407  0.38  0.27   0-1     –  0.12 0.25* 0.19*  0.45
#> 2 Distraction motivation  7,360  5.09  1.19   1-7 0.12*     – 0.39* 0.38*  0.27
#> 3 Wellbeing motivation    7,360  3.48  1.58   1-7  0.02 0.13*     – 0.33*  0.50
#> 4 Presence motivation     7,360  4.50  1.39   1-7 0.15* 0.20* 0.10*     –  0.31
#>   ====================== ====== ===== ===== ===== ===== ===== ===== ===== =====
#> # Between-group correlations above, within-group correlations below the diagonal.
#> # All correlations marked with a star are significant at p < .05.
#> # Unweighted multilevel descriptive statistics computed with mlstats.
```

### Pretty Printing

The `mldesc()`-function also supports various print-methods to create
nicely formatted tables that look great in your Quarto document and can
be directly copied into a manuscript. Whereas the default print method
outputs the table to the console, `gt` and `tinytable` outputs are also
supported by setting the respective `format` option. In addition, users
can provide a custom table title via the `table_title` parameter, as
well as custom `correlation_note` and `note_text`.

``` r
data |>
  mldesc(
    group = "person",
    vars = vars
  ) |> 
  print(
    format = "gt", # "tt" for tinytable
    table_title = "Descriptive statistics, within- and between-person correlations of digital disconnection and motivations",
    correlation_note = "Within-person correlations are displayed above the diagonal, between-person correlations below the diagonal.",
    note_text = NULL
  )
```

![](man/img/gt-example.png)

### Pipe-Friendly Output

Although the main purpose of the package is to enable user-friendly
creation of publication-ready tables, the output of both `mldesc()` and
the underlying `within_between_correlations()` are tibbles with vectors
of class `mlstats_stat` (or short, `mls`). These tibbles can be used for
subsequent calculations by casting the contents to useful types such as
`numeric` (this will remove significance stars and other formatting).
For example, the output of `within_between_correlations()` can be used
to identify the largest within-person correlation in the dataset:

``` r
cors <-
  data |>
  within_between_correlations(
    group = "person",
    vars = vars
  )

cors |>
  mutate(across(-variable, as.numeric)) |> 
  rename_with(~ cors$variable, .cols = -variable) |>
  pivot_longer(-variable) |>
  rename(v1 = variable, v2 = name) |>
  group_by(v1) |>
  mutate(type = if_else(row_number() > which(is.na(value)), "wp", "bp")) |>
  ungroup() |>
  filter(type == "wp") |> 
  filter(value == max(value))
#> # A tibble: 1 × 4
#>   v1                     v2                  value type 
#>   <chr>                  <chr>               <dbl> <chr>
#> 1 distraction_motivation presence_motivation   0.2 wp
```

## Centering

The package also facilitates centering of variables for further use with
the `decompose_within_between()` function. This centering approach is
commonly used in multilevel modeling to separate within-group and
between-group variance components (Enders & Tofighi, 2007). The
decomposed variables are particularly useful for Random Effects
Within-Between (REWB) models (Bell et al., 2019), which allow the
estimation of distinct within-group and between-group effects.

``` r
data |>
  rename(disco = disconnective_behavior) |>
  decompose_within_between(
    group = "person",
    vars = "disco"
  ) |>
  select(person, matches("disco"))
#> # A tibble: 12,407 × 5
#>    person disco disco_grand_mean_cent…¹ disco_between_person disco_within_person
#>     <int> <dbl>                   <dbl>                <dbl>               <dbl>
#>  1      1   0                   -0.376                 0.127             -0.127 
#>  2      1   0                   -0.376                 0.127             -0.127 
#>  3      1   0                   -0.376                 0.127             -0.127 
#>  4      1   0                   -0.376                 0.127             -0.127 
#>  5      1   0.2                 -0.176                 0.127              0.0727
#>  6      1   0                   -0.376                 0.127             -0.127 
#>  7      1   0.4                  0.0239                0.127              0.273 
#>  8      1   0                   -0.376                 0.127             -0.127 
#>  9      1   0                   -0.376                 0.127             -0.127 
#> 10      1   0                   -0.376                 0.127             -0.127 
#> # ℹ 12,397 more rows
#> # ℹ abbreviated name: ¹​disco_grand_mean_centered
```

## Bayesian Estimation

If desired, the package also supports Bayesian estimation via `brms`,
providing credible intervals instead of *p*-values through
`bayes_mldesc()` and `bayes_within_between_correlations()`. In addition
to the parameters available in the frequentist functions, users must
specify the credible interval width (`ci`) and a folder to save the
fitted models (`folder`).

Note that the Bayesian functions may take a considerable amount of time
to run (and use a considerable amount of disc space for model files)
because they fit one `brms`-model per correlation coefficient.

## References

Bell, A., Fairbrother, M., & Jones, K. (2019). Fixed and random effects
models: Making an informed choice. *Quality & Quantity, 53*(2),
1051–1074. <https://doi.org/10/gd8wcr>

Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in
cross-sectional multilevel models: A new look at an old issue.
*Psychological Methods, 12*(2), 121–138. <https://doi.org/10/b2jz57>

Klingelhoefer, J., Gilbert, A., & Meier, A. (2024). Momentary
motivations for digital disconnection: An experience sampling study.
*Journal of Computer-Mediated Communication, 29*(5), zmae013.
<https://doi.org/10/hbb5gx>

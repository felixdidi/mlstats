
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlstats <img src="man/img/sticker.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/felixdidi/mlstats/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/felixdidi/mlstats/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The **mlstats** package provides tools for conducting multilevel
analyses, such as centering variables to compute random effects
within-between (REWB) models or creating publication-ready descriptive
tables, including within-group and between-group correlations, as well
as intraclass correlation coefficients (ICCs). The package supports
frequentist (using `lme4`) and Bayesian (using `brms`) estimation.

## Installation

You can install the development version of mlstats from GitHub:

``` r
# install.packages("pak")
pak::pak("felixdidi/mlstats")
```

## Multilevel Descriptives

### Easy Defaults

The `mldesc()` function has defaults that make it easy to compute
descriptive statistics for multilevel data. It outputs basic
descriptives, ICCs, as well as within-group and between-group
correlations for a set of variables, given a grouping variable (e.g.,
person ID). If desired, the `within_between_correlations()` function
computes only a correlation matrix without additional descriptives.

``` r
library(mlstats)

vars <- c(
  "self_control",
  "goal_conflict",
  "disconnection",
  "procrastination"
)

data |>
  mldesc(
    group = "person",
    vars = vars
  )
#> # Multilevel descriptive statistics
#>   =============== ====== ===== ===== ===== ===== ===== ===== ===== =====
#>   variable         n_obs     m    sd range   `1`   `2`   `3`   `4`   icc
#>   --------------- ------ ----- ----- ----- ----- ----- ----- ----- -----
#> 1 Self control    12,408  3.78  1.15   2-7     –    NA    NA    NA  1.00
#> 2 Goal conflict   12,408  3.19  2.13   1-7 -.22*     –  .13*  .31*   .47
#> 3 Disconnection   12,408  0.59  0.49   0-1  .14*  .36*     – -.09*   .40
#> 4 Procrastination 12,408  2.27  1.76   1-7 -.37*  .56*   .05     –   .25
#>   =============== ====== ===== ===== ===== ===== ===== ===== ===== =====
#> # ℹ Within-group correlations above, between-group correlations below the
#> #   diagonal.
#> # ℹ All correlations marked with a star are significant at p < .05.
#> # ℹ Group-weighted multilevel descriptive statistics computed with mlstats.
```

### Customizable Options

There are several options to customize the output:

- **Weight by group size**: By default, the group means, standard
  deviations, and between-group correlations are weighted by group size.
  This can be disabled by setting `weight = FALSE` (e.g., so that each
  person in the sample contributes equally to the overall mean and the
  between-person correlations).
- **Remove leading zeros**: By default, `mldesc()` removes leading zeros
  from decimal numbers to comply with APA formatting guidelines. This
  can be disabled by setting `remove_leading_zero = FALSE`.
- **Flip correlation matrix**: By default, within-group correlations are
  displayed above the diagonal and between-group correlations below the
  diagonal. This can be changed by setting `flip = TRUE`.
- **Significance stars**: By default, one star is added to all
  correlation coefficients with *p* \< .05. By setting
  `significance = "detailed"`, this can be changed to one star for *p*
  \< .05, two stars for *p* \< .01, and three stars for *p* \< .001.

``` r
data |>
  mldesc(
    group = "person",
    vars = vars,
    weight = FALSE,
    remove_leading_zero = FALSE,
    flip = TRUE,
    significance = "detailed"
  )
#> # Multilevel descriptive statistics
#>   =============== ====== ===== ===== ===== ===== ======== ======== ========
#>   variable         n_obs     m    sd range   `1`      `2`      `3`      `4`
#>   --------------- ------ ----- ----- ----- ----- -------- -------- --------
#> 1 Self control    12,408  3.78  1.16   2-7     – -0.22***    0.13* -0.36***
#> 2 Goal conflict   12,408  3.22  1.48   1-7    NA        –  0.37***  0.56***
#> 3 Disconnection   12,408  0.60  0.32   0-1    NA  0.13***        –     0.06
#> 4 Procrastination 12,408  2.29  0.90   1-7    NA  0.31*** -0.09***        –
#>   =============== ====== ===== ===== ===== ===== ======== ======== ========
#> # ℹ 1 more variable: icc <mls>
#> # ℹ Between-group correlations above, within-group correlations below the
#> #   diagonal.
#> # ℹ Correlations marked with * are significant at p < .05, ** at p < .01, and
#> #   *** at p < .001.
#> # ℹ Unweighted multilevel descriptive statistics computed with mlstats.
```

### Pretty Printing

The `mldesc()` function supports various print-methods that can be
accessed by passing its output to `print()`. All printing methods allow
customization of the `table_title`, `correlation_note`,
`significance_note`, and `note_text`. Whereas the default print method
prints to the console, a `gt` object is returned when setting
`format = "gt"` and a `tinytable` object when setting `format = "tt"`.

All outputs are designed to look great by default — however, users can
further customize the output by modifying the resulting `tibble`, `gt`,
and `tt` objects (for customization of `gt` tables, see its
documentation [here](https://gt.rstudio.com/); for tinytable, see
[here](https://vincentarelbundock.github.io/tinytable/)). For example,
to reproduce Table 1 from Klingelhoefer et al. (2025), we can adjust the
output by selecting relevant columns, replacing `NA`s with dashes, and
adding footnotes:

``` r
data |>
  mldesc(group = "person", vars = vars, significance = "detailed") |>
  select(-n_obs, -range) |>
  mutate(across(everything(), ~ str_replace(.x, "NA", "–"))) |>
  mutate(across(any_of(c("m", "sd")), ~ if_else(variable == "Disconnection", "–", .x))) |>
  mutate(variable = case_when(variable == "Self control" ~ "Self-control<sup>c</sup>", variable == "Goal conflict" ~ "Goal-conflict", variable == "Disconnection" ~ "Disconnection<sup>d</sup>", variable == "Procrastination" ~ "Procrastination")) |>
  print(
    format = "gt",
    table_title = "Descriptive statistics, within- and between-person correlations for central variables",
    correlation_note = "Within-person correlations depicted above, between-person correlations below the diagonal.",
    note_text = "<i>Note</i>. <i>N</i> = 237, <i>T</i> = 12,408."
  ) |>
  gt::tab_source_note(source_note = gt::html("<sup>c</sup> Self-control was measured as a trait, and no within-person correlation is available.")) |>
  gt::tab_source_note(source_note = gt::html("<sup>d</sup> Digital disconnection was operationalized as a binary variable and does not have a mean or standard deviation.")) |>
  gt::fmt_markdown(columns = variable)
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
#>   v1            v2              value type 
#>   <chr>         <chr>           <dbl> <chr>
#> 1 goal_conflict procrastination  0.31 wp
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
  rename(disco = disconnection) |>
  decompose_within_between(
    group = "person",
    vars = "disco"
  ) |>
  select(person, matches("disco"))
#> # A tibble: 12,408 × 5
#>    person disco disco_grand_mean_cent…¹ disco_between_person disco_within_person
#>     <int> <int>                   <dbl>                <dbl>               <dbl>
#>  1      1     0                  -0.593                0.488              -0.488
#>  2      1     1                   0.407                0.488               0.512
#>  3      1     0                  -0.593                0.488              -0.488
#>  4      1     1                   0.407                0.488               0.512
#>  5      1     1                   0.407                0.488               0.512
#>  6      1     0                  -0.593                0.488              -0.488
#>  7      1     0                  -0.593                0.488              -0.488
#>  8      1     0                  -0.593                0.488              -0.488
#>  9      1     1                   0.407                0.488               0.512
#> 10      1     1                   0.407                0.488               0.512
#> # ℹ 12,398 more rows
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

Klingelhoefer, J., Gilbert, A., & Meier, A. (2025). Digital
disconnection as a self-regulatory strategy against procrastination.
*PsyArXiv*. <https://doi.org/10.31234/osf.io/3j64v_v1>

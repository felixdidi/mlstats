# REWB Models

``` r

library(mlstats)
library(dplyr)
library(lme4)
library(lmerTest)
```

When observations are nested within groups (repeated measurements per
person, students within classrooms, etc.), the association between two
variables has two faces: how they co-vary *within* groups over time, and
whether groups that score higher on one variable also tend to score
higher on the other. A naive linear regression cannot distinguish these
two effects, and conflating them can produce severely misleading
conclusions.

The **Random Effects Within-Between (REWB)** model (Bell et al., 2019)
solves this by including both components as separate predictors. This
vignette shows how to use
[`decompose_within_between()`](https://felixdidi.github.io/mlstats/reference/decompose_within_between.md)
to prepare data for REWB models and how to fit and interpret those
models.

## Example Data

We use `media_diary`, a simulated daily diary dataset included with
**mlstats** (100 participants over 14 days; *N* = 100 persons, *T* =
1,400 daily observations). See
[`?media_diary`](https://felixdidi.github.io/mlstats/reference/media_diary.md)
for details.

``` r

data("media_diary")
```

The dataset was generated to illustrate two processes that can operate
simultaneously — and in opposite directions — at within- and
between-person levels:

1.  **Between persons**: people who watch more entertainment media on
    average tend to have lower average wellbeing — perhaps because
    chronic heavy media use reflects lower trait self-control, which
    itself predicts lower wellbeing.
2.  **Within persons**: on days when someone watches more than usual,
    their wellbeing tends to be higher — consistent with short-term
    escapism or mood repair through media use.

Because these processes were built into the simulation, they are present
by design — not empirical discoveries. The purpose of the example is to
show how REWB models recover effects that point in *opposite
directions*, and what happens when they are conflated. A naive
regression conflates them and produces a near-zero coefficient, making
it appear that screen time has no relationship with wellbeing, when in
fact it has two real and opposing effects (in the simulation).

## Decomposing Time-Varying Predictors

[`decompose_within_between()`](https://felixdidi.github.io/mlstats/reference/decompose_within_between.md)
splits each specified variable into up to three components:

- **`_between_{group}`**: group mean (stable between-group component)
- **`_within_{group}`**: deviation from the group mean (within-group
  fluctuation)
- **`_grand_mean_centered`**: grand-mean-centered value (opt-in; see
  below)

The `vars` argument names the variables to decompose. `group` names the
grouping variable. By default,
[`decompose_within_between()`](https://felixdidi.github.io/mlstats/reference/decompose_within_between.md)
returns the within- and between-group components — the two predictors
REWB models need.

``` r

media_diary |>
  decompose_within_between(group = "person", vars = "screen_time") |>
  select(starts_with("screen_time_"))
#> # A tibble: 1,400 × 2
#>    screen_time_between_person screen_time_within_person
#>                         <dbl>                     <dbl>
#>  1                       69.9                      10.1
#>  2                       69.9                      50.1
#>  3                       69.9                      28.1
#>  4                       69.9                      42.1
#>  5                       69.9                     -48.9
#>  6                       69.9                      14.1
#>  7                       69.9                     -21.9
#>  8                       69.9                     -11.9
#>  9                       69.9                     -30.9
#> 10                       69.9                      12.1
#> # ℹ 1,390 more rows
```

`screen_time_within_person` is the group-mean-centred score: how many
more (or fewer) minutes this person watched today compared to their own
average. `screen_time_between_person` is the person’s average screen
time, repeated for every row belonging to that person.

### Selecting Components

Use the `components` argument to add the grand-mean-centered score, or
to restrict the output to a single component.

``` r

media_diary |>
  decompose_within_between(
    group = "person",
    vars = "screen_time",
    components = c("within", "between", "gmc")
  ) |>
  select(starts_with("screen_time"))
#> # A tibble: 1,400 × 4
#>    screen_time screen_time_grand_mean_centered screen_time_between_person
#>          <dbl>                           <dbl>                      <dbl>
#>  1          80                          -48.7                        69.9
#>  2         120                           -8.66                       69.9
#>  3          98                          -30.7                        69.9
#>  4         112                          -16.7                        69.9
#>  5          21                         -108.                         69.9
#>  6          84                          -44.7                        69.9
#>  7          48                          -80.7                        69.9
#>  8          58                          -70.7                        69.9
#>  9          39                          -89.7                        69.9
#> 10          82                          -46.7                        69.9
#> # ℹ 1,390 more rows
#> # ℹ 1 more variable: screen_time_within_person <dbl>
```

`screen_time_grand_mean_centered` is the grand-mean-centred value, which
shows each observation’s deviation from the overall mean. Valid values
for `components` are any non-empty subset of
`c("within", "between", "gmc")`; the default is
`c("within", "between")`.

### Customising Column Names

The `within_pattern`, `between_pattern`, and `gmc_pattern` arguments
control the naming of the new columns. Each pattern is a glue-style
string where `{col}` is replaced by the variable name and `{group}` is
replaced by the grouping variable name. For example, the default
`{col}_within_{group}` produces `screen_time_within_person`. Here, we
use `{col}_wg` and `{col}_bg` to produce shorter names:

``` r

media_diary |>
  decompose_within_between(
    group = "person",
    vars = c("screen_time"),
    components = c("within", "between"),
    within_pattern = "{col}_wg",
    between_pattern = "{col}_bg"
  ) |>
  select(starts_with("screen_time"))
#> # A tibble: 1,400 × 3
#>    screen_time screen_time_bg screen_time_wg
#>          <dbl>          <dbl>          <dbl>
#>  1          80           69.9           10.1
#>  2         120           69.9           50.1
#>  3          98           69.9           28.1
#>  4         112           69.9           42.1
#>  5          21           69.9          -48.9
#>  6          84           69.9           14.1
#>  7          48           69.9          -21.9
#>  8          58           69.9          -11.9
#>  9          39           69.9          -30.9
#> 10          82           69.9           12.1
#> # ℹ 1,390 more rows
```

### Decomposing Multiple Variables at Once

Pass a character vector to `vars` to decompose several variables in a
single call. The same `components` and naming patterns apply to all
variables:

``` r

media_diary |>
  decompose_within_between(
    group = "person",
    vars = c("screen_time", "stress"),
    components = c("within", "between")
  ) |>
  glimpse()
#> Rows: 1,400
#> Columns: 10
#> $ person                     <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2…
#> $ self_control               <dbl> 5.1, 5.1, 5.1, 5.1, 5.1, 5.1, 5.1, 5.1, 5.1…
#> $ wellbeing                  <dbl> 4.9, 5.4, 5.8, 6.5, 5.5, 5.9, 6.4, 5.7, 6.5…
#> $ screen_time                <dbl> 80, 120, 98, 112, 21, 84, 48, 58, 39, 82, 8…
#> $ stress                     <dbl> 4.1, 4.0, 3.4, 3.5, 2.8, 3.2, 2.6, 2.6, 2.4…
#> $ enjoyment                  <dbl> 5.7, 5.6, 5.0, 6.2, 4.4, 6.0, 5.1, 4.5, 5.3…
#> $ screen_time_between_person <dbl> 69.92857, 69.92857, 69.92857, 69.92857, 69.…
#> $ stress_between_person      <dbl> 3.257143, 3.257143, 3.257143, 3.257143, 3.2…
#> $ screen_time_within_person  <dbl> 10.071429, 50.071429, 28.071429, 42.071429,…
#> $ stress_within_person       <dbl> 0.84285714, 0.74285714, 0.14285714, 0.24285…
```

## Fitting the REWB Model

### Step 1 — Within and between effects

We start with a model that includes only the within- and between-person
components of `screen_time` and a random intercept for person. This is
the core REWB specification:

``` r

diary_decomp <- decompose_within_between(
  data            = media_diary,
  group           = "person",
  vars            = "screen_time",
  components      = c("within", "between"),
  within_pattern  = "{col}_within",
  between_pattern = "{col}_between"
)

fit_rewb <- lmer(
  wellbeing ~ screen_time_within + screen_time_between + (1 | person),
  data = diary_decomp
)

summary(fit_rewb, correlation = FALSE)
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: wellbeing ~ screen_time_within + screen_time_between + (1 | person)
#>    Data: diary_decomp
#> 
#> REML criterion at convergence: 2747.7
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -2.9465 -0.6343 -0.0225  0.6429  3.3618 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev.
#>  person   (Intercept) 0.3126   0.5591  
#>  Residual             0.3393   0.5825  
#> Number of obs: 1400, groups:  person, 100
#> 
#> Fixed effects:
#>                       Estimate Std. Error         df t value Pr(>|t|)    
#> (Intercept)          5.364e+00  2.596e-01  9.800e+01  20.659  < 2e-16 ***
#> screen_time_within   8.585e-03  5.143e-04  1.299e+03  16.694  < 2e-16 ***
#> screen_time_between -7.095e-03  1.967e-03  9.800e+01  -3.607  0.00049 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

**Interpreting the coefficients:**

In this simulated dataset, the within-person coefficient (0.0086) is
positive and highly significant. To illustrate how such an effect would
be interpreted: on days when someone watches one minute more than their
own average, their wellbeing is 0.0086 points higher. For a person
watching 60 minutes more than usual, the expected gain would be 0.52
wellbeing points.

The between-person coefficient (-0.0071) is negative and significant.
Illustrating interpretation: people who watch one minute more per day on
average show 0.0071 lower wellbeing. For someone who watches 60 minutes
more per day on average than another person, the expected wellbeing gap
would be 0.43 points.

The two effects point in *opposite directions* — exactly the pattern
built into the simulation. A naive regression conflates them:

``` r

fit_naive <- lm(wellbeing ~ screen_time, data = diary_decomp)
summary(fit_naive)
#> 
#> Call:
#> lm(formula = wellbeing ~ screen_time, data = diary_decomp)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.95423 -0.61285  0.01078  0.60269  2.42402 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 4.3293929  0.0744348  58.164   <2e-16 ***
#> screen_time 0.0009457  0.0005496   1.721   0.0855 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.8693 on 1398 degrees of freedom
#> Multiple R-squared:  0.002113,   Adjusted R-squared:  0.0014 
#> F-statistic: 2.961 on 1 and 1398 DF,  p-value: 0.08553
```

The naive coefficient is near zero because the positive within-person
and negative between-person effects cancel each other out — an entirely
uninformative result that hides two simulated effects pointing in
opposite directions. This illustrates why a naive regression can be
misleading when within- and between-group processes operate
simultaneously.

### Step 2 — Accounting for confounding

`self_control` was identified above as a confounder of the
*between-person* effect: people with lower trait self-control may watch
more media on average *and* have lower wellbeing, making it appear as
though heavy media use causes worse wellbeing at the between-person
level. Adding `self_control` as a covariate lets us test whether the
between-person association with screen time persists after removing this
alternative explanation.

``` r

fit_rewb_conf <- lmer(
  wellbeing ~ screen_time_within + screen_time_between + self_control +
    (1 | person),
  data = diary_decomp
)

summary(fit_rewb_conf, correlation = FALSE)
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: wellbeing ~ screen_time_within + screen_time_between + self_control +  
#>     (1 | person)
#>    Data: diary_decomp
#> 
#> REML criterion at convergence: 2715.5
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -2.9677 -0.6221 -0.0331  0.6453  3.3345 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev.
#>  person   (Intercept) 0.2129   0.4614  
#>  Residual             0.3393   0.5825  
#> Number of obs: 1400, groups:  person, 100
#> 
#> Fixed effects:
#>                      Estimate Std. Error        df t value Pr(>|t|)    
#> (Intercept)         2.026e+00  5.582e-01 9.700e+01   3.629 0.000457 ***
#> screen_time_within  8.585e-03  5.143e-04 1.299e+03  16.694  < 2e-16 ***
#> screen_time_between 2.673e-03  2.233e-03 9.700e+01   1.197 0.234218    
#> self_control        5.170e-01  7.960e-02 9.700e+01   6.495 3.56e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

**Interpreting the coefficients:**

In this simulated dataset, the within-person coefficient is unchanged
(0.0086): `self_control` is a stable trait measured once per person, so
it carries no within-person variation and cannot alter the within-person
estimate. This is a general property of between-person covariates in
REWB models, not specific to these simulated data.

The between-person coefficient changes substantially — from -0.0071
(significant) in the unadjusted model to 0.0027 (*p* = .23,
non-significant) after adjusting for `self_control`. This illustrates
confounding: the simulation was designed so that the apparent
between-person harm of screen time is driven by self-control. In a real
study, a similar pattern would suggest that people with lower
self-control watch more TV on average *and* have lower wellbeing, and
that the self-control deficit — not screen time — explains the wellbeing
gap.

## Adding More Predictors

When you have several time-varying predictors, decompose all of them at
once:

``` r

diary_decomp2 <- decompose_within_between(
  data            = media_diary,
  group           = "person",
  vars            = c("screen_time", "stress"),
  components      = c("within", "between"),
  within_pattern  = "{col}_within",
  between_pattern = "{col}_between"
)

fit_multi <- lmer(
  wellbeing ~ screen_time_within + screen_time_between +
    stress_within + stress_between +
    self_control + (1 | person),
  data = diary_decomp2
)
```

Here `stress_within` captures whether more stressful days than usual
predict lower wellbeing on those days (within-person), while
`stress_between` captures whether chronically more stressed people have
lower wellbeing overall (between-person).

## Adding Random Slopes

The REWB model above assumes the within-person effect of screen time on
wellbeing is the same for all persons. You can allow this effect to vary
by adding a random slope:

``` r

fit_slopes <- lmer(
  wellbeing ~ screen_time_within + screen_time_between + self_control +
    (screen_time_within | person),
  data = diary_decomp
)
```

A significant random slope variance indicates that the within-person
association between screen time and wellbeing differs across persons —
for some, extra media use lifts their mood more than for others.

## Further Reading

This vignette covers the data-preparation and basic modelling side of
REWB analysis. For thorough treatments of model specification,
assumption checking, and interpretation — including cross-level
interactions — see Bell et al. (2019) and Enders & Tofighi (2007). For
descriptive statistics and correlation matrices that can inform REWB
model specification, see
[`vignette("multilevel-descriptives")`](https://felixdidi.github.io/mlstats/articles/multilevel-descriptives.md).

## References

Bell, A., Fairbrother, M., & Jones, K. (2019). Fixed and random effects
models: Making an informed choice. *Quality & Quantity, 53*(2),
1051–1074. <https://doi.org/10.1007/s11135-018-0802-x>

Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in
cross-sectional multilevel models: A new look at an old issue.
*Psychological Methods, 12*(2), 121–138.
<https://doi.org/10.1037/1082-989X.12.2.121>

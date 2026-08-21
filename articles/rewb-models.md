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
**mlstats** (100 participants asked to complete up to 14 daily surveys;
as in most real mobile diary studies, not everyone completed every day,
so *T* varies from 5 to 14 observations per person, 1,184 total across
the *N* = 100 persons). See
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
#> # A tibble: 1,184 × 2
#>    screen_time_between_person screen_time_within_person
#>                         <dbl>                     <dbl>
#>  1                         98                       -15
#>  2                         98                       -16
#>  3                         98                         5
#>  4                         98                         7
#>  5                         98                       -30
#>  6                         98                        45
#>  7                         98                        41
#>  8                         98                         7
#>  9                         98                       -23
#> 10                         98                       -43
#> # ℹ 1,174 more rows
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
#> # A tibble: 1,184 × 4
#>    screen_time screen_time_grand_mean_centered screen_time_between_person
#>          <dbl>                           <dbl>                      <dbl>
#>  1          83                          -49.0                          98
#>  2          82                          -50.0                          98
#>  3         103                          -29.0                          98
#>  4         105                          -27.0                          98
#>  5          68                          -64.0                          98
#>  6         143                           11.0                          98
#>  7         139                            6.95                         98
#>  8         105                          -27.0                          98
#>  9          75                          -57.0                          98
#> 10          55                          -77.0                          98
#> # ℹ 1,174 more rows
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
#> # A tibble: 1,184 × 3
#>    screen_time screen_time_bg screen_time_wg
#>          <dbl>          <dbl>          <dbl>
#>  1          83             98            -15
#>  2          82             98            -16
#>  3         103             98              5
#>  4         105             98              7
#>  5          68             98            -30
#>  6         143             98             45
#>  7         139             98             41
#>  8         105             98              7
#>  9          75             98            -23
#> 10          55             98            -43
#> # ℹ 1,174 more rows
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
#> Rows: 1,184
#> Columns: 10
#> $ person                     <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2…
#> $ self_control               <dbl> 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0…
#> $ wellbeing                  <dbl> 3.5, 4.0, 3.4, 3.7, 3.9, 4.3, 5.3, 3.5, 3.1…
#> $ screen_time                <dbl> 83, 82, 103, 105, 68, 143, 139, 105, 75, 55…
#> $ stress                     <dbl> 3.9, 4.4, 4.8, 4.7, 3.6, 5.3, 2.9, 4.7, 3.7…
#> $ enjoyment                  <dbl> 4.5, 3.9, 3.8, 4.6, 3.7, 5.0, 5.4, 4.2, 3.9…
#> $ screen_time_between_person <dbl> 98.0000, 98.0000, 98.0000, 98.0000, 98.0000…
#> $ stress_between_person      <dbl> 4.121429, 4.121429, 4.121429, 4.121429, 4.1…
#> $ screen_time_within_person  <dbl> -15.0000000, -16.0000000, 5.0000000, 7.0000…
#> $ stress_within_person       <dbl> -0.22142857, 0.27857143, 0.67857143, 0.5785…
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
#> REML criterion at convergence: 2311.7
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -3.5585 -0.6411 -0.0129  0.6385  2.7273 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev.
#>  person   (Intercept) 0.3626   0.6022  
#>  Residual             0.3235   0.5687  
#> Number of obs: 1184, groups:  person, 100
#> 
#> Fixed effects:
#>                       Estimate Std. Error         df t value Pr(>|t|)    
#> (Intercept)          5.548e+00  3.378e-01  9.855e+01   16.43  < 2e-16 ***
#> screen_time_within   8.397e-03  5.442e-04  1.083e+03   15.43  < 2e-16 ***
#> screen_time_between -8.487e-03  2.526e-03  9.834e+01   -3.36  0.00111 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

**Interpreting the coefficients:**

In this simulated dataset, the within-person coefficient (0.0084) is
positive and highly significant. To illustrate how such an effect would
be interpreted: on days when someone watches one minute more than their
own average, their wellbeing is 0.0084 points higher. For a person
watching 60 minutes more than usual, the expected gain would be 0.5
wellbeing points.

The between-person coefficient (-0.0085) is negative and significant.
Illustrating interpretation: people who watch one minute more per day on
average show 0.0085 lower wellbeing. For someone who watches 60 minutes
more per day on average than another person, the expected wellbeing gap
would be 0.51 points.

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
#> -2.91016 -0.60219 -0.01771  0.59952  2.57131 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 4.1887746  0.0907832  46.140  < 2e-16 ***
#> screen_time 0.0017710  0.0006594   2.686  0.00733 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.8847 on 1182 degrees of freedom
#> Multiple R-squared:  0.006067,   Adjusted R-squared:  0.005226 
#> F-statistic: 7.215 on 1 and 1182 DF,  p-value: 0.007332
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
#> REML criterion at convergence: 2295.7
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -3.5307 -0.6466 -0.0147  0.6388  2.7396 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev.
#>  person   (Intercept) 0.3001   0.5478  
#>  Residual             0.3233   0.5686  
#> Number of obs: 1184, groups:  person, 100
#> 
#> Fixed effects:
#>                      Estimate Std. Error        df t value Pr(>|t|)    
#> (Intercept)         2.512e+00  7.422e-01 9.953e+01   3.384  0.00102 ** 
#> screen_time_within  8.397e-03  5.440e-04 1.084e+03  15.435  < 2e-16 ***
#> screen_time_between 5.065e-04  3.058e-03 9.886e+01   0.166  0.86880    
#> self_control        4.713e-01  1.047e-01 9.984e+01   4.501 1.83e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

**Interpreting the coefficients:**

In this simulated dataset, the within-person coefficient is unchanged
(0.0084): `self_control` is a stable trait measured once per person, so
it carries no within-person variation and cannot alter the within-person
estimate. This is a general property of between-person covariates in
REWB models, not specific to these simulated data.

The between-person coefficient changes substantially — from -0.0085
(significant) in the unadjusted model to 5^{-4} (*p* = .23,
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

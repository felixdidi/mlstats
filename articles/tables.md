# Publication-Ready Tables

``` r

library(mlstats)
library(dplyr)
library(stringr)
```

[`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md)
(for any `method`, including `"bayes"`) returns a tibble that can be
printed in three formats: a console-friendly default, a **tinytable**
object, and a **gt** object. This vignette shows how to move from the
default output to a fully-customised, journal-ready table.

## Example data

We use `media_diary`, a simulated daily diary dataset included with
**mlstats** (100 participants over 14 days; *N* = 100 persons, *T* =
1,400 daily observations). See
[`?media_diary`](https://felixdidi.github.io/mlstats/reference/media_diary.md)
for details.

``` r

data("media_diary")
vars <- c("self_control", "wellbeing", "screen_time", "stress", "enjoyment")
```

``` r

result <- mldesc(data = media_diary, group = "person", vars = vars)
```

## Default console output

Simply printing the result gives a compact console-friendly view:

``` r

result
#> # Multilevel Descriptive Statistics
#>   ============ ===== ====== ===== ========= ===== ===== ===== ===== ===== =====
#>   variable     n_obs      m    sd     range   `1`   `2`   `3`   `4`   `5`   icc
#>   ------------ ----- ------ ----- --------- ----- ----- ----- ----- ----- -----
#> 1 Self control   100   4.03  0.83 1.60–5.80     –    NA    NA    NA    NA  1.00
#> 2 Wellbeing    1,400   4.45  0.87 1.50–6.90  .61*     –  .42* -.43*  .45*   .46
#> 3 Screen time  1,400 128.66 42.29     0–272 -.67* -.34*     –  .29*  .57*   .45
#> 4 Stress       1,400   3.81  0.91       1–7 -.53* -.38*  .38*     –  .05*   .33
#> 5 Enjoyment    1,400   4.44  0.79       2–7  -.18 -.21*  .22*  .25*     –   .44
#>   ============ ===== ====== ===== ========= ===== ===== ===== ===== ===== =====
#> # ℹ Within-person correlations above, between-person correlations below the
#> #   diagonal.
#> # ℹ All correlations marked with a star are significant at p < .05.
#> # ℹ Correlations estimated via variance decomposition.
#> # ℹ Group-weighted multilevel descriptive statistics computed with mlstats.
```

## tinytable format

`tinytable` is a lightweight table package included with **mlstats** (no
extra installation needed). Pass `format = "tt"` to
[`print()`](https://rdrr.io/r/base/print.html):

``` r

print(result, format = "tt")
```

|  |  | Descriptives |  |  |  | Correlations^(a,b) |  |  |  |  | ICC |
|----|----|----|----|----|----|----|----|----|----|----|----|
|  | Variable | *N*_(obs) | *M* | *SD* | Range | 1 | 2 | 3 | 4 | 5 |  |
| 1 | Self control | 100 | 4.03 | 0.83 | 1.60–5.80 | – | NA | NA | NA | NA | 1.00 |
| 2 | Wellbeing | 1,400 | 4.45 | 0.87 | 1.50–6.90 | .61\* | – | .42\* | -.43\* | .45\* | .46 |
| 3 | Screen time | 1,400 | 128.66 | 42.29 | 0–272 | -.67\* | -.34\* | – | .29\* | .57\* | .45 |
| 4 | Stress | 1,400 | 3.81 | 0.91 | 1–7 | -.53\* | -.38\* | .38\* | – | .05\* | .33 |
| 5 | Enjoyment | 1,400 | 4.44 | 0.79 | 2–7 | -.18 | -.21\* | .22\* | .25\* | – | .44 |
| *Note.* Group-weighted multilevel descriptive statistics computed with mlstats. |  |  |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person correlations below the diagonal. |  |  |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |  |  |

Multilevel Descriptive Statistics {#tinytable_hqj4w0ioy0lc1w6vygmi
.table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

The result is a `tinytable` object that renders to HTML, PDF, or Word
via Quarto/R Markdown (see below).

### Custom title and notes

All print methods accept `table_title`, `correlation_note`,
`significance_note`, and `note_text`:

``` r

print(result,
  format           = "tt",
  table_title      = "Daily diary study: descriptive statistics and multilevel correlations",
  correlation_note = "Within-person correlations above, between-person below the diagonal.",
  note_text        = "N = 100 persons, 14 daily observations each. Simulated data."
)
```

|  |  | Descriptives |  |  |  | Correlations^(a,b) |  |  |  |  | ICC |
|----|----|----|----|----|----|----|----|----|----|----|----|
|  | Variable | *N*_(obs) | *M* | *SD* | Range | 1 | 2 | 3 | 4 | 5 |  |
| 1 | Self control | 100 | 4.03 | 0.83 | 1.60–5.80 | – | NA | NA | NA | NA | 1.00 |
| 2 | Wellbeing | 1,400 | 4.45 | 0.87 | 1.50–6.90 | .61\* | – | .42\* | -.43\* | .45\* | .46 |
| 3 | Screen time | 1,400 | 128.66 | 42.29 | 0–272 | -.67\* | -.34\* | – | .29\* | .57\* | .45 |
| 4 | Stress | 1,400 | 3.81 | 0.91 | 1–7 | -.53\* | -.38\* | .38\* | – | .05\* | .33 |
| 5 | Enjoyment | 1,400 | 4.44 | 0.79 | 2–7 | -.18 | -.21\* | .22\* | .25\* | – | .44 |
| *Note.* N = 100 persons, 14 daily observations each. Simulated data. |  |  |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person below the diagonal. |  |  |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |  |  |

Daily diary study: descriptive statistics and multilevel correlations
{#tinytable_0fayebf9wmd8io9wpp61 .table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

## gt format

`gt` produces richly formatted HTML tables and supports markdown in
cells, footnotes, and fine typographic control. It must be installed
separately:

``` r

install.packages("gt")
```

``` r

print(result, format = "gt")
```

[TABLE]

`gt` tables support further customisation via the `gt` package API after
the initial [`print()`](https://rdrr.io/r/base/print.html) call — see
the [gt documentation](https://gt.rstudio.com/) for details.

## Manipulating the result before printing

Because
[`mldesc()`](https://felixdidi.github.io/mlstats/reference/mldesc.md)
returns a tibble, standard `dplyr` operations work on it before
printing.

### Removing columns

Drop columns you don’t need in the final table:

``` r

result |>
  select(-n_obs, -range) |>
  print(format = "tt")
```

|  |  | Descriptives |  | Correlations^(a,b) |  |  |  |  | ICC |
|----|----|----|----|----|----|----|----|----|----|
|  | Variable | *M* | *SD* | 1 | 2 | 3 | 4 | 5 |  |
| 1 | Self control | 4.03 | 0.83 | – | NA | NA | NA | NA | 1.00 |
| 2 | Wellbeing | 4.45 | 0.87 | .61\* | – | .42\* | -.43\* | .45\* | .46 |
| 3 | Screen time | 128.66 | 42.29 | -.67\* | -.34\* | – | .29\* | .57\* | .45 |
| 4 | Stress | 3.81 | 0.91 | -.53\* | -.38\* | .38\* | – | .05\* | .33 |
| 5 | Enjoyment | 4.44 | 0.79 | -.18 | -.21\* | .22\* | .25\* | – | .44 |
| *Note.* Group-weighted multilevel descriptive statistics computed with mlstats. |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person correlations below the diagonal. |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |

Multilevel Descriptive Statistics {#tinytable_zri9umrdtulfupqtq1uz
.table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

### Replacing NA with a dash

`self_control` is a between-person-only trait: its within-person
correlations are `NA`. Replace these with an em dash for cleaner output:

``` r

result |>
  mutate(across(everything(), ~ str_replace(as.character(.x), "^NA$", "–"))) |>
  print(format = "tt")
```

|  |  | Descriptives |  |  |  | Correlations^(a,b) |  |  |  |  | ICC |
|----|----|----|----|----|----|----|----|----|----|----|----|
|  | Variable | *N*_(obs) | *M* | *SD* | Range | 1 | 2 | 3 | 4 | 5 |  |
| 1 | Self control | 100 | 4.03 | 0.83 | 1.60–5.80 | – | – | – | – | – | 1.00 |
| 2 | Wellbeing | 1,400 | 4.45 | 0.87 | 1.50–6.90 | .61\* | – | .42\* | -.43\* | .45\* | .46 |
| 3 | Screen time | 1,400 | 128.66 | 42.29 | 0–272 | -.67\* | -.34\* | – | .29\* | .57\* | .45 |
| 4 | Stress | 1,400 | 3.81 | 0.91 | 1–7 | -.53\* | -.38\* | .38\* | – | .05\* | .33 |
| 5 | Enjoyment | 1,400 | 4.44 | 0.79 | 2–7 | -.18 | -.21\* | .22\* | .25\* | – | .44 |
| *Note.* Group-weighted multilevel descriptive statistics computed with mlstats. |  |  |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person correlations below the diagonal. |  |  |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |  |  |

Multilevel Descriptive Statistics {#tinytable_q82zfc3ctpf1mvkpz5em
.table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

### Renaming variables

Variable names are auto-formatted as sentence case. To customise them:

``` r

result |>
  mutate(variable = case_when(
    variable == "Self control" ~ "Trait self-control",
    variable == "Wellbeing"    ~ "Daily wellbeing",
    variable == "Screen time"  ~ "Screen time (min)",
    variable == "Stress"       ~ "Perceived stress",
    variable == "Enjoyment"    ~ "Media enjoyment"
  )) |>
  print(format = "tt", table_title = "Study variables: descriptive statistics")
```

|  |  | Descriptives |  |  |  | Correlations^(a,b) |  |  |  |  | ICC |
|----|----|----|----|----|----|----|----|----|----|----|----|
|  | Variable | *N*_(obs) | *M* | *SD* | Range | 1 | 2 | 3 | 4 | 5 |  |
| 1 | Trait self-control | 100 | 4.03 | 0.83 | 1.60–5.80 | – | NA | NA | NA | NA | 1.00 |
| 2 | Daily wellbeing | 1,400 | 4.45 | 0.87 | 1.50–6.90 | .61\* | – | .42\* | -.43\* | .45\* | .46 |
| 3 | Screen time (min) | 1,400 | 128.66 | 42.29 | 0–272 | -.67\* | -.34\* | – | .29\* | .57\* | .45 |
| 4 | Perceived stress | 1,400 | 3.81 | 0.91 | 1–7 | -.53\* | -.38\* | .38\* | – | .05\* | .33 |
| 5 | Media enjoyment | 1,400 | 4.44 | 0.79 | 2–7 | -.18 | -.21\* | .22\* | .25\* | – | .44 |
| *Note.* Group-weighted multilevel descriptive statistics computed with mlstats. |  |  |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person correlations below the diagonal. |  |  |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |  |  |

Study variables: descriptive statistics {#tinytable_5xd9n7up7icpcob4k5bs
.table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

### Combining manipulations

All of the above can be chained. Here is an example of a polished table
combining several customisations:

``` r

result |>
  select(-n_obs, -range) |>
  mutate(across(everything(), ~ str_replace(as.character(.x), "^NA$", "–"))) |>
  mutate(variable = case_when(
    variable == "Self control" ~ "Trait self-control",
    variable == "Wellbeing"    ~ "Daily wellbeing",
    variable == "Screen time"  ~ "Screen time (min)",
    variable == "Stress"       ~ "Perceived stress",
    variable == "Enjoyment"    ~ "Media enjoyment"
  )) |>
  print(
    format           = "tt",
    table_title      = "Descriptive statistics and multilevel correlations",
    correlation_note = "Within-person correlations above, between-person below the diagonal.",
    note_text        = "N = 100, T = 1,400 daily observations. Self-control was measured as a trait (between-person only)."
  )
```

|  |  | Descriptives |  | Correlations^(a,b) |  |  |  |  | ICC |
|----|----|----|----|----|----|----|----|----|----|
|  | Variable | *M* | *SD* | 1 | 2 | 3 | 4 | 5 |  |
| 1 | Trait self-control | 4.03 | 0.83 | – | – | – | – | – | 1.00 |
| 2 | Daily wellbeing | 4.45 | 0.87 | .61\* | – | .42\* | -.43\* | .45\* | .46 |
| 3 | Screen time (min) | 128.66 | 42.29 | -.67\* | -.34\* | – | .29\* | .57\* | .45 |
| 4 | Perceived stress | 3.81 | 0.91 | -.53\* | -.38\* | .38\* | – | .05\* | .33 |
| 5 | Media enjoyment | 4.44 | 0.79 | -.18 | -.21\* | .22\* | .25\* | – | .44 |
| *Note.* N = 100, T = 1,400 daily observations. Self-control was measured as a trait (between-person only). |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person below the diagonal. |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |

Descriptive statistics and multilevel correlations
{#tinytable_hhvqo0accobap1r92a8r .table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

For the equivalent using `gt` (which additionally supports footnotes and
markdown-formatted cell content):

``` r

result |>
  select(-n_obs, -range) |>
  mutate(across(everything(), ~ str_replace(as.character(.x), "^NA$", "–"))) |>
  mutate(
    variable = case_when(
      variable == "Self control" ~ "Trait self-control<sup>c</sup>",
      variable == "Wellbeing"    ~ "Daily wellbeing",
      variable == "Screen time"  ~ "Screen time (min)",
      variable == "Stress"       ~ "Perceived stress",
      variable == "Enjoyment"    ~ "Media enjoyment"
    )
  ) |>
  print(
    format           = "gt",
    table_title      = "Descriptive statistics and multilevel correlations",
    correlation_note = "Within-person correlations above, between-person below the diagonal.",
    note_text        = "<i>Note</i>. <i>N</i> = 100, <i>T</i> = 1,400 daily observations."
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      "<sup>c</sup> Self-control was measured as a stable trait; no within-person correlations are available."
    )
  ) |>
  gt::fmt_markdown(columns = variable)
```

[TABLE]

## Embedding in Quarto documents

### Word / DOCX output

Wrap the [`print()`](https://rdrr.io/r/base/print.html) call in a Quarto
code chunk with `format: docx`:

```` default
---
format: docx
---

```{r}
library(mlstats)
data("media_diary")

mldesc(
  data  = media_diary,
  group = "person",
  vars  = c("self_control", "wellbeing", "screen_time", "stress")
) |>
  print(format = "tt")
```
````

`tinytable` automatically converts to the appropriate format based on
the output Quarto is rendering to.

### HTML / PDF

Both `tinytable` and `gt` render natively to HTML and LaTeX. No extra
setup is needed:

```` default
---
format: html   # or pdf
---

```{r}
library(mlstats)
data("media_diary")

mldesc(
  data  = media_diary,
  group = "person",
  vars  = c("self_control", "wellbeing", "screen_time", "stress")
) |>
  print(format = "tt")
```
````

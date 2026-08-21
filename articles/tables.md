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
**mlstats** (100 participants asked to complete up to 14 daily surveys;
as in most real mobile diary studies, not everyone completed every day,
so *T* varies from 5 to 14 observations per person, 1,184 total across
the *N* = 100 persons). See
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
#> 1 Self control   100   3.93  0.70 2.40–6.20     –    NA    NA    NA    NA  1.00
#> 2 Wellbeing    1,184   4.42  0.89 1.50–7.00  .47*     –  .42* -.40*  .51*   .50
#> 3 Screen time  1,184 132.05 39.01    15–246 -.66* -.32*     –  .29*  .54*   .34
#> 4 Stress       1,184   3.77  0.95       1–7 -.55* -.27*  .46*     –  -.02   .39
#> 5 Enjoyment    1,184   4.49  0.83 2.30–7.00  -.12  .40*  .32*   .15     –   .50
#>   ============ ===== ====== ===== ========= ===== ===== ===== ===== ===== =====
#> # ℹ Within-person correlations above, between-person correlations below the
#> #   diagonal.
#> # ℹ All correlations marked with a star are significant at p < .05.
#> # ℹ Based on 100 persons and 1,184 observations (median 12 per person; range:
#> #   5–14).
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
| 1 | Self control | 100 | 3.93 | 0.70 | 2.40–6.20 | – | NA | NA | NA | NA | 1.00 |
| 2 | Wellbeing | 1,184 | 4.42 | 0.89 | 1.50–7.00 | .47\* | – | .42\* | -.40\* | .51\* | .50 |
| 3 | Screen time | 1,184 | 132.05 | 39.01 | 15–246 | -.66\* | -.32\* | – | .29\* | .54\* | .34 |
| 4 | Stress | 1,184 | 3.77 | 0.95 | 1–7 | -.55\* | -.27\* | .46\* | – | -.02 | .39 |
| 5 | Enjoyment | 1,184 | 4.49 | 0.83 | 2.30–7.00 | -.12 | .40\* | .32\* | .15 | – | .50 |
| *Note.* Group-weighted multilevel descriptive statistics computed with mlstats. |  |  |  |  |  |  |  |  |  |  |  |
|  Based on 100 persons and 1,184 observations (median 12 per person; range: 5–14). |  |  |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person correlations below the diagonal. |  |  |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |  |  |

Multilevel Descriptive Statistics {#tinytable_v1za8twxj4lzildkw72t
.table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

The result is a `tinytable` object that renders to HTML, PDF, or Word
via Quarto/R Markdown (see below).

### Custom title and notes

All print methods accept `table_title`, `correlation_note`,
`significance_note`, `group_size_note`, and `note_text`:

``` r

print(result,
  format           = "tt",
  table_title      = "Daily diary study: descriptive statistics and multilevel correlations",
  correlation_note = "Within-person correlations above, between-person below the diagonal.",
  group_size_note  = "Based on 100 persons; observations per person varied due to non-response.",
  note_text        = "Simulated data."
)
```

|  |  | Descriptives |  |  |  | Correlations^(a,b) |  |  |  |  | ICC |
|----|----|----|----|----|----|----|----|----|----|----|----|
|  | Variable | *N*_(obs) | *M* | *SD* | Range | 1 | 2 | 3 | 4 | 5 |  |
| 1 | Self control | 100 | 3.93 | 0.70 | 2.40–6.20 | – | NA | NA | NA | NA | 1.00 |
| 2 | Wellbeing | 1,184 | 4.42 | 0.89 | 1.50–7.00 | .47\* | – | .42\* | -.40\* | .51\* | .50 |
| 3 | Screen time | 1,184 | 132.05 | 39.01 | 15–246 | -.66\* | -.32\* | – | .29\* | .54\* | .34 |
| 4 | Stress | 1,184 | 3.77 | 0.95 | 1–7 | -.55\* | -.27\* | .46\* | – | -.02 | .39 |
| 5 | Enjoyment | 1,184 | 4.49 | 0.83 | 2.30–7.00 | -.12 | .40\* | .32\* | .15 | – | .50 |
| *Note.* Simulated data. |  |  |  |  |  |  |  |  |  |  |  |
|  Based on 100 persons; observations per person varied due to non-response. |  |  |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person below the diagonal. |  |  |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |  |  |

Daily diary study: descriptive statistics and multilevel correlations
{#tinytable_k1yh8767cfa5jezzk0j9 .table .tinytable
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
| 1 | Self control | 3.93 | 0.70 | – | NA | NA | NA | NA | 1.00 |
| 2 | Wellbeing | 4.42 | 0.89 | .47\* | – | .42\* | -.40\* | .51\* | .50 |
| 3 | Screen time | 132.05 | 39.01 | -.66\* | -.32\* | – | .29\* | .54\* | .34 |
| 4 | Stress | 3.77 | 0.95 | -.55\* | -.27\* | .46\* | – | -.02 | .39 |
| 5 | Enjoyment | 4.49 | 0.83 | -.12 | .40\* | .32\* | .15 | – | .50 |
| *Note.* Group-weighted multilevel descriptive statistics computed with mlstats. |  |  |  |  |  |  |  |  |  |
|  Based on 100 persons and 1,184 observations (median 12 per person; range: 5–14). |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person correlations below the diagonal. |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |

Multilevel Descriptive Statistics {#tinytable_hqx4zsylnm9ojvbi6c8t
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
| 1 | Self control | 100 | 3.93 | 0.70 | 2.40–6.20 | – | – | – | – | – | 1.00 |
| 2 | Wellbeing | 1,184 | 4.42 | 0.89 | 1.50–7.00 | .47\* | – | .42\* | -.40\* | .51\* | .50 |
| 3 | Screen time | 1,184 | 132.05 | 39.01 | 15–246 | -.66\* | -.32\* | – | .29\* | .54\* | .34 |
| 4 | Stress | 1,184 | 3.77 | 0.95 | 1–7 | -.55\* | -.27\* | .46\* | – | -.02 | .39 |
| 5 | Enjoyment | 1,184 | 4.49 | 0.83 | 2.30–7.00 | -.12 | .40\* | .32\* | .15 | – | .50 |
| *Note.* Group-weighted multilevel descriptive statistics computed with mlstats. |  |  |  |  |  |  |  |  |  |  |  |
|  Based on 100 persons and 1,184 observations (median 12 per person; range: 5–14). |  |  |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person correlations below the diagonal. |  |  |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |  |  |

Multilevel Descriptive Statistics {#tinytable_vwdemh488c92jg97qw6t
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
| 1 | Trait self-control | 100 | 3.93 | 0.70 | 2.40–6.20 | – | NA | NA | NA | NA | 1.00 |
| 2 | Daily wellbeing | 1,184 | 4.42 | 0.89 | 1.50–7.00 | .47\* | – | .42\* | -.40\* | .51\* | .50 |
| 3 | Screen time (min) | 1,184 | 132.05 | 39.01 | 15–246 | -.66\* | -.32\* | – | .29\* | .54\* | .34 |
| 4 | Perceived stress | 1,184 | 3.77 | 0.95 | 1–7 | -.55\* | -.27\* | .46\* | – | -.02 | .39 |
| 5 | Media enjoyment | 1,184 | 4.49 | 0.83 | 2.30–7.00 | -.12 | .40\* | .32\* | .15 | – | .50 |
| *Note.* Group-weighted multilevel descriptive statistics computed with mlstats. |  |  |  |  |  |  |  |  |  |  |  |
|  Based on 100 persons and 1,184 observations (median 12 per person; range: 5–14). |  |  |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person correlations below the diagonal. |  |  |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |  |  |

Study variables: descriptive statistics {#tinytable_qv0940x0j7uty51u2jqf
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
    note_text        = "N = 100, T = 5-14 daily observations per person (1,184 total). Self-control was measured as a trait (between-person only)."
  )
```

|  |  | Descriptives |  | Correlations^(a,b) |  |  |  |  | ICC |
|----|----|----|----|----|----|----|----|----|----|
|  | Variable | *M* | *SD* | 1 | 2 | 3 | 4 | 5 |  |
| 1 | Trait self-control | 3.93 | 0.70 | – | – | – | – | – | 1.00 |
| 2 | Daily wellbeing | 4.42 | 0.89 | .47\* | – | .42\* | -.40\* | .51\* | .50 |
| 3 | Screen time (min) | 132.05 | 39.01 | -.66\* | -.32\* | – | .29\* | .54\* | .34 |
| 4 | Perceived stress | 3.77 | 0.95 | -.55\* | -.27\* | .46\* | – | -.02 | .39 |
| 5 | Media enjoyment | 4.49 | 0.83 | -.12 | .40\* | .32\* | .15 | – | .50 |
| *Note.* N = 100, T = 5-14 daily observations per person (1,184 total). Self-control was measured as a trait (between-person only). |  |  |  |  |  |  |  |  |  |
|  Based on 100 persons and 1,184 observations (median 12 per person; range: 5–14). |  |  |  |  |  |  |  |  |  |
| ^(a) Within-person correlations above, between-person below the diagonal. |  |  |  |  |  |  |  |  |  |
| ^(b) All correlations marked with a star are significant at p \< .05. |  |  |  |  |  |  |  |  |  |

Descriptive statistics and multilevel correlations
{#tinytable_dwcob70t06qyuym3k7bm .table .tinytable
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
    note_text        = "<i>Note</i>. <i>N</i> = 100, <i>T</i> = 5–14 daily observations per person (1,184 total)."
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

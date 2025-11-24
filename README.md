
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlstats <img src="man/img/sticker.png" align="right" height="138" /></a>

<!-- badges: start -->

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

## Usage

### Multilevel Descriptives

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
    vars = vars,
    remove_leading_zero = FALSE
  )
#> # A tibble: 4 × 10
#>   variable               n_obs m     sd    range `1`   `2`   `3`   `4`   icc  
#>   <chr>                  <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#> 1 Disconnective behavior 12407 0.38  0.39  0-1   –     0.12* 0.02  0.15* 0.45 
#> 2 Distraction motivation 7360  5.11  2.02  1-7   0.10  –     0.13* 0.20* 0.27 
#> 3 Wellbeing motivation   7360  3.59  2.20  1-7   0.23* 0.41* –     0.10* 0.50 
#> 4 Presence motivation    7360  4.61  2.21  1-7   0.18* 0.39* 0.32* –     0.31
```

### Weighting by Group Size

By default, `mldesc()` weights the group means, standard deviations, and
between-group correlations by group size. This can be disabled by
setting `weight = FALSE` (e.g., so that each person in the sample
contributes equally to the overall mean as well as to the between-person
correlations):

``` r
data |>
  mldesc(
    group = "person",
    vars = vars,
    weight = FALSE,
    remove_leading_zero = FALSE
  )
#> # A tibble: 4 × 10
#>   variable               n_obs m     sd    range `1`   `2`   `3`   `4`   icc  
#>   <chr>                  <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#> 1 Disconnective behavior 12407 0.38  0.27  0-1   –     0.12* 0.02  0.15* 0.45 
#> 2 Distraction motivation 7360  5.09  1.19  1-7   0.12  –     0.13* 0.20* 0.27 
#> 3 Wellbeing motivation   7360  3.48  1.58  1-7   0.25* 0.39* –     0.10* 0.50 
#> 4 Presence motivation    7360  4.50  1.39  1-7   0.19* 0.38* 0.33* –     0.31
```

### Pretty Tables

By setting `print_gt = TRUE`, it is also easy to create a nicely
formatted table that looks great in your Quarto document and can be
directly copied into a manuscript. Here, we use the default setting of
`remove_leading_zero = TRUE` to comply with APA formatting guidelines:

``` r
data |> 
  mldesc(
    group = "person",
    vars = vars,
    print_gt = TRUE,
    table_title = "Descriptive statistics, within- and between-person correlations of digital disconnection and motivations"
  )
```

<div id="kelxyravjc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#kelxyravjc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#kelxyravjc thead, #kelxyravjc tbody, #kelxyravjc tfoot, #kelxyravjc tr, #kelxyravjc td, #kelxyravjc th {
  border-style: none;
}
&#10;#kelxyravjc p {
  margin: 0;
  padding: 0;
}
&#10;#kelxyravjc .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 99%;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #FFFFFF;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#kelxyravjc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#kelxyravjc .gt_title {
  color: #333333;
  font-size: 16px;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#kelxyravjc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#kelxyravjc .gt_heading {
  background-color: #FFFFFF;
  text-align: left;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#kelxyravjc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#kelxyravjc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#kelxyravjc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#kelxyravjc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#kelxyravjc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#kelxyravjc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#kelxyravjc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#kelxyravjc .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#kelxyravjc .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#kelxyravjc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#kelxyravjc .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#kelxyravjc .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#kelxyravjc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#kelxyravjc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#kelxyravjc .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#kelxyravjc .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#kelxyravjc .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#kelxyravjc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#kelxyravjc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#kelxyravjc .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#kelxyravjc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#kelxyravjc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#kelxyravjc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#kelxyravjc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#kelxyravjc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#kelxyravjc .gt_table_body {
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #FFFFFF;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #000000;
}
&#10;#kelxyravjc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#kelxyravjc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#kelxyravjc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#kelxyravjc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#kelxyravjc .gt_left {
  text-align: left;
}
&#10;#kelxyravjc .gt_center {
  text-align: center;
}
&#10;#kelxyravjc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#kelxyravjc .gt_font_normal {
  font-weight: normal;
}
&#10;#kelxyravjc .gt_font_bold {
  font-weight: bold;
}
&#10;#kelxyravjc .gt_font_italic {
  font-style: italic;
}
&#10;#kelxyravjc .gt_super {
  font-size: 65%;
}
&#10;#kelxyravjc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#kelxyravjc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#kelxyravjc .gt_indent_1 {
  text-indent: 5px;
}
&#10;#kelxyravjc .gt_indent_2 {
  text-indent: 10px;
}
&#10;#kelxyravjc .gt_indent_3 {
  text-indent: 15px;
}
&#10;#kelxyravjc .gt_indent_4 {
  text-indent: 20px;
}
&#10;#kelxyravjc .gt_indent_5 {
  text-indent: 25px;
}
&#10;#kelxyravjc .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#kelxyravjc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="true" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><b>Table.</b> Descriptive statistics, within- and between-person correlations of digital disconnection and motivations</td>
    </tr>
    &#10;    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="a::stub"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="variable">Variable</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="Descriptives">
        <div class="gt_column_spanner">Descriptives</div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="Correlations&lt;sup&gt;a,b&lt;/sup&gt;">
        <div class="gt_column_spanner">Correlations<sup>a,b</sup></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1" scope="col" id="ICC ">
        <div class="gt_column_spanner">ICC </div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n_obs"><i>N</i><sub>obs</sub></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="m"><i>M</i></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="sd"><i>SD</i></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="range">Range</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="a1">1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="a2">2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="a3">3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="a4">4</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="icc"> </th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_center gt_stub" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">1</th>
<td headers="stub_1_1 variable" class="gt_row gt_left" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">Disconnective behavior</td>
<td headers="stub_1_1 n_obs" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">12407</td>
<td headers="stub_1_1 m" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">0.38</td>
<td headers="stub_1_1 sd" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">0.39</td>
<td headers="stub_1_1 range" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">0-1</td>
<td headers="stub_1_1 1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">–</td>
<td headers="stub_1_1 2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.12*</td>
<td headers="stub_1_1 3" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.02</td>
<td headers="stub_1_1 4" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.15*</td>
<td headers="stub_1_1 icc" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.45</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_center gt_stub" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">2</th>
<td headers="stub_1_2 variable" class="gt_row gt_left" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">Distraction motivation</td>
<td headers="stub_1_2 n_obs" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">7360</td>
<td headers="stub_1_2 m" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">5.11</td>
<td headers="stub_1_2 sd" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">2.02</td>
<td headers="stub_1_2 range" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">1-7</td>
<td headers="stub_1_2 1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.10</td>
<td headers="stub_1_2 2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">–</td>
<td headers="stub_1_2 3" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.13*</td>
<td headers="stub_1_2 4" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.20*</td>
<td headers="stub_1_2 icc" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.27</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_center gt_stub" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">3</th>
<td headers="stub_1_3 variable" class="gt_row gt_left" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">Wellbeing motivation</td>
<td headers="stub_1_3 n_obs" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">7360</td>
<td headers="stub_1_3 m" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">3.59</td>
<td headers="stub_1_3 sd" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">2.20</td>
<td headers="stub_1_3 range" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">1-7</td>
<td headers="stub_1_3 1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.23*</td>
<td headers="stub_1_3 2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.41*</td>
<td headers="stub_1_3 3" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">–</td>
<td headers="stub_1_3 4" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.10*</td>
<td headers="stub_1_3 icc" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.50</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_center gt_stub" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">4</th>
<td headers="stub_1_4 variable" class="gt_row gt_left" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">Presence motivation</td>
<td headers="stub_1_4 n_obs" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">7360</td>
<td headers="stub_1_4 m" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">4.61</td>
<td headers="stub_1_4 sd" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">2.21</td>
<td headers="stub_1_4 range" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">1-7</td>
<td headers="stub_1_4 1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.18*</td>
<td headers="stub_1_4 2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.39*</td>
<td headers="stub_1_4 3" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.32*</td>
<td headers="stub_1_4 4" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">–</td>
<td headers="stub_1_4 icc" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white; border-left-width: 1px; border-left-style: solid; border-left-color: white; border-right-width: 1px; border-right-style: solid; border-right-color: white; background-color: #FFFFFF;">.31</td></tr>
  </tbody>
  <tfoot>
    <tr class="gt_sourcenotes">
      <td class="gt_sourcenote" colspan="11"><sup>a</sup> Within-group correlations depicted above, between-group correlations below the diagonal.<br><sup>b</sup> All correlations marked with a star are significant at <i>p</i> < .05.<br><br><i>Note.</i> Multilevel descriptive statistics computed with mlstats.</td>
    </tr>
  </tfoot>
</table>
</div>

### Centering

We can also simply center variables for further use with the
`decompose_within_between()` function. This centering approach is
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

### Bayesian Estimation

If you really want to do this, the package also supports Bayesian
estimation via `brms`, providing credible intervals instead of
*p*-values through `bayes_mldesc()` and
`bayes_within_between_correlations()`. In addition to the parameters
available in the frequentist functions, users must specify the credible
interval width (`ci`) and a folder to save the fitted models (`folder`).

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

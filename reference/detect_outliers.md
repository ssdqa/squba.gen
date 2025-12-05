# Hotspots Anomaly Detection

This function will identify anomalies in a dataframe using the
[`hotspots::outliers()`](https://rdrr.io/pkg/hotspots/man/hotspots.html)
function. It assumes: (1) No time component; (2) Table has a column
indicating whether a particular group or row is eligible for analysis;
(3) numerical variable exists that should be used for anomaly detection
analysis These conditions are met by the output of
[`compute_dist_anomalies()`](https://ssdqa.github.io/squba.gen/reference/compute_dist_anomalies.md),
which is typically the input for this function

## Usage

``` r
detect_outliers(
  df_tbl,
  tail_input = "both",
  p_input = 0.9,
  column_analysis,
  column_variable,
  column_eligible = "analysis_eligible"
)
```

## Arguments

- df_tbl:

  *tabular input* \|\| **required**

  A table meeting the previously described criteria. This input will
  typically be the table output by
  [`compute_dist_anomalies()`](https://ssdqa.github.io/squba.gen/reference/compute_dist_anomalies.md)

- tail_input:

  *string* \|\| defaults to `both`

  A string indicating whether outliers are identified for positive
  values, negative values, or both.

  Acceptable inputs are `positive`, `negative`, or `both`

- p_input:

  *numeric* \|\| defaults to `0.9`

  The p-value threshold that should be used to identify anomalies

- column_analysis:

  *string* \|\| **required**

  The name of the numerical column that is the target of the anomaly
  detection analysis

- column_variable:

  *string* \|\| **required**

  The name of the column with the variable to which the analysis column
  is related (ex: concept_id for the Concept Set Distribution module)

- column_eligible:

  *string* \|\| defaults to `analysis_eligible`

  The name of the column that indicates eligibility for analysis

## Examples

``` r
# create sample input (modeled after EVP)
sample_ms_input <- dplyr::tibble('site' = c('Site A', 'Site A', 'Site A',
                                            'Site A', 'Site B', 'Site B',
                                            'Site B', 'Site B'),
                                 'variable' = c('dx', 'dx', 'drug', 'drug',
                                                'dx', 'dx', 'drug', 'drug'),
                                 'count' = c(100, 140, 39, 42, 137, 111,
                                             12, 15),
                                 'total_var' = c(1000, 1000, 200, 200, 1500,
                                                 1500, 100, 100))
# execute the full analysis, including compute_dist_anomalies and
# detect_outliers
anomaly_output1 <- compute_dist_anomalies(df_tbl = sample_ms_input,
                                          grp_vars = 'variable',
                                          var_col = 'count',
                                          denom_cols = 'total_var')
#> Joining with `by = join_by(site)`
#> Joining with `by = join_by(site, variable, total_var)`

anomaly_output1
#> # A tibble: 12 × 14
#>    site   variable total_var count mean_val median_val sd_val mad_val cov_val
#>    <chr>  <chr>        <dbl> <dbl>    <dbl>      <dbl>  <dbl>   <dbl>   <dbl>
#>  1 Site A dx            1000   100     81.3      106.    64.8    48.9   0.797
#>  2 Site A dx            1000   140     81.3      106.    64.8    48.9   0.797
#>  3 Site A dx             200     0     81.3      106.    64.8    48.9   0.797
#>  4 Site A drug          1000     0     18         13.5   18.5    20.0   1.03 
#>  5 Site A drug           200    39     18         13.5   18.5    20.0   1.03 
#>  6 Site A drug           200    42     18         13.5   18.5    20.0   1.03 
#>  7 Site B dx            1500   137     81.3      106.    64.8    48.9   0.797
#>  8 Site B dx            1500   111     81.3      106.    64.8    48.9   0.797
#>  9 Site B dx             100     0     81.3      106.    64.8    48.9   0.797
#> 10 Site B drug          1500     0     18         13.5   18.5    20.0   1.03 
#> 11 Site B drug           100    12     18         13.5   18.5    20.0   1.03 
#> 12 Site B drug           100    15     18         13.5   18.5    20.0   1.03 
#> # ℹ 5 more variables: max_val <dbl>, min_val <dbl>, range_val <dbl>,
#> #   total_ct <int>, analysis_eligible <chr>

anomaly_output2 <- detect_outliers(df_tbl = anomaly_output1,
                                   column_analysis = 'count',
                                   column_variable = 'variable')
#> Joining with `by = join_by(site, variable, total_var, count, mean_val,
#> median_val, sd_val, mad_val, cov_val, max_val, min_val, range_val, total_ct,
#> analysis_eligible)`

anomaly_output2
#> # A tibble: 12 × 17
#>    site   variable total_var count mean_val median_val sd_val mad_val cov_val
#>    <chr>  <chr>        <dbl> <dbl>    <dbl>      <dbl>  <dbl>   <dbl>   <dbl>
#>  1 Site A dx            1000   100     81.3      106.    64.8    48.9   0.797
#>  2 Site A dx            1000   140     81.3      106.    64.8    48.9   0.797
#>  3 Site A dx             200     0     81.3      106.    64.8    48.9   0.797
#>  4 Site A drug          1000     0     18         13.5   18.5    20.0   1.03 
#>  5 Site A drug           200    39     18         13.5   18.5    20.0   1.03 
#>  6 Site A drug           200    42     18         13.5   18.5    20.0   1.03 
#>  7 Site B dx            1500   137     81.3      106.    64.8    48.9   0.797
#>  8 Site B dx            1500   111     81.3      106.    64.8    48.9   0.797
#>  9 Site B dx             100     0     81.3      106.    64.8    48.9   0.797
#> 10 Site B drug          1500     0     18         13.5   18.5    20.0   1.03 
#> 11 Site B drug           100    12     18         13.5   18.5    20.0   1.03 
#> 12 Site B drug           100    15     18         13.5   18.5    20.0   1.03 
#> # ℹ 8 more variables: max_val <dbl>, min_val <dbl>, range_val <dbl>,
#> #   total_ct <int>, analysis_eligible <chr>, lower_tail <dbl>,
#> #   upper_tail <dbl>, anomaly_yn <chr>
```

# Hotspots Anomaly Detection Eligibility Determination

This function will, for each group in a dataframe, identify groups that
are eligible for anomaly detection analysis by examining the values in
the `var_col`. The following conditions will disqualify a group from the
anomaly detection analysis: (1) Mean \< 0.02 or Median \< 0.01 (2) Mean
value \< 0.05 and range \< 0.01 (3) Coefficient of variance \< 0.1 and
sample size \< 11 If no groups meet this criteria, a warning will
display in the console indicating that no groups were eligible.

## Usage

``` r
compute_dist_anomalies(df_tbl, grp_vars, var_col, denom_cols)
```

## Arguments

- df_tbl:

  *tabular input* \|\| **required**

  A dataframe with at least one numerical variable & any relevant
  variables needed for grouping

- grp_vars:

  *string or vector* \|\| **required**

  The variable(s) to be used as grouping variables in the analysis

- var_col:

  *string* \|\| **required**

  The variable with the numerical statistic of interest for the
  euclidean distance computation

- denom_cols:

  *string or vector* \|\| **required**

  The variable containing a denominator or any other variables that
  should be preserved without nulls after a cross_join takes place

## Value

This function will return the original `df_tbl` with the addition of the
summary statistics used in the eligibility computation and a flag
indicating whether a given variable (based on the grp_vars) is eligible
for anomaly detection analysis. This table can then be passed into
`detect_outliers` to identify anomalous values.

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

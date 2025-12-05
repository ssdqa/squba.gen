# Compute Distance from Mean & Median

This function will, for the values in a given `var_col`, compute the
distance between that value and the mean & median values for each group
established by the `grp_vars` parameter.

## Usage

``` r
compute_dist_mean_median(tbl, grp_vars, var_col, num_sd, num_mad)
```

## Arguments

- tbl:

  *tabular input* \|\| **required**

  A table with the target numerical variable (`var_col`) for the
  analysis and each of the required grouping variables (`grp_vars`)

- grp_vars:

  *string or vector* \|\| **required**

  The name of the variable(s) to group by when computing summary
  statistics

- var_col:

  *string* \|\| **required**

  The name of the numeric variable that should be the target of summary
  statistic computations

- num_sd:

  *integer* \|\| **required**

  The number of standard deviations away from the mean that should be
  used as the lower and upper bounds for outlier detection. Values
  falling, for example, over 2 standard deviations above or below the
  mean would be considered outliers.

- num_mad:

  *integer* \|\| **required**

  The number of median absolute deviations (MADs) away from the median
  that should be used as lower and upper bounds. Outliers are not
  formally identified based on the median, but the information will be
  available in the final table should you prefer that method.

## Value

A table where, for each group in `grp_vars`, various summary statistics
like the mean, median, standard deviation, MAD, and others, are computed
based on the `var_col`. Outliers are identified in the `anomaly_yn`
column based on whether the data point is +/- `num_sd` from the mean or
the data point is \> the 90th percentile.

## Examples

``` r
# sample input table
sample_input <- dplyr::tibble('variable' = c('scd', 'scd', 'scd', 'scd'),
                              'site' = c('Site A', 'Site A', 'Site B',
                                         'Site B'),
                              'count' = c(15, 24, 100, 93))

# execute function against sample data
compute_dist_mean_median(tbl = sample_input,
                         grp_vars = 'variable',
                         var_col = 'count',
                         num_sd = 1,
                         num_mad = 1)
#> Joining with `by = join_by(site, variable)`
#> Joining with `by = join_by(variable)`
#> # A tibble: 4 × 16
#>   site   variable count  mean median    sd   mad `90th_percentile` sd_lower
#>   <chr>  <chr>    <dbl> <dbl>  <dbl> <dbl> <dbl>             <dbl>    <dbl>
#> 1 Site A scd         15    58   58.5  44.7  56.3              99.0     13.3
#> 2 Site A scd         24    58   58.5  44.7  56.3              99.0     13.3
#> 3 Site B scd        100    58   58.5  44.7  56.3              99.0     13.3
#> 4 Site B scd         93    58   58.5  44.7  56.3              99.0     13.3
#> # ℹ 7 more variables: sd_upper <dbl>, mad_lower <dbl>, mad_upper <dbl>,
#> #   anomaly_yn <lgl>, abs_diff_mean <dbl>, abs_diff_median <dbl>, n_mad <dbl>
```

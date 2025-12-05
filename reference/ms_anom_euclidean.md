# Euclidean Distance Computation

This function will compute the Euclidean Distance for the `var_col` at
each site in comparison to the overall, all-site mean. This is the
backend for most of the Multi Site, Anomaly Detection, Longitudinal
analyses.

## Usage

``` r
ms_anom_euclidean(fot_input_tbl, grp_vars, var_col)
```

## Arguments

- fot_input_tbl:

  *tabular input* \|\| **required**

  A table, typically output by
  [`compute_fot()`](https://ssdqa.github.io/squba.gen/reference/compute_fot.md)

- grp_vars:

  *string or vector* \|\| **required**

  The variable(s) to be used as grouping variables in the analysis.
  These variables will also be preserved in the cross-join, meaning
  there should not be any NAs as an artifact of the join for these
  variables.

- var_col:

  *string* \|\| **required**

  The variable with the numerical statistic of interest for the
  euclidean distance computation

## Value

This function will return the original data frame, where any time
periods without data are filled in with 0s, with mean and median values
for the `var_col` and the euclidean distance value based on the all-site
mean

## Examples

``` r
# sample multi-site, longitudinal input data (modeled after EVP)
sample_ms_la_input <- dplyr::tibble('variable' = c('scd', 'scd', 'scd',
                                                   'scd', 'scd', 'scd',
                                                   'scd', 'scd', 'scd',
                                                   'scd', 'scd', 'scd',
                                                   'scd', 'scd'),
                             'site' = c('Site A','Site A','Site A',
                                        'Site A','Site A','Site A',
                                        'Site A','Site B','Site B',
                                        'Site B','Site B','Site B',
                                        'Site B','Site B'),
                             'count' = c(15, 24, 100, 93, 47, 65,
                                         33, 92, 153, 122, 5, 99,
                                         10, 30),
                             'time_start'=c('2018-01-01','2019-01-01',
                                   '2020-01-01', '2021-01-01', '2022-01-01',
                                   '2023-01-01', '2024-01-01','2018-01-01',
                                   '2019-01-01', '2020-01-01', '2021-01-01',
                                   '2022-01-01', '2023-01-01', '2024-01-01'),
                             'time_increment' = c('year','year','year',
                                   'year', 'year','year', 'year','year',
                                   'year','year','year','year','year',
                                   'year'))

# compute euclidean distance for each site & variable combination
ms_anom_euclidean(fot_input_tbl = sample_ms_la_input %>%
                        dplyr::mutate(time_start = as.Date(time_start)),
                  grp_vars = c('site', 'variable'),
                  var_col = 'count')
#> Joining with `by = join_by(time_start, site, variable)`
#> Joining with `by = join_by(site, variable, time_start)`
#> Joining with `by = join_by(variable, time_start)`
#> # A tibble: 14 × 9
#>    site   time_start variable count mean_allsiteprop median date_numeric
#>    <chr>  <date>     <chr>    <dbl>            <dbl>  <dbl>        <dbl>
#>  1 Site A 2018-01-01 scd         15             53.5   53.5        17532
#>  2 Site A 2019-01-01 scd         24             88.5   88.5        17897
#>  3 Site A 2020-01-01 scd        100            111    111          18262
#>  4 Site A 2021-01-01 scd         93             49     49          18628
#>  5 Site A 2022-01-01 scd         47             73     73          18993
#>  6 Site A 2023-01-01 scd         65             37.5   37.5        19358
#>  7 Site A 2024-01-01 scd         33             31.5   31.5        19723
#>  8 Site B 2018-01-01 scd         92             53.5   53.5        17532
#>  9 Site B 2019-01-01 scd        153             88.5   88.5        17897
#> 10 Site B 2020-01-01 scd        122            111    111          18262
#> 11 Site B 2021-01-01 scd          5             49     49          18628
#> 12 Site B 2022-01-01 scd         99             73     73          18993
#> 13 Site B 2023-01-01 scd         10             37.5   37.5        19358
#> 14 Site B 2024-01-01 scd         30             31.5   31.5        19723
#> # ℹ 2 more variables: site_loess <dbl>, dist_eucl_mean <dbl>
```

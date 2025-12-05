# Compute Euclidean Distance

Compute Euclidean Distance

## Usage

``` r
compute_euclidean(ms_tbl, output_var, grp_vars = c("site", "concept_id"))
```

## Arguments

- ms_tbl:

  output from
  [`compute_dist_mean_median()`](https://ssdqa.github.io/squba.gen/reference/compute_dist_mean_median.md)
  where the cross-joined table from
  [`compute_at_cross_join()`](https://ssdqa.github.io/squba.gen/reference/compute_at_cross_join.md)
  is used as input

- output_var:

  the output variable that should be used to compute the Euclidean
  distance i.e. a count or proportion

- grp_vars:

  vector of grouping variables; the euclidean distance will be computed
  per group

## Value

one dataframe with all variables from ms_tbl with the addition of
columns with a site Loess value and a site Euclidean distance value

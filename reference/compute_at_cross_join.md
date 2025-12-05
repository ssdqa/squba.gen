# Create a cross-joined master table for variable reference

Create a cross-joined master table for variable reference

## Usage

``` r
compute_at_cross_join(
  cj_tbl,
  cj_var_names = c("site", "concept_id"),
  join_type = "left"
)
```

## Arguments

- cj_tbl:

  a table with the results of a longitudinal analysis

- cj_var_names:

  a vector with the names of variables that should be used as the
  "anchor" of the cross join where all combinations of the variables
  should be present in the final table

- join_type:

  the type of join that should be performed at the end of the function
  left is used for multi-site anomaly (euclidean distance) while full is
  used for single site anomaly (timetk package)

## Value

one data frame with all combinations of the variables from cj_var_names
with their associated facts from the original cj_tbl input

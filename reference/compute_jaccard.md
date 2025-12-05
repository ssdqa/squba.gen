# Jaccard Index Anomaly Detection

This function will compute the Jaccard Similarity Index for each
combination of two variables that occur within a specific patient's
record. This function is compatible with both the OMOP and PCORnet CDMs
based on the user's selection.

## Usage

``` r
compute_jaccard(jaccard_input_tbl, var_col, omop_or_pcornet)
```

## Arguments

- jaccard_input_tbl:

  *tabular input* \|\| **required**

  A table that contains at least `person_id`/`patid` and a variable
  column, and where each row represents a unique instance where the
  variable occurred for a given patient

  Alternatively, it can be a list of all unique `person_id`/`patid` and
  variable combinations

- var_col:

  *string* \|\| **required**

  The name of the column within `jaccard_input_table` that contains all
  the variables that should be compared to each other in the similarity
  index

- omop_or_pcornet:

  *string* \|\| **required**

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

## Value

A table with pairs of variables, labeled `concept1` and `concept2`,
counts & proportions of patients with each concept individually
(`concept1_ct`, `concept1_prop`, `concept2_ct`, `concept2_prop`), count
of patients with BOTH concepts (`cocount`), count of patients with
EITHER concept (`concept_count_union`), and the `jaccard_index`
statistic

## Examples

``` r
# create sample input with person identifier and variable associations
sample_input <- dplyr::tibble('person_id' = c(1,1,2,2,3,4,5,5),
                              'variable' = c('dx', 'drug', 'drug', 'dx',
                                             'drug', 'dx', 'dx', 'drug'))
# compute jaccard index
compute_jaccard(jaccard_input_tbl = sample_input,
                var_col = 'variable',
                omop_or_pcornet = 'omop')
#> # A tibble: 1 × 9
#>   concept1 concept2 cocount concept1_ct concept2_ct concept_count_union
#>   <chr>    <chr>      <int>       <int>       <int>               <int>
#> 1 dx       drug           3           4           4                   5
#> # ℹ 3 more variables: jaccard_index <dbl>, concept1_prop <dbl>,
#> #   concept2_prop <dbl>

```

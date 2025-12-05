# Join to a reference vocabulary table

This is a convenience function that allows users to join to a reference
vocabulary table (most likely the OMOP `concept` table). It is intended
to pull in the full name of a concept and facilitate the review of
results.

## Usage

``` r
join_to_vocabulary(tbl, vocab_tbl, col, vocab_col = "concept_id")
```

## Arguments

- tbl:

  *tabular input* \|\| **required**

  A table to which the vocabulary table should be joined

- vocab_tbl:

  *tabular input* \|\| **required**

  The reference vocabulary table (ex: the OMOP `concept` table). This
  table should minimally contain the provided `vocab_col`,
  `concept_name`, and `vocabulary_id`

- col:

  *string* \|\| **required**

  The name of the column in `tbl` that should be used in the `by`
  statement to join to the `vocab_col` in the vocabulary table

- vocab_col:

  *string* \|\| defaults to `concept_id`

  The name of the column in `vocab_tbl` that should be used in the `by`
  statement to join to the `col` in the provided data table

## Value

This function will return the dataframe provided in `tbl` with the
addition of the `concept_name` associated with each concept

## Examples

``` r
if (FALSE) { # \dontrun{

sample_input_tbl <- dplyr::tibble('concept' = c(1234, 5678, 91011),
                                  'ct_concept' = c(100, 200, 300),
                                  'site' = c('Site A', 'Site A', 'Site A'))

join_to_vocabulary(tbl = sample_input_tbl,
                   vocab_tbl = vocabulary_tbl('concept'),
                   col = 'concept',
                   vocab_col = 'concept_id')
} # }

```

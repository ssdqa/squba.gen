# Replace Summary Site Column

For analyses where a summary site column was created by
[`check_site_type()`](https://ssdqa.github.io/squba.gen/reference/check_site_type.md),
this function will replace the name of that column with the original
"site" name.

## Usage

``` r
replace_site_col(tbl)
```

## Arguments

- tbl:

  *tabular input* \|\| **required**

  The table with a `site_summ` column that needs to be replaced

## Value

The same table that was used as input, with `site` replacing `site_summ`

## Examples

``` r
## Sample input table
input_sample <- dplyr::tibble(person_id = c(1,2,3),
                              site_summ = c("combined","combined","combined"))

## Replace site_summ col for final output
replace_site_col(tbl = input_sample)
```

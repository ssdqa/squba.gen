# Check Site Type

This function will evaluate the number of distinct values in the `site`
column in the provided cohort table and determine how that compares to
the provided `multi_or_single_site` designation.

## Usage

``` r
check_site_type(cohort, multi_or_single_site)
```

## Arguments

- cohort:

  *tabular input* \|\| **required**

  The cohort to be used for data quality testing. This table should
  contain, at minimum:

  - `site` \| *character* \| the name(s) of institutions included in
    your cohort

  - `person_id` / `patid` \| *integer* / *character* \| the patient
    identifier

  - `start_date` \| *date* \| the start of the cohort period

  - `end_date` \| *date* \| the end of the cohort period

- multi_or_single_site:

  *string* \|\| defaults to `single`

  A string, either `single` or `multi`, indicating whether a single-site
  or multi-site analysis should be executed

## Value

If `multi_or_single_site = single` but multiple sites are provided, the
cohort table is returned with a summary site column set to `combined` so
all sites will be treated as one group. Otherwise, the existing site
column is returned as-is. If an illogical parameter combination is
supplied, the function will return an error with recommendations on how
to remedy the issue.

## Examples

``` r
## Create sample cohort
cohort_sample <- dplyr::tibble(site = c('Site A', 'Site B', 'Site C'),
                               person_id = c(1,2,3))

## If number of sites & indicated multi/single site match, output same table
check_site_type(cohort = cohort_sample,
                multi_or_single_site = 'multi')
#> $cohort
#> # A tibble: 3 × 2
#>   site   person_id
#>   <chr>      <dbl>
#> 1 Site A         1
#> 2 Site B         2
#> 3 Site C         3
#> 
#> $grouped_list
#> [1] "site"
#> 
#> $site_list_adj
#> [1] "Site A" "Site B" "Site C"
#> 

## If multiple sites but single site indicated, create site_summ column
check_site_type(cohort = cohort_sample,
                multi_or_single_site = 'single')
#> $cohort
#> # A tibble: 3 × 3
#>   site   person_id site_summ
#>   <chr>      <dbl> <chr>    
#> 1 Site A         1 combined 
#> 2 Site B         2 combined 
#> 3 Site C         3 combined 
#> 
#> $grouped_list
#> [1] "site_summ"
#> 
#> $site_list_adj
#> [1] "combined"
#> 
```

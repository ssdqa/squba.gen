# Compute Facts Over Time

This function will loop through a time series and execute a
user-provided function against only the data within the time period of
interest. It will also ensure that only patients whose start date has
passed but whose end date has not passed are included in the analysis
for any given time period. This means that, for example, a patient who
has a cohort period 1/1/2020 to 1/1/2025 would be included when looking
at the year of 2024, but they would not be included when looking at the
year of 2019 as they had not yet entered the cohort.

## Usage

``` r
compute_fot(
  cohort,
  check_func,
  site_col,
  reduce_id = NULL,
  time_period = "year",
  time_span = c("2012-01-01", "2020-12-31"),
  site_list
)
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

  Note that the start and end dates included in this table will be used
  to limit the search window for the analyses in this module.

- check_func:

  *function* \|\| **required**

  The function that should be executed within each time period in the
  loop. This parameter should be structured as the following, where
  `dat` is the input data for the function:

  `function(dat){check_function(param1 = dat, param2 = param2_input, ..., paramX = paramX_input)}`

  Make sure to include all parameters required for the original function
  where default values are not being used.

- site_col:

  *string* \|\| **required**

  The name of the column in the cohort table that contains the site
  names. This will typically be either `site` or `site_summ`

- reduce_id:

  *string* \|\| defaults to `NULL`

  If the function provided in `check_func` returns a list of tables,
  this parameter should be the name of the column that should be used to
  reduce the tables into one dataframe (via
  [dplyr::bind_rows](https://dplyr.tidyverse.org/reference/bind_rows.html))

- time_period:

  *string* \|\| defaults to `year`

  A string indicating the distance between dates within the specified
  time_span. Defaults to `year`, but other time periods such as `month`
  or `week` are also acceptable

- time_span:

  *vector - length 2* \|\| defaults to `c('2012-01-01', '2020-01-01')`

  A vector indicating the lower and upper bounds of the time series for
  longitudinal analyses

- site_list:

  *list* \|\| **required**

  A list of sites for which you would like to examine clinical facts.
  Can be one site (single-site) or multiple (multi-site). Ensure that
  all sites listed here exist in the provided cohort table.

## Value

This function will return a dataframe where the output of the provided
`check_func` has been executed once for each `time_period` in the
provided `time_span` for each of the sites included in `site_list`

## Examples

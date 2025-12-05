# Patient Facts per Visit Type – PCORnet

Patient Facts per Visit Type – PCORnet

## Usage

``` r
loop_through_visits_pcnt(
  cohort_tbl,
  check_func,
  site_col,
  time = FALSE,
  visit_type_tbl,
  visit_tbl = cdm_tbl("encounter"),
  site_list,
  visit_list = c("inpatient", "outpatient"),
  domain_tbl
)
```

## Arguments

- cohort_tbl:

  a table with members of the cohort that has been run through
  [`prepare_cohort()`](https://ssdqa.github.io/squba.gen/reference/prepare_cohort.md)

- check_func:

  the base function for the check that needs to be executed across time;
  this argument should be structured as the following, where cht is the
  cohort and t is the input data for the function:

                    function(cht, t){check_function(param1 = cht, param2 = t, param3 = param3_input, ...,
                    paramX = paramX_input)}

                    all parameters for the base check function should be included if any defaults are not being
                    used

- site_col:

  the column in the data where the site variable can be found

- time:

  a logical indicating whether the analysis is being conducted
  longitudinally

- visit_type_tbl:

  a table that defines available visit types that are called in
  `visit_list.`

  - `enc_type`: enc_type that represents the visit type of interest
    (i.e. 9201 or IP)

  - `visit_type`: the string label to describe the visit type; this
    label can be used multiple times within the file if multiple
    visit_concept_ids/enc_types represent the visit type

- visit_tbl:

  the cdm encounter tbl

- site_list:

  the sites to iterate through

- visit_list:

  the list of visit types to iterate through

- domain_tbl:

  a table that defines the domains where facts should be identified with
  the following columns:

  - `domain`: a string label for the domain being examined (i.e.
    prescription drugs)

  - `domain_tbl`: the CDM table where information for this domain can be
    found (i.e. drug_exposure)

  - `filter_logic`: an optional string to be parsed as logic to filter
    the domain_tbl as needed to best represent the domain

## Value

a list of dataframes with median number of facts per patient for all
domains in domain_tbl, where each dataframe is specific to a given visit
type from visit_list

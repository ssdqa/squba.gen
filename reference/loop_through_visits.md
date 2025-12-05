# Patient Facts per Visit Type

This function will loop through each site and visit type provided and
execute the provided check function to compute facts stratified by visit
type. Primarily intended for use in the Patient Facts module, but can be
adapted for use elsewhere.

## Usage

``` r
loop_through_visits(
  cohort_tbl,
  omop_or_pcornet,
  check_func,
  site_col,
  time = FALSE,
  visit_type_tbl,
  visit_tbl,
  site_list,
  visit_list = c("outpatient", "inpatient"),
  domain_tbl
)
```

## Arguments

- cohort_tbl:

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
  to limit the search window for the analyses in this module. It is
  recommended that this table has been passed through the
  [prepare_cohort](https://ssdqa.github.io/squba.gen/reference/prepare_cohort.md)
  function as well.

- omop_or_pcornet:

  *string* \|\| **required**

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

  - `omop`: run the
    [`loop_through_visits_omop()`](https://ssdqa.github.io/squba.gen/reference/loop_through_visits_omop.md)
    function against an OMOP CDM instance

  - `pcornet`: run the
    [`loop_through_visits_pcnt()`](https://ssdqa.github.io/squba.gen/reference/loop_through_visits_pcnt.md)
    function against a PCORnet CDM instance

- check_func:

  *function* \|\| **required**

  The function that should be executed within each time period in the
  loop. This parameter should be structured as the following, where
  `cht` is the cohort table and `t` is the input data for the function:

  `function(cht, t){check_function(param1 = cht, param2 = t, param3 = param3_input, ..., paramX = paramX_input)}`

  Make sure to include all parameters required for the original function
  where default values are not being used.

- site_col:

  *string* \|\| **required**

  The name of the column in the cohort table that contains the site
  names. This will typically be either `site` or `site_summ`

- time:

  *boolean* \|\| defaults to `FALSE`

  A boolean to indicate whether to execute a longitudinal analysis

- visit_type_tbl:

  *tabular input* \|\| **required**

  A table that defines visit types of interest called in `visit_list.`
  This input should contain:

  - `visit_concept_id` / `visit_detail_concept_id` or `enc_type` \|
    *integer* or *character* \| the `visit_(detail)_concept_id` or
    `enc_type` that represents the visit type of interest (i.e. 9201 or
    IP)

  - `visit_type` \| *character* \| the string label to describe the
    visit type

- visit_tbl:

  *tabular input* \|\| defaults to `cdm_tbl('visit_occurrence')`

  The CDM table with visit information (i.e. visit_occurrence or
  encounter)

- site_list:

  *list* \|\| **required**

  A list of sites for which you would like to examine clinical facts.
  Can be one site (single-site) or multiple (multi-site). Ensure that
  all sites listed here exist in the provided cohort table.

- visit_list:

  *string or vector* \|\| defaults to `c('outpatient', 'inpatient')`

  A string or vector of visit types by which the output should be
  stratified. Each visit type listed in this parameter should match an
  associated visit type defined in the `visit_type_tbl`

- domain_tbl:

  *tabular input* \|\| **required**

  A table that defines the fact domains to be investigated in the
  analysis. This input should contain:

  - `domain` \| *character* \| a string label for the domain being
    examined (i.e. prescription drugs)

  - `domain_tbl` \| *character* \| the CDM table where information for
    this domain can be found (i.e. drug_exposure)

  - `filter_logic` \| *character* \| logic to be applied to the
    domain_tbl in order to achieve the definition of interest; should be
    written as if you were applying it in a dplyr::filter command in R

## Value

This function will return a list of dataframes with the median number of
facts per patient for all domains in `domain_tbl`, where each dataframe
is specific to a given visit type from `visit_list`

## Examples

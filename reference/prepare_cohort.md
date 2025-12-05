# Prepare Cohort

For the cohort table input into any of the \*\_process functions, this
function will prepare that cohort for the rest of the analysis. This
includes computing follow-up based on start and end dates and computing
age at cohort entry categorized into user-provided groups.

## Usage

``` r
prepare_cohort(cohort_tbl, age_groups = NULL, omop_or_pcornet)
```

## Arguments

- cohort_tbl:

  *tabular input* \|\| **required**

  The cohort to be used for data quality testing. This table should
  contain, at minimum:

  - `site` \| *character* \| the name(s) of institutions included in
    your cohort

  - `person_id` / `patid` \| *integer / character* \| the patient
    identifier

  - `start_date` \| *date* \| the start of the cohort period

  - `end_date` \| *date* \| the end of the cohort period

- age_groups:

  *tabular input* \|\| defaults to `NULL`

  If you would like to stratify the results by age group, create a table
  or CSV file with the following columns and use it as input to this
  parameter:

  - `min_age` \| *integer* \| the minimum age for the group (i.e. 10)

  - `max_age` \| *integer* \| the maximum age for the group (i.e. 20)

  - `group` \| *character* \| a string label for the group (i.e. 10-20,
    Young Adult, etc.)

  If you would *not* like to stratify by age group, leave as `NULL`

- omop_or_pcornet:

  *string* \|\| **required**

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

## Value

This function will return the same input cohort table with the addition
of the column `fu`, which includes the length of patient follow-up based
on the provided start & end dates. Optionally, if age_groups is not
NULL, the output will also include `age_ce` (patient age at cohort
entry) and `age_grp` (the user-provided age group category)

## Examples

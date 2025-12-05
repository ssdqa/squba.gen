# Age at Cohort Entry – OMOP

Age at Cohort Entry – OMOP

## Usage

``` r
compute_age_groups_omop(cohort_tbl, person_tbl, age_groups)
```

## Arguments

- cohort_tbl:

  table of cohort members with at least `person_id`, `start_date`, and
  `end_date`

- person_tbl:

  the CDM person table

- age_groups:

  a table where the user defines the minimum and maximum age allowed for
  a group and provides a string name for the group

## Value

`cohort_tbl` with the age at cohort entry and age group for each patient

# Build birth_date Column

This function, which is specific to the OMOP CDM implementation, will
create an aggregate birth_date column using `year_of_birth`,
`month_of_birth`, and `day_of_birth`

## Usage

``` r
build_birth_date(cohort, person_tbl)
```

## Arguments

- cohort:

  *tabular input* \|\| **required**

  A table with cohort members, which contains at least a `person_id`
  column

- person_tbl:

  *tabular input* \|\| **required**

  The CDM `person` table or another table that contains at least
  `person_id`, `year_of_birth`, `month_of_birth`, and `day_of_birth`

## Value

This function will return a table with a new birth_date column that is a
combination of year, month, and day of birth. This is intended to
facilitate computations using date of birth, like computing age.

## Examples

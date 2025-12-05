# Calculate Date Differences in Multiple SQL Backends

Function to get sql code for number of days between date1 and date2.
Adapted for sql dialects for Postgres, MSSQL, Snowflake, Oracle,
BigQuery, Presto, & Spark.

## Usage

``` r
calc_days_between_dates(
  date_col_1,
  date_col_2,
  db = get_argos_default()$config("db_src")
)
```

## Arguments

- date_col_1:

  Date col 1

- date_col_2:

  Date col 2

- db:

  connection type object. Defaulted to config('db_src') for standard
  framework Functionality added for Postgres, MS SQL and Snowflake

## Value

an integer representing the difference (in days) between the two
provided dates

## Details

Should always be wrapped by sql()

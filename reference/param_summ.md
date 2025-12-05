# Generate output_function identifier & build output instructions

This function will summarize the input parameters for the \*\_process
functions, which is then used to output a string to the console that
indicates the appropriate `output_function` to use in the \*\_output
step

## Usage

``` r
param_summ(check_string, ...)
```

## Arguments

- check_string:

  *string* \|\| **required**

  A string abbreviation to represent the module (ex: evp, pf). This will
  be what is prefixed to the names of the output functions

- ...:

  All of the parameters input into the primary \*\_process function. Any
  argument that is not able to be vectorized (i.e. a CDM tbl, codeset,
  etc) will not appear in the final summary

## Value

A string with the name of the output_function that will be added to the
final result table, and avector with information about which parameters
will be required to use the \*\_output function for each module. This
vector is intended to be fed into a cli message command to generate a
console message.

## Examples

``` r
# intended for use inside the *_process functions

param_summ(check_string = 'evp',
           multi_or_single_site = 'single',
           anomaly_or_exploratory = 'exploratory',
           time = FALSE)
#> Rows: 77 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (5): module, check, Always Required, Required for Check, Optional
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> $vector
#> [1] "\033[1m\033[34mAlways Required\033[39m\033[22m: process_output" 
#> [2] "\033[1m\033[34mRequired for Check\033[39m\033[22m: output_level"
#> 
#> $string
#> [1] "evp_ss_exp_cs"
#> 
```

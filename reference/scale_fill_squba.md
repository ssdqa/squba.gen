# squba Fill Scale Constructor

This function will operate with ggplot objects to use the `squba`
standard colors/color palettes to add fill to graphs

## Usage

``` r
scale_fill_squba(palette = "main", discrete = TRUE, reverse = FALSE, ...)
```

## Arguments

- palette:

  *string* \|\| defaults to `main`

  The name of the palette as it appears in `squba_palettes_standard`

- discrete:

  *boolean* \|\| defaults to `TRUE`

  A boolean indicating whether the fill aesthetic is discrete or not

- reverse:

  *boolean* \|\| defaults to `FALSE`

  A boolean indicating whether the palette should be reversed

- ...:

  Any additional arguments passed to
  [ggplot2::discrete_scale](https://ggplot2.tidyverse.org/reference/discrete_scale.html)
  or ggplot2::scale_fill_gradientn, used respectively when discrete is
  TRUE or FALSE

## Examples

``` r
library(ggplot2)

mtcars %>%
  tibble::rownames_to_column('car_model') %>%
  ggplot(aes(x = car_model, y = wt, fill = car_model)) +
  geom_col() +
  scale_fill_squba()

```

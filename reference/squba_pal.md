# Return function to interpolate a color palette

Return function to interpolate a color palette

## Usage

``` r
squba_pal(palette, reverse = FALSE, ...)
```

## Arguments

- palette:

  Character name of palette in squba_palettes_standard

- reverse:

  Boolean indicating whether the palette should be reversed

- ...:

  Additional arguments to pass to colorRampPalette()

## Value

color palettes, interpolated if necessary, with specified scheme and
number of colors example usage: squba_pal("beachy")(10)

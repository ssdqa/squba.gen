#' @import ggplot2
#' @import scales
#' @import ggpubr
#' @importFrom grDevices colorRampPalette
NULL

# tutorial: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
#' Function to extract colors as hex codes
#'
#' @param ... Character names of squba_colors_standard
#' @return name and hex code/s for specified color
#' example useage: extract_color() (returns all colors) or extract_color("rust") (just returns "rust")
#'
extract_color <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (squba_colors_standard)

  squba_colors_standard[cols]
}

#' Return function to interpolate a color palette
#'
#' @param palette Character name of palette in squba_palettes_standard
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @return color palettes, interpolated if necessary, with specified scheme and number of colors
#' example usage: squba_pal("beachy")(10)
squba_pal <- function(palette, reverse = FALSE, ...) {
  pal <- squba_palettes_standard[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' SQUBA Standard Color Hexes
#'
#' @export
squba_colors_standard<-c(`brightpink`="#FF4D6FFF",
                         `lightblue`="#579EA4FF",
                         `burntorange`="#DF7713FF",
                         `yellow`="#F9C000FF",
                         `lightgreen`="#86AD34FF",
                         `dustblue`="#5D7298FF",
                         `seagreen`="#81B28DFF",
                         `rust`="#7E1A2FFF",
                         `violet`="#2D2651FF",
                         `redorange`="#C8350DFF",
                         `rosypink`="#BD777AFF",
                         `grey=`="#E2D8D6FF",
                         `lightorange`="#FCB861")

#' SQUBA Standard Color Palettes
#'
#' @export
squba_palettes_standard<-list(
  `dark` = extract_color("rust", "violet", "redorange"),
  `fun` = extract_color("brightpink", "lightblue", "yellow"),
  `beachy`=extract_color("lightblue","dustblue","seagreen"),
  `diverging`=extract_color("dustblue", "lightorange", "rust"),
  `sequential`=extract_color("grey", "rosypink", "rust"),
  `main`=extract_color("brightpink", "lightblue", "burntorange", "yellow",
                      "lightgreen","dustblue", "seagreen", "rust",
                      "violet", "redorange", "rosypink")
)

# usage: squba_pal("dark")(10)


#' SQUBA Fill Scale Constructor
#'
#' This function will operate with ggplot objects to use the SQUBA
#' standard colors/color palettes to add fill to graphs
#'
#' @param palette Character name of palette in squba_palettes_standard
#'                  If no palette specified, defaults to "main" palette
#' @param discrete Boolean indicating whether fill aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @examples
#' library(ggplot2)
#'
#' mtcars %>%
#'   tibble::rownames_to_column('car_model') %>%
#'   ggplot(aes(x = car_model, y = wt, fill = car_model)) +
#'   geom_col() +
#'   scale_fill_squba()
#'
#' @export
#'
scale_fill_squba <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- squba_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("squba_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' SQUBA Color Scale Constructor
#'
#' This function will operate with ggplot objects to use the SQUBA
#' standard colors/color palettes to add color to graphs
#'
#' @param palette Character name of palette in squba_palettes_standard.
#'                  If no palette specified, defaults to "main" palette
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @examples
#' library(ggplot2)
#'
#' mtcars %>%
#'   tibble::rownames_to_column('car_model') %>%
#'   ggplot(aes(x = mpg, y = wt, color = car_model)) +
#'   geom_point() +
#'   scale_color_squba()
#'
#' @export
#'
scale_color_squba <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- squba_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("squba_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


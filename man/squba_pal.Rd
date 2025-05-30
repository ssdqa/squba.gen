% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colors.R
\name{squba_pal}
\alias{squba_pal}
\title{Return function to interpolate a color palette}
\usage{
squba_pal(palette, reverse = FALSE, ...)
}
\arguments{
\item{palette}{Character name of palette in squba_palettes_standard}

\item{reverse}{Boolean indicating whether the palette should be reversed}

\item{...}{Additional arguments to pass to colorRampPalette()}
}
\value{
color palettes, interpolated if necessary, with specified scheme and number of colors
example usage: squba_pal("beachy")(10)
}
\description{
Return function to interpolate a color palette
}

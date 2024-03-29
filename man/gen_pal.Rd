% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_pal.R
\name{gen_pal}
\alias{gen_pal}
\title{Generate binary or 4 color palettes}
\usage{
gen_pal(mode, pal, type = "leaflet")
}
\arguments{
\item{mode}{Mode to generate the palette. "bin" is suitable for binary maps (thresholded maps). "quad" will return a 4 color palette, suitable for maps showing differences from a (binary) reference to a distinct scenario (either future or past).}

\item{pal}{Which palete to use. Character string indicating the name of the palette. Can also be a character vector of colors, in which case two or four should be supplied. The following pre-seted (and tested) palettes are available and are recommended:
These are the available palettes for binary maps:
\itemize{
 \item{"BlGr":}{ Blue and dark gray}
 \item{"GeGr":}{ Green and dark gray}
 \item{"OrGr":}{ Orange and dark gray}
 \item{"YeBl":}{ Yellow and dark blue}
}

These are the available color schemes for 4 colors maps:

\itemize{
 \item{"Cont":}{ Dark gray, Yellow, Purple, Light green/blue}
 \item{"Bang":}{ Black, Gold, Light green, Blue} \href{https://www.nature.com/articles/nmeth.1618}{Based on Bang Wong's palette.}
 \item{"Happy":}{ Dark gray, Violet, Orange, Red tone}
}

All of these palettes are colorblind safe.

For the continuous colors, you can use any of the palettes available for the colorNumeric function (see \link[leaflet]{colorNumeric}) (THIS FUNCTION WILL NOT WORK WITH CONTINUOUS COLORS. SUPPLY THE NAME DIRECTLY IN THE SDMVIS FUNCTIONS.).}

\item{type}{Which type of palette to return. If "leaflet" (default), a leaflet ready palette is returned. Any other value will return a character vector of colors.}
}
\value{
A palette to produce interactive SDM/ENM maps.
}
\description{
This is an accessory function to generate binary or 4 color palettes to be used in the leaflet maps.
}
\examples{
library(leaflet)
library(rgdal)

data(thresholded_sdm)

binpal <- gen_pal(mode = "bin", pal = "BlGr")

leaflet()\%>\%
        addProviderTiles("Esri.OceanBasemap") \%>\%
        addRasterImage(thresholded_sdm[[1]], colors = binpal, opacity = 0.9)
        
}

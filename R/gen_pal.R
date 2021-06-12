#' Generate binary or 4 color palettes
#'
#' This is an accessory function to generate binary or 4 color palettes to be used in the leaflet maps.
#' @param mode Mode to generate the palette. "bin" is suitable for binary maps (thresholded maps). "quad" will return a 4 color palette, suitable for maps showing differences from a (binary) reference to a distinct scenario (either future or past).
#' @param pal Which palete to use. Character string indicating the name of the palette. Can also be a character vector of colors, in which case two or four should be supplied. The following pre-seted (and tested) palettes are available and are recommended:
#' These are the available palettes for binary maps:
#' \itemize{
#'  \item{"BlGr":}{ Blue and dark gray}
#'  \item{"GeGr":}{ Green and dark gray}
#'  \item{"OrGr":}{ Orange and dark gray}
#'  \item{"YeBl":}{ Yellow and dark blue}
#' }
#' 
#' These are the available color schemes for 4 colors maps:
#' 
#' \itemize{
#'  \item{"Cont":}{ Dark gray, Yellow, Purple, Light green/blue}
#'  \item{"Bang":}{ Black, Gold, Light green, Blue} \href{https://www.nature.com/articles/nmeth.1618}{Based on Bang Wong's palette.}
#'  \item{"Happy":}{ Dark gray, Violet, Orange, Red tone}
#' }
#' 
#' All of these palettes are colorblind safe.
#' 
#' For the continuous colors, you can use any of the palettes available for the colorNumeric function (see \link[leaflet]{colorNumeric}) (THIS FUNCTION WILL NOT WORK WITH CONTINUOUS COLORS. SUPPLY THE NAME DIRECTLY IN THE SDMVIS FUNCTIONS.).
#' 
#' @param type Which type of palette to return. If "leaflet" (default), a leaflet ready palette is returned. Any other value will return a character vector of colors.
#' 
#' @return A palette to produce interactive SDM/ENM maps.
#' 
#' @examples 
#' library(leaflet)
#' library(rgdal)
#' 
#' data(thresholded_sdm)
#' 
#' binpal <- gen_pal(mode = "bin", pal = "BlGr")
#' 
#' leaflet()%>%
#'         addProviderTiles("Esri.OceanBasemap") %>%
#'         addRasterImage(thresholded_sdm[[1]], colors = binpal, opacity = 0.9)
#'         
#' @import leaflet
#' @import rgdal
#' @export
gen_pal <- function(mode, pal, type = "leaflet") {
        
        bin.cols <- list(
                BlGr = c("#2c3e50", "#00DBC7"),
                GeGr = c("#2EE22E", "#00DBC7"),
                OrGr = c("#FF9000", "#00DBC7"),
                YeBl = c("#EFD807", "#19518A")
        )
        
        quad.cols <- list(
                Cont = c("#2c3e50","#f1e40e","#a13eda","#00c5b3"),
                Bang = c("#000000","#E69F00","#009E73","#0072B2"),
                Happy = c("#1E262D","#FE6100","#DC267F","#785EF0")
                
        )
        
        
        if (mode == "bin") {
                if (length(pal) < 2) {
                        if (pal %in% names(bin.cols)) {
                                sel.col <- bin.cols[[pal]]
                        } else{
                                stop("Wrong palette name.")
                        }
                } else {
                        sel.col <- pal
                }
                
                if (type == "leaflet") {
                        exp.pal <- leaflet::colorFactor(sel.col,
                                                        levels = c(0,1),
                                                        na.color = NA)
                } else{
                        exp.pal <- sel.col
                }

        } else {
                
                if (mode == "quad") {
                        if (length(pal) < 2) {
                                if (pal %in% names(quad.cols)) {
                                        sel.col <- quad.cols[[pal]]
                                } else{
                                        stop("Wrong palette name.")
                                }
                        } else {
                                sel.col <- pal
                        }
                        
                        if (type == "leaflet") {
                                exp.pal <- leaflet::colorFactor(sel.col,
                                                                levels = c(0,1,
                                                                           2,3),
                                                                na.color = NA)
                        } else{
                                exp.pal <- sel.col
                        }
                        
                } else{
                        stop("Mode should be one of bin or quad")
                }
        }
        
        exp.pal
        
}
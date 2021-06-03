#' Generate binary or 4 color palettes
#'
#' This is an accessory function to generate binary or 4 color palettes to be used in the leaflet maps.
#' @param mode Mode to generate the palette. "bin" is suitable for binary maps (thresholded maps). "quad" will return a 4 color palette, suitable for maps showing differences from a (binary) reference to a distinct scenario (either future or past).
#' @param pal Which palete to use. Character string indicating the name of the palette. Can also be a character vector of colors, in which case two or four should be supplied. The following pre-seted (and tested) palettes are available and are recommended:
#' \itemize{
#'  \item{"YeBl":}{ Yellow and Blue}
#'  \item{"GrBl":}{ Green and Blue}
#' }
#' All of these palettes are colorblind safe.
#' @param type Which type of palette to return. If "leaflet" (default), a leaflet ready palette is returned. Any other value will return a character vector of colors.
#' 
#' @return A palette to produce interactive SDM/ENM maps.
#' @examples 
#' library(raster)
#' library(leaflet)
#' 
#' sdm <- raster(nrows=10, ncols=10)
#' sdm[] <- sample(c(0, 1, NA), size = ncell(r), replace = T)
#' 
#' binpal <- gen_pal(mode = "binary", pal = "YeBl")
#' 
#' leaflet()%>%
#' addRasterImage(sdm, colors = binpal, opacity = 0.9)
#' @import leaflet
#' @export
gen_pal <- function(mode, pal, type = "leaflet") {
        
        bin.cols <- list(
                YeBl = c("#2c3e50", "#00DBC7"),
                YePu = c("#2c3e50", "#00DBC7")
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

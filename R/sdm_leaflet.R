#' SDM Leaflet map
#'
#' Create a Leaflet map with the results of an SDM analysis.
#' This map can be explored interactively in the viewer, so its possible to have a better comprehension of the outcomes of your SDM.
#' @param sdm The SDM/ENM result (or any other spatial analysis result). Should be in the Raster* format. 
#' If you want to plot more than 1 layer at the same time (for example, current and future predictions), you should put each RasterLayer in a list (for now, it's not possible to plot everything from a RasterStack or RasterBrick).
#' Binary maps should have just two values (0 for absence and 1 for presence).
#' Difference maps ("quad" mode, see below) should have 4 values: 0 for unsuitable areas, 1 for areas that were lost, 2 for areas where there was gain of area and 3 for areas where suitability was mantained. Its easy to produce such a map from binary maps. Just multiply the future condition binary map by 2 and sum the current map.
#' @param mode A character string indicating the mode of ploting. Should be one of "bin" (used for thresholded binary SDMs), "quad" (used for ploting difference maps between a binary reference map and a future one) or "continuous" (used for plotting continuous scale data).
#' @param pts A data frame containing the presence or presence/absence points (optional). The first column should be longitude (x) and the sencond latitude (y). In the case of presence/absence data, an additional collumn should be provided, coded as 0 (absence) and 1 (presence).
#' @param pal Character string indicating the name of the palette (see \link[sdmvis]{gen_pal}). If not supplied, the default will be used.
#' @param layernames An optional character vector indicating the names of the layers. This will be used in the legend. If not supplied, names will be extracted from the RasterLayer.
#' @param crs Enables to change the default projection used in the Leaflet package. For now, not functional.
#' @param cluster Should the points be clustered (i.e., aggregated)? Only valid if `pts` is supplied. Default is TRUE.
#' 
#' @return A Leaflet map.
#' 
#' @example 
#' library(sdmvis)
#' 
#' data("thresholded_sdm")
#' data("pa_data")
#' 
#' sdm.list <- list(thresholded_sdm[[1]], thresholded_sdm[[2]])
#' sdm.names <- c("current", "future")
#' 
#' sdm_leaflet(sdm = sdm.list,
#'             mode = "bin",
#'             pts = pa_data,
#'            layernames = sdm.names)
#' 
#' @import raster
#' @import leaflet
#' @import leaflet.extras
#' 
#' @export
sdm_leaflet <- function(sdm, mode = "bin", pts = NULL, pal = NULL,
                        layernames = NULL, crs = "standard", cluster = TRUE){
        
        # Verify conditions
        if (is.list(sdm)) {
                if (class(sdm[[1]]) != "RasterLayer") {
                        stop("SDM maps should be in the Raster* format.")
                }
        } else{
                if (class(sdm) != "RasterLayer") {
                        stop("SDM maps should be in the Raster* format.")
                } else{
                        sdm <- list(sdm)
                }
        }
        
        if (crs == "standard") {
                crs <- paste0("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0",
                " +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0",
                " +units=m +no_defs")
        }
        
        if (!is.null(layernames)) {
                lname <- layernames
        } else{
                
               lname <- lapply(sdm, names)
               
               lname <- unlist(lname)
               
        }
        
        # Reproject
        sdm <- lapply(sdm, projectRaster,
                      crs = crs,
                      method = "ngb")

        if (mode != "continuous") {
                # Convert to factor
                sdm <- lapply(sdm, as.factor)
        }

        # Plot
        if (mode == "bin") {
                if (is.null(pal)) {
                        binpal <- sdmvis::gen_pal("bin", "BlGr")
                }else{
                        if (length(pal) > 1) {
                                colors <- pal
                                
                                binpal <- leaflet::colorFactor(colors,
                                                               levels = c(0,1),
                                                               na.color = NA)
                        } else{
                                binpal <- sdmvis::gen_pal("bin", pal = pal)
                        }
                        
                }
        }
        
        if (mode == "quad") {
                if (is.null(pal)) {
                        binpal <- sdmvis::gen_pal("quad", "Cool")
                }else{
                        if (length(pal) > 1) {
                                colors <- pal
                                
                                binpal <- leaflet::colorFactor(colors,
                                                               levels = c(0,1),
                                                               na.color = NA)
                        } else{
                                binpal <- sdmvis::gen_pal("quad", pal = pal)
                        }
                }
        }
        
        if (mode == "continuous") {
                if (is.null(pal)) {
                        pal <- "viridis"
                }
        }

        basemap <- leaflet()
                # Base groups
        basemap <- addProviderTiles(basemap, "Esri.OceanBasemap",
                                    group = "Esri Ocean")
        basemap <- addProviderTiles(basemap, "CartoDB.Positron",
                                    group = "CartoDB")
        basemap <- addProviderTiles(basemap, "CartoDB.DarkMatter",
                                    group = "CartoDB Dark")
        
        overmap <- basemap
        
        if (!is.null(pts)) {
                if (cluster) {
                        clopt <- markerClusterOptions()
                }else{
                        clopt = NULL
                }
                
                if (length(pts) == 2) {
                        colnames(pts) <- c("longitude", "latitude")
                        
                        overmap <- addMarkers(overmap,
                                   data = pts,
                                   clusterOptions = clopt,
                                   group = "Points",
                                   weight = 2,
                                   radius = 5
                        )
                } else{
                        pts <- pts[,1:3]
                        
                        colnames(pts) <- c("longitude", "latitude", "dsp")
                        
                        overmap <- addCircleMarkers(overmap,
                                              data = pts[pts[,3] == 1, 1:2],
                                              clusterOptions = clopt,
                                              group = "Presence Points",
                                              color = "blue",
                                              weight = 2,
                                              radius = 5
                        )
                        
                        overmap <- addCircleMarkers(overmap,
                                              data = pts[pts[,3] == 0, 1:2],
                                              clusterOptions = clopt,
                                              group = "Absence Points",
                                              color = "orange",
                                              weight = 2,
                                              radius = 5
                        )
                }
        }

        if (mode == "bin") {
                for (i in 1:length(sdm)) {
                        overmap <- addRasterImage(overmap, sdm[[i]],
                                                  colors = binpal,
                                                  opacity = 0.9,
                                                  group = lname[i])
                        overmap <- addLegend(overmap,
                                             colors = binpal(c(0,1)),
                                             labels = c("Unsuitable", 
                                                        "Suitable"),
                                             title = lname[i],
                                             opacity = 1,
                                             group = lname[i])
                }
        }
        
        if (mode == "quad") {
                for (i in 1:length(sdm)) {
                        overmap <- addRasterImage(overmap, sdm[[i]],
                                                  colors = binpal,
                                                  opacity = 0.9,
                                                  group = lname[i])
                        overmap <- addLegend(overmap,
                                             colors = binpal(0:3),
                                             labels = c("Unsuitable", 
                                                        "Lost",
                                                        "Gain",
                                                        "Kept"),
                                             title = lname[i],
                                             opacity = 1,
                                             group = lname[i])
                }
        }
        
        if (mode == "continuous") {
                for (i in 1:length(sdm)) {
                        
                        binpal <- colorNumeric(palette = pal,
                                               values(sdm[[i]]),
                                               na.color = "transparent")
                        
                        overmap <- addRasterImage(overmap, sdm[[i]],
                                                  colors = binpal,
                                                  opacity = 0.9,
                                                  group = lname[i])
                        overmap <- addLegend(overmap,
                                             colors = binpal(0:3),
                                             labels = c("Unsuitable", 
                                                        "Lost",
                                                        "Gain",
                                                        "Kept"),
                                             title = lname[i],
                                             opacity = 1,
                                             group = lname[i])
                }
        }
        
        if (!is.null(pts)) {
                if (length(pts) == 2) {
                        lname <- c("Points", lname)
                } else{
                        lname <- c("Presence", "Absence", lname)
                }
        }

                # Layers control
        finalmap <- addLayersControl(
                        overmap,
                        baseGroups = c("Esri Ocean", "CartoDB", "CartoDB Dark"),
                        overlayGroups = lname,
                        options = layersControlOptions(collapsed = T),
                        position = "bottomright")

        finalmap <- addFullscreenControl(finalmap)

        finalmap

        
}

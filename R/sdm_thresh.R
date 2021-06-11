#' SDM Leaflet map with different thresholds
#'
#' Create a Leaflet map with the results of an SDM and draw how different thresholds will result.
#' This map can be explored interactively in the viewer, so its possible to have a better comprehension of the outcomes of your SDM.
#' @param sdm The SDM/ENM result. Should be in the Raster* format.
#' @param thresh A vector containing the thresholds to be applied (numeric values). If you want to try p10 and MTP, there is a function included in the package (\link[sdmvis]{sdm_get_thresh})
#' @param tname An optional character vector containing the names of the thresholds to be used in the legend. If not supplied, numerals will be used instead.
#' @param pts A data frame containing the presence or presence/absence points (optional). The first column should be longitude (x) and the sencond latitude (y). In the case of presence/absence data, an additional collumn should be provided, coded as 0 (absence) and 1 (presence).
#' @param pal Character string indicating the name of the continuous mode palette (see \link[sdmvis]{gen_pal}). If not supplied, the default will be used.
#' @param crs Enables to change the default projection used in the Leaflet package. For now, not functional.
#' @param cluster Should the points be clustered (i.e., aggregated)? Only valid if `pts` is supplied. Default is FALSE.
#' @param simplify Should the polygons be simplified? If TRUE, the output became lighter.
#' 
#' @return A Leaflet map.
#' 
#' @examples 
#' library(sdmvis)
#' # Load data
#' data("thresholded_sdm")
#' data("pa_data")
#' # Create a list of SDMs and names
#' sdm.list <- list(thresholded_sdm[[1]], thresholded_sdm[[2]])
#' sdm.names <- c("current", "future")
#' # Plot
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
sdm_thresh <- function(sdm, thresh, tname = NULL, pts = NULL, pal = NULL,
                       crs = "standard", cluster = FALSE, layernames = NULL,
                       simplify = TRUE, thresh.color = NULL){
        
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
        
        sdm.orig <- sdm
        
        # Reproject
        sdm <- lapply(sdm, projectRaster,
                      crs = crs,
                      method = "ngb")

        if (is.null(pal)) {
            
                pal <- "viridis"
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
                
                popdata <- as.character(rownames(pts))
                
                if (length(pts) == 2) {
                        colnames(pts) <- c("longitude", "latitude")
                        
                        overmap <- addMarkers(overmap,
                                   data = pts,
                                   clusterOptions = clopt,
                                   group = "Points",
                                   popup = popdata,
                                   weight = 2,
                                   radius = 5,
                                   opacity = 1,
                                   fillOpacity = 0.1
                        )
                } else{
                        pts <- pts[,1:3]
                        
                        colnames(pts) <- c("longitude", "latitude", "dsp")
                        
                        overmap <- addCircleMarkers(overmap,
                                              data = pts[pts[,3] == 1, 1:2],
                                              clusterOptions = clopt,
                                              group = "Presence Points",
                                              color = "blue",
                                              popup = popdata,
                                              weight = 2,
                                              radius = 5,
                                              opacity = 1,
                                              fillOpacity = 0.1
                        )
                        
                        overmap <- addCircleMarkers(overmap,
                                              data = pts[pts[,3] == 0, 1:2],
                                              clusterOptions = clopt,
                                              group = "Absence Points",
                                              color = "orange",
                                              popup = popdata,
                                              weight = 2,
                                              radius = 5,
                                              opacity = 1,
                                              fillOpacity = 0.1
                        )
                }
        }

        for (i in 1:length(sdm)) {
                binpal <- colorNumeric(palette = pal,
                                       values(sdm[[i]]),
                                       na.color = "transparent")
                
                overmap <- addRasterImage(overmap, sdm[[i]],
                                          colors = binpal,
                                          opacity = 0.9,
                                          group = lname[i])
                overmap <- addLegend(overmap,
                                     pal = binpal,
                                     values = values(sdm[[i]]),
                                     title = lname[i],
                                     opacity = 1,
                                     group = lname[i])
        }
        
        if (is.null(tname)) {
                tname <- paste("Threshold", 1:length(thresh))
        }
        
        if (is.null(thresh.color)) {
                cols <- c("#00CD66", "#00C5CD", "#FFA500", "#CD00CD")
                cols <- rep(cols, times = length(thresh))
        }
        
        for (i in 1:length(thresh)) {
                if (thresh <= 0) {
                        sdm.temp <- calc(sdm.orig[[i]],
                                         function(x){
                                                 x[x >= thresh] <- 1
                                                 x[x < thresh] <- NA
                                                 x
                                         })
                }
                if (thresh > 0) {
                        sdm.temp <- calc(sdm.orig[[i]],
                                         function(x){
                                                 x[x < thresh] <- NA
                                                 x[x >= thresh] <- 1
                                                 x
                                         })
                }
                
                sdm.temp <- rasterToPolygons(sdm.temp, dissolve = T)
                
                if (isTRUE(simplify)) {
                        sdm.temp <- rmapshaper::ms_simplify(sdm.temp,
                                                            keep = 0.1)
                }
                
                overmap <- addPolygons(overmap, data = sdm.temp,
                            opacity = 0.5,
                            color = cols[i],
                            fillOpacity = 0.2, group = tname[i])
        }
        
        if (!is.null(pts)) {
                if (length(pts) == 2) {
                        lname <- c("Points", lname, tname)
                } else{
                        lname <- c("Presence Points", "Absence Points", lname,
                                   tname)
                }
        }else{
                lname <- c(lname, tname)
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

#' SDM Leaflet map with different thresholds
#'
#' Create a Leaflet map with the results of an SDM and draw how different thresholds will result.
#' This map can be explored interactively in the viewer, so its possible to have a better comprehension of the outcomes of your SDM.
#' @param sdm The SDM/ENM result. Should be in the Raster* format.
#' @param thresh A vector containing the thresholds to be applied (numeric values).
#' @param tname An optional character vector containing the names of the thresholds to be used in the legend. If not supplied, numerals will be used instead.
#' @param pts A data frame containing the presence or presence/absence points (optional). The first column should be longitude (x) and the sencond latitude (y). In the case of presence/absence data, an additional collumn should be provided, coded as 0 (absence) and 1 (presence).
#' @param pal Character string indicating the name of the continuous mode palette (see \link[sdmvis]{gen_pal}). If not supplied, the default will be used.
#' @param crs Enables to change the default projection used in the Leaflet package. For now, not functional.
#' @param cluster Should the points be clustered (i.e., aggregated)? Only valid if `pts` is supplied. Default is FALSE.
#' @param simplify Should the polygons be simplified? If TRUE, the output became lighter.
#' @param thresh.color Vector of color(s) to be used for the threshold(s) polygon(s)
#' 
#' @return A Leaflet map.
#' 
#' @examples 
#' library(sdmvis)
#' # Load data
#' data("original_sdm")
#' data("pa_data")
#' 
#' # Plot
#' sdm_thresh(sdm = original_sdm[[1]],
#'            thresh = -2,
#'            tname = "TSS",
#'            pts = pa_data)
#' 
#' @import raster
#' @import leaflet
#' @import leaflet.extras
#' @import leafem
#' @export

setGeneric("sdm_thresh", function(sdm, thresh, tname = NULL, pts = NULL,
                                  pal = NULL,
                                  crs = "standard", cluster = FALSE,
                                  simplify = TRUE, thresh.color = NULL) {
        standardGeneric("sdm_thresh")
})

# Raster* method
.traster_method <- function(sdm, thresh, tname = NULL, pts = NULL, pal = NULL,
                           crs = "standard", cluster = FALSE,
                           simplify = TRUE, thresh.color = NULL){
        
        if (crs == "standard") {
                crs <- paste0("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0",
                              " +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
        }
        
        lname <- "SDM"
        
        sdm.orig <- sdm
        
        sdm <- projectRaster(sdm,
                             crs = crs,
                             method = "ngb")
        
        if (is.null(pal)) {
                
                pal <- "viridis"
        }
        
        basemap <- leaflet()
        # Base groups
        overmap <- addProviderTiles(basemap, "Esri.OceanBasemap",
                                    group = "Esri Ocean") %>%
                addProviderTiles("CartoDB.Positron",
                                 group = "CartoDB") %>%
                addProviderTiles("CartoDB.DarkMatter",
                                 group = "CartoDB Dark")
        
        if (!is.null(pts)) {
                if (cluster) {
                        clopt <- markerClusterOptions()
                }else{
                        clopt = NULL
                }
                
                popdata <- as.character(rownames(pts))
                
                if (length(pts) == 2) {
                        colnames(pts) <- c("longitude", "latitude")
                        
                        overmap <- addCircleMarkers(overmap,
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
        
        binpal <- colorNumeric(palette = pal,
                               values(sdm),
                               na.color = "transparent")
        
        overmap <- addRasterImage(overmap, sdm,
                                  colors = binpal,
                                  opacity = 0.9,
                                  group = lname)
        overmap <- addLegend(overmap,
                             pal = binpal,
                             values = values(sdm),
                             title = lname,
                             opacity = 1,
                             group = lname)
        
        if (is.null(tname)) {
                tname <- paste("Threshold", 1:length(thresh))
        }
        
        if (is.null(thresh.color)) {
                cols <- c("#00CD66", "#00C5CD", "#FFA500", "#CD00CD")
                cols <- rep(cols, times = length(thresh))
        }
        
        for (i in 1:length(thresh)) {
                if (thresh[i] <= 0) {
                        sdm.temp <- calc(sdm.orig,
                                         function(x){
                                                 x[x >= thresh[i]] <- 1
                                                 x[x < thresh[i]] <- NA
                                                 x
                                         })
                }
                if (thresh[i] > 0) {
                        sdm.temp <- calc(sdm.orig,
                                         function(x){
                                                 x[x < thresh[i]] <- NA
                                                 x[x >= thresh[i]] <- 1
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
        }
        
        # Layers control
        finalmap <- addLayersControl(
                overmap,
                baseGroups = c("Esri Ocean", "CartoDB", "CartoDB Dark"),
                overlayGroups = lname,
                options = layersControlOptions(collapsed = T),
                position = "bottomright")
        
        if (!is.null(pts)) {
                if (length(pts) == 2) {
                        finalmap <- finalmap %>%
                                hideGroup(lname[lname != lname[2:3]]) %>% 
                                addFullscreenControl()
                } else{
                        finalmap <- finalmap %>%
                                hideGroup(lname[lname != lname[3:4]]) %>% 
                                addFullscreenControl()
                }
        } else{
                if (thresh > 1) {
                        finalmap <- finalmap %>%
                                hideGroup(lname[lname != lname[1:2]]) %>% 
                                addFullscreenControl()
                }else{
                        finalmap <- finalmap %>%
                                addFullscreenControl()
                }
                        
        }
        
        finalmap %>% leafem::addMouseCoordinates()

        
}

#' @describeIn sdm_thresh Method for Raster*
setMethod("sdm_thresh", signature = c(sdm = "Raster"), .traster_method)

# Data frame methdd
.tdf_method <- function(sdm, thresh, tname = NULL, pts = NULL, pal = NULL,
                        crs = "standard", cluster = FALSE,
                        simplify = TRUE, thresh.color = NULL){
        
        sdm <- rasterFromXYZ(sdm[,1:3])
        
        if (crs == "standard") {
                crs <- paste0("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0",
                              " +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
        }
        
        lname <- "SDM"
        
        sdm.orig <- sdm
        
        sdm <- projectRaster(sdm,
                             crs = crs,
                             method = "ngb")
        
        if (is.null(pal)) {
                
                pal <- "viridis"
        }
        
        basemap <- leaflet()
        # Base groups
        overmap <- addProviderTiles(basemap, "Esri.OceanBasemap",
                                    group = "Esri Ocean") %>%
                addProviderTiles("CartoDB.Positron",
                                 group = "CartoDB") %>%
                addProviderTiles("CartoDB.DarkMatter",
                                 group = "CartoDB Dark")
        
        if (!is.null(pts)) {
                if (cluster) {
                        clopt <- markerClusterOptions()
                }else{
                        clopt = NULL
                }
                
                popdata <- as.character(rownames(pts))
                
                if (length(pts) == 2) {
                        colnames(pts) <- c("longitude", "latitude")
                        
                        overmap <- addCircleMarkers(overmap,
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
        
        binpal <- colorNumeric(palette = pal,
                               values(sdm),
                               na.color = "transparent")
        
        overmap <- addRasterImage(overmap, sdm,
                                  colors = binpal,
                                  opacity = 0.9,
                                  group = lname)
        overmap <- addLegend(overmap,
                             pal = binpal,
                             values = values(sdm),
                             title = lname,
                             opacity = 1,
                             group = lname)
        
        if (is.null(tname)) {
                tname <- paste("Threshold", 1:length(thresh))
        }
        
        if (is.null(thresh.color)) {
                cols <- c("#00CD66", "#00C5CD", "#FFA500", "#CD00CD")
                cols <- rep(cols, times = length(thresh))
        }
        
        for (i in 1:length(thresh)) {
                if (thresh[i] <= 0) {
                        sdm.temp <- calc(sdm.orig,
                                         function(x){
                                                 x[x >= thresh[i]] <- 1
                                                 x[x < thresh[i]] <- NA
                                                 x
                                         })
                }
                if (thresh[i] > 0) {
                        sdm.temp <- calc(sdm.orig,
                                         function(x){
                                                 x[x < thresh[i]] <- NA
                                                 x[x >= thresh[i]] <- 1
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
        }
        
        # Layers control
        finalmap <- addLayersControl(
                overmap,
                baseGroups = c("Esri Ocean", "CartoDB", "CartoDB Dark"),
                overlayGroups = lname,
                options = layersControlOptions(collapsed = T),
                position = "bottomright")
        
        if (!is.null(pts)) {
                if (length(pts) == 2) {
                        finalmap <- finalmap %>%
                                hideGroup(lname[lname != lname[2:3]]) %>% 
                                addFullscreenControl()
                } else{
                        finalmap <- finalmap %>%
                                hideGroup(lname[lname != lname[3:4]]) %>% 
                                addFullscreenControl()
                }
        } else{
                if (thresh > 1) {
                        finalmap <- finalmap %>%
                                hideGroup(lname[lname != lname[1:2]]) %>% 
                                addFullscreenControl()
                }else{
                        finalmap <- finalmap %>%
                                addFullscreenControl()
                }
                
        }
        
        finalmap %>% leafem::addMouseCoordinates()
        
        
}

#' @describeIn sdm_thresh Method for data frames
setMethod("sdm_thresh", signature = c(sdm = "data.frame"), .tdf_method)

#' SDM Leaflet map highlighting extreme values
#'
#' Create a Leaflet map highlighting extreme values in an SDM output. This can either be minimum, maximum or both values set as a quantile.
#' 
#' @param sdm The SDM/ENM result. Should be a data.frame (XYZ columns) or in the Raster* format.
#' @param quantile A single numeric value between 0 and 1 indicating which quantile to use (will be the same for both sides, if `both.sides` set to TRUE). For example, if you want the 25% higher values, you can set the quantile to 0.75. If you want both higher and lower 25%, just put 0.75, and set `both.sides = TRUE`. Note: setting quantile 1 makes no sense, as it will be 100%. The same for setting it 0.5 and both.sides = TRUE. 
#' @param both.sides If set to TRUE, than both the higher and lower values will be ploted.
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
#' data("original_sdm")
#' data("pa_data")
#' 
#' # Plot
#' .hraster_method(sdm = original_sdm[[1]],
#'                 quantile = .75,
#'                 pts = pa_data,
#'                 both.sides = T)
#' 
#' @import raster
#' @import leaflet
#' @import leaflet.extras
#' @import leafem
#' @export

setGeneric("sdm_highlight", function(sdm, ...) {
        standardGeneric("sdm_highlight")
})

# Raster* method
.hraster_method <- function(sdm, quantile, both.sides = FALSE,
                            pts = NULL, pal = NULL,
                            crs = "standard", cluster = FALSE,
                            simplify = TRUE){
        
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
        
        if (isTRUE(both.sides)) {
                if (quantile < 0.5) {
                        thresh <- c(quantile, (1-quantile))
                }else{
                        thresh <- c((1-quantile), quantile)
                }
                
                tname <- c("Lower", "Higher")
        }else{
                thresh <- quantile
                
                tname <- "Higher quantile"
        }
        
        cols <- c("red", "blue")
        
        if (!isTRUE(both.sides)) {
                
                v <- na.omit(values(sdm.orig))
                
                q <- quantile(v, thresh)
                
                if (q <= 0) {
                        sdm.temp <- calc(sdm.orig,
                                         function(x){
                                                 x[x >= q] <- 1
                                                 x[x < q] <- NA
                                                 x
                                         })
                }
                if (q > 0) {
                        sdm.temp <- calc(sdm.orig,
                                         function(x){
                                                 x[x < q] <- NA
                                                 x[x >= q] <- 1
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
                                       color = "red",
                                       fillOpacity = 0.2, group = tname)
        }
        
        if (isTRUE(both.sides)) {
                
                for (i in 1:2) {
                        v <- na.omit(values(sdm.orig))
                        
                        q <- quantile(v, thresh[i])
                        
                        if (thresh[i] < 0.5) {
                                sdm.temp <- calc(sdm.orig,
                                                 function(x){
                                                         x[x > q] <- NA
                                                         x[x <= q] <- 1
                                                         x
                                                 })
                        }
                        
                        if (thresh[i] > 0.5) {
                                sdm.temp <- calc(sdm.orig,
                                                 function(x){
                                                         x[x < q] <- NA
                                                         x[x >= q] <- 1
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
                                               fillOpacity = 0.2,
                                               group = tname[i])
                }
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

#' @describeIn sdm_highlight Method for Raster*
setMethod("sdm_highlight", signature = c(sdm = "Raster"), .hraster_method)

# Data frame methdd
.hdf_method <- function(sdm, quantile, both.sides = FALSE,
                        pts = NULL, pal = NULL,
                        crs = "standard", cluster = FALSE,
                        simplify = TRUE){
        
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
        
        if (isTRUE(both.sides)) {
                if (quantile < 0.5) {
                        thresh <- c(quantile, (1-quantile))
                }else{
                        thresh <- c((1-quantile), quantile)
                }
                
                tname <- c("Lower", "Higher")
        }else{
                thresh <- quantile
                
                tname <- "Higher quantile"
        }
        
        cols <- c("red", "blue")
        
        if (!isTRUE(both.sides)) {
                
                v <- na.omit(values(sdm.orig))
                
                q <- quantile(v, thresh)
                
                if (q <= 0) {
                        sdm.temp <- calc(sdm.orig,
                                         function(x){
                                                 x[x >= q] <- 1
                                                 x[x < q] <- NA
                                                 x
                                         })
                }
                if (q > 0) {
                        sdm.temp <- calc(sdm.orig,
                                         function(x){
                                                 x[x < q] <- NA
                                                 x[x >= q] <- 1
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
                                       color = "red",
                                       fillOpacity = 0.2, group = tname)
        }
        
        if (isTRUE(both.sides)) {
                
                for (i in 1:2) {
                        v <- na.omit(values(sdm.orig))
                        
                        q <- quantile(v, thresh[i])
                        
                        if (thresh[i] < 0.5) {
                                sdm.temp <- calc(sdm.orig,
                                                 function(x){
                                                         x[x > q] <- NA
                                                         x[x <= q] <- 1
                                                         x
                                                 })
                        }
                        
                        if (thresh[i] > 0.5) {
                                sdm.temp <- calc(sdm.orig,
                                                 function(x){
                                                         x[x < q] <- NA
                                                         x[x >= q] <- 1
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
                                               fillOpacity = 0.2,
                                               group = tname[i])
                }
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

#' @describeIn sdm_highlight Method for data frames
setMethod("sdm_highlight", signature = c(sdm = "data.frame"), .hdf_method)

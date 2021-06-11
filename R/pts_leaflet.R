#' Occurrence points Leaflet map
#'
#' Create a Leaflet map with the presence or presence/absence locations.
#' This map can be explored interactively in the viewer, so its possible to have a better comprehension of the outcomes of your SDM.
#' @param pts A data frame containing the presence or presence/absence points. The first column should be longitude (x) and the sencond latitude (y). In the case of presence/absence data, an additional collumn should be provided, coded as 0 (absence) and 1 (presence).
#' @param cols Character string indicating the color for the points or a character vector of length 2 indicating the colors for the presence and absence, respectively. If not supplied, the default will be used.
#' @param cluster Should the points be clustered (i.e., aggregated)? Only valid if `pts` is supplied. Default is FALSE.
#' @param popinfo An optional vector of values to be used in the popup box when clicking in a point. Can be either a number indicating the column of the data.frame (this may be useful for e.g. showing value for a certain variable) or a character vector of length = number of points. If not supplied, the number of the row will be used (this may be useful for spotting points with problems).
#' 
#' @return A Leaflet map with the points.
#' 
#' @examples 
#' library(sdmvis)
#' # Load data
#' data("pa_data")
#' # Plot
#' pts_leaflet(pts = pa_data)
#' 
#' @import raster
#' @import leaflet
#' @import leaflet.extras
#' @import leafem
#' @export

pts_leaflet <- function(pts, cols = NULL,
                  cluster = FALSE, popinfo = NULL){

        basemap <- leaflet()
                # Base groups
        overmap <- addProviderTiles(basemap, "Esri.OceanBasemap",
                                    group = "Esri Ocean") %>%
                addProviderTiles("CartoDB.Positron",
                                    group = "CartoDB") %>%
                addProviderTiles("CartoDB.DarkMatter",
                                    group = "CartoDB Dark")
        
        if (cluster) {
                clopt <- markerClusterOptions()
        }else{
                clopt = NULL
        }
        
        if (!is.null(popinfo)) {
                popdata <- as.character(rownames(pts))
        } else{
                if (length(popinfo) == 1) {
                        popdata <- pts[,popinfo]
                } else{
                        if (length(popinfo) != nrow(pts)) {
                                stop("popinfo should have the same number of points.")
                        } else{
                                popdata <- popinfo
                        }
                }
        }
        
        if (length(pts) == 2) {
                
                if (!is.null(cols)) {
                        if (cols > 1) {
                                warning("More colors than points categories. Default will be used instead.")
                                
                                cols <- "blue"
                        }
                } else {
                        
                        cols <- "blue"
                }
                
                colnames(pts) <- c("longitude", "latitude")
                
                overmap <- addCircleMarkers(overmap,
                                            data = pts,
                                            clusterOptions = clopt,
                                            group = "Points",
                                            popup = popdata,
                                            weight = 2,
                                            radius = 5,
                                            opacity = 1,
                                            fillOpacity = 0.1,
                                            color = cols
                )
        } else{
                
                if (!is.null(cols)) {
                        if (cols < 2) {
                                warning("Less colors than points categories. Default will be used instead.")
                                
                                cols <- c("blue", "orange")
                        }
                        if (cols > 2) {
                                warning("More colors than points categories. First two will be used.")
                                
                                cols <- cols[1:2]
                        }
                } else {
                        
                        cols <- c("blue", "orange")
                }
                
                pts <- pts[,1:3]
                
                colnames(pts) <- c("longitude", "latitude", "dsp")
                
                overmap <- addCircleMarkers(overmap,
                                            data = pts[pts[,3] == 1, 1:2],
                                            clusterOptions = clopt,
                                            group = "Presence Points",
                                            color = cols[1],
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
                                            color = cols[2],
                                            popup = popdata,
                                            weight = 2,
                                            radius = 5,
                                            opacity = 1,
                                            fillOpacity = 0.1
                )
        }

        
        if (length(pts) == 2) {
                lname <- c("Points")
        } else{
                lname <- c("Presence Points", "Absence Points")
        }

                # Layers control
        finalmap <- addLayersControl(
                        overmap,
                        baseGroups = c("Esri Ocean", "CartoDB", "CartoDB Dark"),
                        overlayGroups = lname,
                        options = layersControlOptions(collapsed = T),
                        position = "bottomright")

        finalmap <- addFullscreenControl(finalmap)

        finalmap %>% leafem::addMouseCoordinates()

        
}
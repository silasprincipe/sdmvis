#' SDM Leaflet map
#'
#' Convert degrees Fahrenheit temperatures to degrees Celsius
#' @param F_temp The temperature in degrees Fahrenheit
#' @return The temperature in degrees Celsius
#' @examples 
#' 
#' @export
sdm_leaflet <- function(sdm, mode = "bin", pts = NULL, pal = NULL,
                        layernames = NULL, crs = "standard"){
        
        # Load needed libraries
        library("raster")
        library("leaflet")
        library("leaflet.extras")
        
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
        
        # Reproject
        sdm <- lapply(sdm, projectRaster,
                      crs = crs,
                      method = "ngb")

        # Convert to factor
        sdm <- lapply(sdm, as.factor)

        # Plot
        if (mode == "bin") {
                if (is.null(pal)) {
                        binpal <- leaflet::colorFactor(c("#2c3e50","#00DBC7"),
                                                       levels = c(0,1),
                                                       na.color = NA)
                }else{
                        if (pal == "green") {
                                colors <- c("#2c3e50","#00DBC7")
                        }
                        if (pal == "orange") {
                                colors <- c("#2c3e50","#00DBC7")
                        }
                        if (pal == "blue") {
                                colors <- c("#2c3e50","#00DBC7")
                        }
                        if (length(pal) > 1) {
                                colors <- pal
                        }
                        
                        binpal <- leaflet::colorFactor(colors,
                                                       levels = c(0,1),
                                                       na.color = NA)
                        
                }
        }

        basemap <- leaflet()
                # Base groups
        basemap <- addProviderTiles(basemap, "Esri.OceanBasemap",
                                 group = "Esri Ocean")

                # Overlay groups
        overmap <- addRasterImage(basemap, sdm[[1]], colors = binpal, opacity = 0.9, group = "Suitability projection")
        overmap <- addLegend(overmap, colors = c("#2c3e50","#00DBC7"), labels = c("Absence", "Presence"),
                          title = "Habitat suitability", opacity = 1,
                          group = "Suitability projection")

                # Layers control
        finalmap <- addLayersControl(
                        overmap,
                        baseGroups = c("Esri Ocean", "CartoDB", "CartoDB Dark"),
                        overlayGroups = c("Suitability projection"),
                        options = layersControlOptions(collapsed = T),
                        position = "bottomright")

        finalmap <- addFullscreenControl(finalmap)

        finalmap
        #basemap
        
}

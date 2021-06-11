#' Environmental variables Leaflet map
#'
#' Create a Leaflet map with the environmental variables.
#' This map can be explored interactively in the viewer.
#' @param var A raster or data.frame with the environmental variables. In the case of a data.frame, the first two collumns should be, respectively, longitude and latitude.
#' @param pal Character string indicating the name of the palette (see \link[sdmvis]{gen_pal}). If not supplied, the default will be used.
#' @param layernames An optional character vector indicating the names of the layers. This will be used in the legend. If not supplied, names will be extracted from the RasterLayer.
#' @param crs Enables to change the default projection used in the Leaflet package. For now, not functional.
#' @param pts A data frame containing the presence or presence/absence points (optional). The first column should be longitude (x) and the sencond latitude (y). In the case of presence/absence data, an additional collumn should be provided, coded as 0 (absence) and 1 (presence).
#' @param cluster Should the points be clustered (i.e., aggregated)? Only valid if `pts` is supplied. Default is FALSE.
#' @param varsummary If set to TRUE, an RMarkdown file will be generated with a series of summary statistics for the variables. Points should also be supplied in that case. This is still quite experimental. Default is FALSE.
#' @param mess If set to TRUE, an aditional layer is included with the MESS map, using as a reference the environmental data in the presence/absence points (i.e. the data that will be used to train the model). In the case you are plotting future environmental data, you should supply a raster* in the mess.ref argument.
#' @param mess.ref If plotting future layers and MESS = TRUE, a Raster* with the same names of the provided layers from where the reference points will be extracted.
#' 
#' @return A Leaflet map and optionally an .html file with summary statistics.
#' 
#' @examples 
#' library(sdmvis)
#' data("pa_data")
#' data("env_data")
#'
#' var_leaflet(var = env_data, pts = pa_data, mess = FALSE)
#'
#' 
#' @import raster
#' @import leaflet
#' @import leaflet.extras
#' @import leafem
#' @import ecospat
#' 
#' @export

setGeneric("var_leaflet", function(var, ...) {
        standardGeneric("var_leaflet")
})

# Raster* method
.vr_method <- function(var, pts = NULL, pal = NULL,
                  layernames = NULL, crs = "standard", cluster = FALSE,
                  varsummary = FALSE, mess = TRUE, mess.ref = NULL){
        
        if (crs == "standard") {
                crs <- paste0("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0",
                " +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
        }
        
        if (!is.null(layernames)) {
                lname <- layernames
        } else{
                
               lname <- names(var)
        }
        
        if (isTRUE(mess) | isTRUE(varsummary)) {
                if (!is.null(mess.ref)) {
                        env <- mess.ref
                        
                        var.o <- var
                } else {
                        env <- var
                        
                        var.o <- var
                }
        }
        
        var <- projectRaster(var,
                             crs = crs,
                             method = "bilinear")

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
        
        if(is.null(pal)){
                pal <- "viridis"
        }

        
        for (i in 1:nlayers(var)) {
                
                binpal <- colorNumeric(palette = pal,
                                       values(var[[i]]),
                                       na.color = "transparent")
                
                overmap <- addRasterImage(overmap, var[[i]],
                                          colors = binpal,
                                          opacity = 0.9,
                                          group = lname[i])
                overmap <- addLegend(overmap,
                                     pal = binpal,
                                     values = values(var[[i]]),
                                     title = lname[i],
                                     opacity = 1,
                                     group = lname[i])
        }
        
        if (isTRUE(mess)) {
                
                if(is.null(pts)) {
                        stop("For the MESS map, points should be supplied.")
                }
                
                cat("Obtaining MESS map using ecospat. This may take some minutes... \n")
                
                var.m <- data.frame(rasterToPoints(var.o))
                
                env.pts <- extract(env, pts[,1:2])
                
                env.pts <- data.frame(cbind(pts[,1:2], env.pts))
                
                env.pts <- na.omit(env.pts)

                colnames(env.pts) <- colnames(var.m)

                m <- ecospat::ecospat.mess(proj = var.m, cal = env.pts)
                
                m <- data.frame(m)
                
                m <- rasterFromXYZ(m[,1:3])
                
                crs(m) <- crs
                
                binpal <- leaflet::colorNumeric(palette = "Spectral",
                                       values(m),
                                       na.color = "transparent")
                
                overmap <- addRasterImage(overmap, m,
                                          colors = binpal,
                                          opacity = 0.9,
                                          group = "MESS")
                overmap <- addLegend(overmap,
                                     pal = binpal,
                                     values = values(m),
                                     title = "MESS",
                                     opacity = 1,
                                     group = "MESS")
                
                lname <- c(lname, "MESS")
                
        }
        
        if (!is.null(pts)) {
                if (length(pts) == 2) {
                        lname <- c("Points", lname)
                } else{
                        lname <- c("Presence Points", "Absence Points", lname)
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


        if (isTRUE(varsummary)) {
                
                sdm_vis_rdm_data <- list(var.o, pts)
                
                names(sdm_vis_rdm_data) <- c("variables", "points")
                
                save(sdm_vis_rdm_data, file = 'sdm_vis_rdm_data.rda')
                
                rmd.text <- c(
                        
                        "---",
                        "title: 'Environmental variables report'",
                        "output: html_document",
                        "---",
                        " ",  
                        "## Variables summary",
                        " ",
                        "Summary of the full environmental layers.",
                        " ",
                        "```{r echo=FALSE}",
                        "#library(sdmvis)",
                        "source('R/var_report.R')",
                        "library(ggplot2)",
                        "library(raster)",
                        "load('sdm_vis_rdm_data.rda')",
                        "var <- sdm_vis_rdm_data[[1]]",
                        "pts <- sdm_vis_rdm_data[[2]]",
                        "var_report(var = var, pts = pts, mode = 'full.summary')",
                        "```",
                        " ",
                        " ",
                        "Summary of environmental layers for presence and absence points.",
                        " ",
                        " ",
                        "```{r echo=FALSE}",
                        "ptsum <- var_report(var = var, pts = pts, mode = 'pts.summary')",
                        "ptsum[[1]]",
                        "ptsum[[2]]",
                        "```",
                        " ",
                        "## Environmental layers plots",
                        "### Density plots",
                        " ",
                        "```{r echo=FALSE}",
                        "var_report(var = var, pts = pts, mode = 'd.plots')",
                        "```",
                        " ",
                        "### Points plots",
                        " ",
                        "Dot is the median, line is mean +- SD.",
                        " ",
                        "```{r echo=FALSE}",
                        "var_report(var = var, pts = pts, mode = 'pt.plots')",
                        "```"
                        
                        
                )
                
                writeLines(rmd.text, "report_output.Rmd")
                
                rmarkdown::render("report_output.Rmd")
                
                browseURL("report_output.html")
                
                unlink(c("report_output.Rmd", "sdm_vis_rdm_data.rda"))
                
                cat("Report done! It will open in your default browser.")
        }

        finalmap %>% leafem::addMouseCoordinates()
}

# Data frame methdd
.var_df_method <- function(var, pts = NULL, pal = NULL,
                           layernames = NULL, crs = "standard", cluster = FALSE,
                           varsummary = FALSE, mess = TRUE, mess.ref = NULL){
        
        if (length(var) == 3) {
                var <- rasterFromXYZ(var[,1:3])
        }else{
                if (length(var) < 3) {
                        stop("Data frame should have at least three columns.")
                }
                if (length(var) > 3) {
                        
                        for (i in 3:length(var)) {
                                temp <- rasterFromXYZ(var[,c(1, 2, i)])
                                
                                names(temp) <- colnames(var)[i]
                                
                                if (i == 3) {
                                        trast <- stack(temp)
                                }else{
                                        trast <- stack(trast, temp)
                                }
                        }
                        
                        var <- trast
                        
                        rm(trast)
                }
        }
        
        
        
        if (crs == "standard") {
                crs <- paste0("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0",
                              " +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
        }
        
        if (!is.null(layernames)) {
                lname <- layernames
        } else{
                
                lname <- names(var)
        }
        
        if (isTRUE(mess) | isTRUE(varsummary)) {
                if (!is.null(mess.ref)) {
                        env <- mess.ref
                        
                        var.o <- var
                } else {
                        env <- var
                        
                        var.o <- var
                }
        }
        
        var <- projectRaster(var,
                             crs = crs,
                             method = "bilinear")
        
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
        
        if(is.null(pal)){
                pal <- "viridis"
        }
        
        
        for (i in 1:nlayers(var)) {
                
                binpal <- colorNumeric(palette = pal,
                                       values(var[[i]]),
                                       na.color = "transparent")
                
                overmap <- addRasterImage(overmap, var[[i]],
                                          colors = binpal,
                                          opacity = 0.9,
                                          group = lname[i])
                overmap <- addLegend(overmap,
                                     pal = binpal,
                                     values = values(var[[i]]),
                                     title = lname[i],
                                     opacity = 1,
                                     group = lname[i])
        }
        
        if (isTRUE(mess)) {
                
                if(is.null(pts)) {
                        stop("For the MESS map, points should be supplied.")
                }
                
                cat("Obtaining MESS map using ecospat. This may take some minutes... \n")
                
                var.m <- data.frame(rasterToPoints(var.o))
                
                env.pts <- raster::extract(env, pts[,1:2])
                
                env.pts <- na.omit(env.pts)
                
                env.pts <- data.frame(cbind(pts[,1:2], env.pts))
                
                colnames(env.pts) <- colnames(var.m)
                
                m <- ecospat::ecospat.mess(proj = var.m, cal = env.pts)
                
                m <- data.frame(m)
                
                m <- rasterFromXYZ(m[,1:3])
                
                crs(m) <- crs
                
                binpal <- leaflet::colorNumeric(palette = "Spectral",
                                                values(m),
                                                na.color = "transparent")
                
                overmap <- addRasterImage(overmap, m,
                                          colors = binpal,
                                          opacity = 0.9,
                                          group = "MESS")
                overmap <- addLegend(overmap,
                                     pal = binpal,
                                     values = values(m),
                                     title = "MESS",
                                     opacity = 1,
                                     group = "MESS")
                
                lname <- c(lname, "MESS")
                
        }
        
        if (!is.null(pts)) {
                if (length(pts) == 2) {
                        lname <- c("Points", lname)
                } else{
                        lname <- c("Presence Points", "Absence Points", lname)
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
        
        if (isTRUE(varsummary)) {
                
                sdm_vis_rdm_data <- list(var.o, pts)
                
                names(sdm_vis_rdm_data) <- c("variables", "points")
                
                save(sdm_vis_rdm_data, file = 'sdm_vis_rdm_data.rda')
                
                rmd.text <- c(
                        
                        "---",
                        "title: 'Environmental variables report'",
                        "output: html_document",
                        "---",
                        " ",  
                        "## Variables summary",
                        " ",
                        "Summary of the full environmental layers.",
                        " ",
                        "```{r echo=FALSE}",
                        "#library(sdmvis)",
                        "source('R/var_report.R')",
                        "library(ggplot2)",
                        "library(raster)",
                        "load('sdm_vis_rdm_data.rda')",
                        "var <- sdm_vis_rdm_data[[1]]",
                        "pts <- sdm_vis_rdm_data[[2]]",
                        "var_report(var = var, pts = pts, mode = 'full.summary')",
                        "```",
                        " ",
                        " ",
                        "Summary of environmental layers for presence and absence points.",
                        " ",
                        " ",
                        "```{r echo=FALSE}",
                        "ptsum <- var_report(var = var, pts = pts, mode = 'pts.summary')",
                        "ptsum[[1]]",
                        "ptsum[[2]]",
                        "```",
                        " ",
                        "## Environmental layers plots",
                        "### Density plots",
                        " ",
                        "```{r echo=FALSE}",
                        "var_report(var = var, pts = pts, mode = 'd.plots')",
                        "```",
                        " ",
                        "### Points plots",
                        " ",
                        "Dot is the median, line is mean +- SD.",
                        " ",
                        "```{r echo=FALSE}",
                        "var_report(var = var, pts = pts, mode = 'pt.plots')",
                        "```"
                        
                        
                )
                
                writeLines(rmd.text, "report_output.Rmd")
                
                rmarkdown::render("report_output.Rmd")
                
                browseURL("report_output.html")
                
                unlink(c("report_output.Rmd", "sdm_vis_rdm_data.rda"))
                
                cat("Report done! It will open in your default browser.")
        }
        
        finalmap %>% leafem::addMouseCoordinates()
}

#' @describeIn var_leaflet Method for data frames
setMethod("var_leaflet", signature = c(var = "data.frame"), .var_df_method)
#' @describeIn var_leaflet Method for raster
setMethod("var_leaflet", signature = c(var = "Raster"), .vr_method)

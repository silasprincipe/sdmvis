#' Summary stats of environmental variables.
#'
#' Gets summary statistics of environmental variables (full and extracted by points) and also plot some graphics that helps to understand the data.
#' It's mainly used by the \link[sdmvis]{var_leaflet} function if the option to get an html summary is set to TRUE. However, it's also possible to use it individually, and also to produce interactive documents in R Markdown.
#' @param var A raster with the environmental variables.
#' @param pts A data frame containing the presence/absence points. The first column should be longitude (x) and the sencond latitude (y). An additional collumn should be provided, coded as 0 (absence) and 1 (presence).
#' @param mode Which type of result return:
#' \itemize{
#'  \item{"full.summary"}{A summary of each variable.}
#'  \item{"pts.summary"}{A summary for each variable, extracted from the points.}
#'  \item{"d.plots"}{Density plots for full data, presence data and absence data.}
#'  \item{"pt.plots"}{Points plot with median and bars of mean+-SD.}
#'  \item{"de.plots"}{Density plots of the full data only, but with points of presence-absence.}
#' }
#' @param type Which type of output for the summaries (either "rmd" for markdown tables or any other value for a list with the summaries as a data.frame).
#' 
#' @return Summary statistics or plots.
#' 
#' @examples 
#' library(sdmvis)
#' data("pa_data")
#' data("env_data")
#' 
#' var_report(env_data, pa_data, mode = "d.plots")
#' 
#' @import raster
#' @import ggplot2
#' @import knitr
#' @export
var_report <- function(var, pts, mode, type = "rmd") {
        
        get.sum <- function(x){
                df <- data.frame(matrix(nrow = 6,
                                        ncol = nlayers(var)))
                
                for (i in 1:nlayers(var)) {
                        df[1,i] <- min(x[,i])
                        df[2,i] <- quantile(x[,i])[2]
                        df[3,i] <- median(x[,i])
                        df[4,i] <- mean(x[,i])
                        df[5,i] <- quantile(x[,i])[4]
                        df[6,i] <- max(x[,i])
                }
                
                colnames(df) <- names(var)
                
                rownames(df) <- c("Min.", "1st Qu.", "Median", "Mean",
                                  "3rd Qu.", "Max.")
                
                df
        }
        
        if (mode == "full.summary") {
                edata <- data.frame(rasterToPoints(var))
                
                df <- get.sum(edata)
                
                if (type == "rmd") {
                        return(knitr::kable(df))
                } else {
                        return(df)
                }
        }
        
        
        if (mode == "pts.summary") {
                
                pts.pres <- pts[pts[,3] == 1, ]
                pts.abs <- pts[pts[,3] == 0, ]
                
                edata.pres <- data.frame(extract(var, pts.pres[,1:2]))
                edata.abs <- data.frame(extract(var, pts.abs[,1:2]))
                
                if (type == "rmd") {
                        
                        df.pres <- get.sum(edata.pres)
                        df.abs <- get.sum(edata.abs)
                        
                        lt <- list(knitr::kable(df.pres),
                                   knitr::kable(df.abs))
                        
                        return(lt)
                } else {
                        pts.list <- list(get.sum(edata.pres),
                                         get.sum(edata.abs))
                        
                        names(pts.list) <- c("presence", "absence")
                        
                        return(pts.list)
                }
                
        }
        
        if (mode == "d.plots") {
                
                for (i in 1:nlayers(var)) {
                        
                        r.val <- data.frame(rasterToPoints(var[[i]]))
                        
                        pts.val <- pts[,1:2]
                        pts.val$values <- raster::extract(var[[i]], pts[,1:2])
                        
                        pts.val$code <- ifelse(pts[,3] == 1, "PRES", "ABS")
                        
                        r.val$code <- "ALL"
                        
                        colnames(pts.val) <- c("x", "y", "values", "code")
                        colnames(r.val) <- c("x", "y", "values", "code")
                        
                        total <- rbind(r.val, pts.val)
                        
                        g <- ggplot(total, aes(x = values, colour = code,
                                          fill = code)) +
                                geom_density(position="identity",
                                             size = 1, alpha = 0.7) +
                                scale_x_continuous(name = names(var[[i]]),
                                                   expand = c(0,0)) +
                                scale_y_continuous(name = "Density",
                                                   expand = expansion(mult = c(0, .1))) +
                                ggtitle(paste0("Density plot of ",
                                               names(var[[i]]))) +
                                theme_bw() +
                                theme(plot.title = element_text(size = 14,
                                                                face = "bold"),
                                      text = element_text(size = 12)) +
                                scale_color_manual(values = c("#00915E",
                                                              "#F27500",
                                                              "#004FFA"))+
                                scale_fill_manual(values = c("#00915E",
                                                             "#F27500",
                                                             "#004FFA"))+
                                theme(legend.title = element_blank())
                        
                        plot(g)
                }
                

        }
        
        if (mode == "pt.plots") {
                
                for (i in 1:nlayers(var)) {
                        
                        r.val <- data.frame(rasterToPoints(var[[i]]))
                        
                        pts.val <- pts[,1:2]
                        pts.val$values <- raster::extract(var[[i]], pts[,1:2])
                        
                        pts.val$code <- ifelse(pts[,3] == 1, "PRES", "ABS")
                        
                        r.val$code <- "ALL"
                        
                        colnames(pts.val) <- c("x", "y", "values", "code")
                        colnames(r.val) <- c("x", "y", "values", "code")
                        
                        total <- rbind(r.val, pts.val)
                        
                        data_summary <- function(x) {
                                m <- mean(x)
                                med <- median(x)
                                ymin <- m-sd(x)
                                ymax <- m+sd(x)
                                return(c(y=med,ymin=ymin,ymax=ymax))
                        }
                        
                        j <- ggplot(total, aes(x = code, y = values, colour = code))+
                                #geom_boxplot()+
                                geom_jitter(alpha = 0.4)+
                                stat_summary(fun.data=data_summary, color="black")+
                                ggtitle(paste0("Values, median and sd of ",
                                               names(var[[i]]))) +
                                theme_bw() +
                                theme(plot.title = element_text(size = 14,
                                                                face = "bold"),
                                      text = element_text(size = 12)) +
                                scale_color_manual(values = c("#00915E",
                                                              "#F27500",
                                                              "#004FFA"))+
                                theme(legend.title = element_blank())
                        
                        
                        plot(j) 
                }
        }
        
        if (mode == "de.plots") {
                
                r.val <- data.frame(rasterToPoints(var[[i]]))
                
                pts.val <- pts[,1:2]
                pts.val$values <- raster::extract(var[[i]], pts[,1:2])
                
                pts.val$code <- ifelse(pts[,3] == 1, "PRES", "ABS")
                
                r.val$code <- "ALL"
                
                colnames(pts.val) <- c("x", "y", "values", "code")
                colnames(r.val) <- c("x", "y", "values", "code")
                
                g <- ggplot() +
                        geom_density(data = r.val,
                                     aes(x = values),
                                     color = "darkgrey",
                                     fill = "grey",
                                     position="identity",
                                     size = 1, alpha = 0.7) +
                        geom_jitter(data = pts.val,
                                    aes(x = values, y = 0, color = code),
                                    height = 0.02, alpha = 0.3)+
                        # geom_rug(sides="b", data = pts.val,
                        #          aes(x = values, color = code),
                        #          alpha = 0.4)+
                        scale_x_continuous(name = names(var[[i]]),
                                           expand = c(0,0)) +
                        scale_y_continuous(name = "Density",
                                           expand = expansion(mult = c(0, .1))) +
                        ggtitle(paste0("Density plot of the full data of ",
                                       names(var[[i]]))) +
                        theme_bw() +
                        theme(plot.title = element_text(size = 14,
                                                        face = "bold"),
                              text = element_text(size = 12)) +
                        scale_color_manual(values = c("#00915E",
                                                      "#F27500"),
                         guide = guide_legend(override.aes = list(size = 3,
                                                                 alpha = 1)))+
                        theme(legend.title = element_blank())
                
                plot(g)
        }
        
        
}

#' Presence/absence data for a marine species
#'
#' A dataset containing presence and absence data for a marine species. This was extracted from an SDM for the coral species *Montastraea cavernosa*, but DOES NOT reflect the actual species distribution. You should think of it as a **virtual species** dataset.
#'
#' @format A data frame with 4400 rows and 3 variables:
#' \describe{
#'   \item{x}{Longitude}
#'   \item{y}{Latitude}
#'   \item{pa}{Indicates presence (1) or absence (0)}
#' }
#' @source The author.
"pa_data"

#' A SDM for a marine species
#'
#' This consists of a RasterStack of current and future projections for a SDM generated with BRT (Boosted Regression Trees) for the marine species using only two variables.
#'
#' @format A RasterStack with two layers:
#' \describe{
#'   \item{current.map}{Projection for the current period.}
#'   \item{future.map}{Projection for the future period.}
#' }
#' @source The author.
#' @rdname original_sdm
"original_sdm"


#' A thresholded SDM for a marine species
#'
#' This consists of a RasterStack of current and future projections for a SDM generated with BRT (Boosted Regression Trees) for the marine species using only two variables, but thresholded with a random value to produce binary maps.
#'
#' @format A RasterStack with two layers:
#' \describe{
#'   \item{current.map}{Projection for the current period.}
#'   \item{future.map}{Projection for the future period.}
#' }
#' @source The author.
#' @rdname thresholded_sdm
"thresholded_sdm"

#' A map with the difference between a current and future projection for the distribution of a marine species.
#'
#' This consists of a RasterLayer with the difference between current and future binarized projections for a SDM generated with BRT (Boosted Regression Trees) for the marine species.
#'
#' @format A RasterLayer with the map indicating the difference. Values are:
#' \describe{
#'   \item{0}{Unsuitable}
#'   \item{2}{Lost areas}
#'   \item{2}{Gain areas}
#'   \item{3}{Kept areas}
#' }
#' @source The author.
#' @rdname dif_fut_sdm
"dif_fut_sdm"


#' Environmental layers used to produce the marine species SDM
#'
#' This consists of a RasterStack with two marine environmental layers. Those layers were obtained from the Bio-ORACLE dataset using the \link[sdmpredictors]{load_layers} function from the sdmpredictors package. The layers were additionally cropped and masked to get only areas up to the depth of 200m.
#'
#' @format A RasterStack with the environmental layers.
#' \describe{
#'   \item{BO21_tempmax_ss}{Maximum Sea-surface temperature}
#'   \item{BO21_salinitymean_ss}{Mean sea-surface salinity}
#' }
#' @source The author, with data from Bio-ORACLE (\url{https://www.bio-oracle.org/}).
#' @rdname env_data
"env_data"
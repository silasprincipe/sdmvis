% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pa_data.R
\docType{data}
\name{env_data}
\alias{env_data}
\title{Environmental layers used to produce the marine species SDM}
\format{
A RasterStack with the environmental layers.
\describe{
  \item{BO21_tempmax_ss}{Maximum Sea-surface temperature}
  \item{BO21_salinitymean_ss}{Mean sea-surface salinity}
}
}
\source{
The author, with data from Bio-ORACLE (\url{https://www.bio-oracle.org/}).
}
\usage{
env_data
}
\description{
This consists of a RasterStack with two marine environmental layers. Those layers were obtained from the Bio-ORACLE dataset using the \link[sdmpredictors]{load_layers} function from the sdmpredictors package. The layers were additionally cropped and masked to get only areas up to the depth of 200m.
}
\keyword{datasets}

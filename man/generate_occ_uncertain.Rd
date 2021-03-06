% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_occ_uncertain.R
\name{generate_occ_uncertain}
\alias{generate_occ_uncertain}
\title{Generate random latitude and longitude coordinates accounting uncertainty values}
\usage{
generate_occ_uncertain(occs_df, lat_col = "latitude",
  lat_uncertainty = "latitude_uncertainty", lon_col = "longitude",
  lon_uncertainty = "longitude_uncertainty", taxa_col = "species")
}
\arguments{
\item{occs_df}{A \code{data.frame} of occurrence locations that includes
\emph{at least these four columns} - latitude, longitude, latitude uncertainty and longitude uncertainty in degrees.}

\item{lat_col}{Name of column of latitude values. Caps sensitive.}

\item{lat_uncertainty}{Name of column of latitude uncertainty in degree values. Caps sensitive.}

\item{lon_col}{Name of column of longitude values. Caps sensitive.}

\item{lon_uncertainty}{Name of column of longitude uncertainty in degree values. Caps sensitive.}

\item{taxa_col}{Name of column of taxa (species) values. Caps sensitive.}

\item{lon_random}{latitude random deviates of the interval from min to max. Caps sensitive.}

\item{lon_random}{longitude random deviates of the interval from min to max. Caps sensitive.}
}
\description{
\code{generate_occ_uncertain} Generate random latitude and longitude coordinates with uncertain values. The use case this function was developed for was to generate occurence point uncertainty coordinates.
}

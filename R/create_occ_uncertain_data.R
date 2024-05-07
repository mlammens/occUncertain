#' @export create_occ_uncertain_data
#'
#' @title Create Occurrence Data with Uncertainty
#'
#' @description
#' Function to create occurrence data sets with spatial uncertainty incorporated
#' into point positions.
#'
#' @details
#' \strong{Input} as a \code{dataframe} should have the following structure:
#' \tabular{ccccc}{
#' [,1] \tab ddlat \tab numeric, latitude (in decimal degrees)\cr
#' [,2] \tab ddlon \tab numeric, longitude (in decimal degrees)\cr
#' [,3] \tab ddlat unc \tab numeric, longitude uncertainty (in decimal degrees)\cr
#' [,4] \tab ddlon unc \tab numeric, longitude uncertainty (in decimal degrees)\cr
#' [,5] \tab tax \tab character or factor, taxa names\cr}
#' @param occs_df A \code{data.frame} of occurrence locations that includes
#'   \emph{at least these four columns} - latitude, longitude, latitude uncertainty and longitude uncertainty in degrees.
#' @param lat_col Name of column of latitude dbl values. Caps sensitive.
#' @param lon_col Name of column of longitude dbl values. Caps sensitive.
#' @param lat_uncertainty Name of column of latitude uncertainty in degree values. Caps sensitive.
#' @param lon_uncertainty Name of column of longitude uncertainty in degree values. Caps sensitive.
#' @param taxa_col Name of column of taxa (species) values. Caps sensitive.


## TO-DO - Drop the uncertaint inputs and instead add the inputs to meters_to_decdeg, then call that function here

create_occ_uncertain_data <-
  function(occs_df,
           lat_col = "latitude",
           lon_col = "longitude",
           lat_uncertainty = "latitude_uncertainty",
           lon_uncertainty = "longitude_uncertainty",
           taxa_col = "species"
  ){
    lat <- occs_df[[lat_col]]
    lon <- occs_df[[lon_col]]
    lat_uncertainty <- occs_df[[lat_uncertainty]]
    lon_uncertainty <- occs_df[[lon_uncertainty]]

    lat_random <- stats::runif(n = length(lat),
                               min = lat - lat_uncertainty, max = lat +
                                 lat_uncertainty)
    lon_random <- stats::runif(n = length(lon),
                               min = lon - lon_uncertainty, max = lon +
                                 lon_uncertainty)
    random_dd <- data.frame(lat_random = lat_random, lon_random = lon_random, tax = occs_df[[taxa_col]] )
    return(random_dd)
  }

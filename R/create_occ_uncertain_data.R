#' @export create_occ_uncertain_data
#'
#' @title Create Occurrence Data with Uncertainty
#'
#' @description
#' Function to create occurrence data sets with spatial uncertainty incorporated
#' into point positions.
#'
#' @details
#' \strong{Input} as a \code{dataframe} must have the following columns:
#' \tabular{ccccc}{
#' [,1] \tab ddlat \tab numeric, latitude (in decimal degrees)\cr
#' [,2] \tab ddlon \tab numeric, longitude (in decimal degrees)\cr
#' [,3] \tab ddlat unc \tab numeric, longitude uncertainty (in decimal degrees)\cr
#' [,4] \tab ddlon unc \tab numeric, longitude uncertainty (in decimal degrees)\cr
#' [,5] \tab tax \tab character or factor, taxa names\cr}
#'
#' There can be additional columns in the data frame, which will be included
#' in the output data frames, but otherwise ignored.
#'
#' @param occs_df A \code{data.frame} of occurrence locations that includes
#'   \emph{at least these four columns} - latitude, longitude, latitude uncertainty and longitude uncertainty in degrees.
#' @param lat_col Name of column of latitude dbl values. Caps sensitive.
#' @param lon_col Name of column of longitude dbl values. Caps sensitive.
#' @param coord_uncert Name of column of coordinate uncertainty values,
#'    in meters. Case sensitive.
#' @param na_action Sets action to take regarding occurrences with coordinate
#'    uncertainty values of NA. Case sensitive

create_occ_uncertain_data <-
  function(occs_df,
           lat_col = "latitude",
           lon_col = "longitude",
           coord_uncert,
           na_action = "NA as NA"
  ){
    # Create lat/lon uncertainty values in decimal degrees
    occs_uncert <-
      meters_to_decdeg(occs_df = occs_df,
                       lat_col = lat_col, lon_col = lon_col,
                       coord_uncert = coord_uncert,
                       na_action = na_action)

    lat <- occs_df[[lat_col]]
    lon <- occs_df[[lon_col]]
    lat_uncertainty <- occs_uncert[["lat_uncertainty"]]
    lon_uncertainty <- occs_uncert[["lon_uncertainty"]]

    lat_random <- stats::runif(n = length(lat),
                               min = lat - lat_uncertainty, max = lat +
                                 lat_uncertainty)
    lon_random <- stats::runif(n = length(lon),
                               min = lon - lon_uncertainty, max = lon +
                                 lon_uncertainty)
    # Make a data frame that includes the new random points
    random_dd <- data.frame(lat_random = lat_random, lon_random = lon_random,
                            lat_uncertainty = lat_uncertainty,
                            lon_uncertainty = lon_uncertainty)

    # Remove the original lat/lon columns from the occs_df and add the new
    # columns made
    occs_df_temp <- occs_df
    occs_df_temp[[lat_col]] <- NULL
    occs_df_temp[[lon_col]] <- NULL
    occs_df_temp[[coord_uncert]] <- NULL

    random_dd <- cbind(occs_df_temp, random_dd)

    return(random_dd)
  }

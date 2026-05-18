#' get soil data from the European Soil DataBase (ESDB), using a local repository for GIS files.
#'
#' @param lat latitude of the geographical location in decimal degrees (WGS84)
#' @param lon longitude of the geographical location in decimal degrees (WGS84)
#' @param format output format:
#' * esdb : raw ESDB information
#' * sunflo : rsunflo soil parameter list (assume a fixed wilting point)
#' @param data dataframe created from ESDB GIS files as made available by the JRC lab for research usage.
#'
#' @return a dataframe of soil properties
#' @export

get_soil_esdb <- function(lat, lon, format = "raw", data=data_soil) {

  # get closer grid position
  index <- which.min(sqrt((data$lat-lat)^2 + (data$lon-lon)^2))


  switch(

    format,

    # extract ESDB features
    raw = {
      data_soil <- data %>% dplyr::slice(index) %>% dplyr::select(-(x:lat))
    },

    # build rsunflo soil parameter list (assume a fixed wilting point)
    sunflo = {

      data_soil <- data %>%
        dplyr::slice(index) %>%
        dplyr::mutate(
          awc = awc_ptf,
          wilting_point_1 = 10,
          field_capacity_1 = (awc_ptf_t / (density_t * 300) + wilting_point_1 / 100) * 100,
          wilting_point_2 = 10,
          field_capacity_2 = ifelse(
            depth == 300, 10,
            (awc_ptf_s / (density_s * (depth - 300)) + wilting_point_2 / 100) * 100),
          stone_content = mean(gravel_t, gravel_s) / 100
        ) %>%
        dplyr::select(
          awc,
          root_depth = depth,
          field_capacity_1,
          wilting_point_1,
          field_capacity_2,
          wilting_point_2,
          soil_density_1 = density_t,
          soil_density_2 = density_s,
          stone_content
        )
    },

    stop("Invalid `format` value")

  )

  return(data_soil)

}

#' Get soil data from the European Soil Data Centre (ESDAC) parquet store.
#'
#' Returns ESDB soil properties for the 1 km grid cell nearest the input
#' coordinates, querying a parquet dataset partitioned by COUNTRY (ISO2).
#'
#' @param lat,lon Numeric. WGS84 coordinates of the target location.
#' @param country Character ISO2 (e.g. "FR"). If `NULL`, all partitions
#'   are scanned (slower but works for borders / unknown locations).
#' @param margin Numeric. Half-width of the bounding-box prefilter, in
#'   degrees. Increase for coastal / sparse locations.
#' @param path Character. Path to the parquet store root (directory
#'   containing one subfolder per COUNTRY). When empty, the function
#'   errors with guidance.
#'
#' @return A one-row tibble with the following ESDB columns.
#'   PTF (Pedotransfer Function): AWC estimated by regression on soil texture
#'   properties at the STU (Soil Typological Unit) level.
#'   PTR (Pedotransfer Rule): AWC assigned by categorical lookup table at the
#'   coarser SMU (Soil Mapping Unit) level. PTF is preferred for crop modelling.
#'   \describe{
#'     \item{lat, lon}{grid cell centroid coordinates (WGS84, degrees)}
#'     \item{depth}{depth available to roots (mm)}
#'     \item{awc_ptf}{total available water capacity from PTF, full profile (mm)}
#'     \item{awc_ptf_t}{available water capacity from PTF, topsoil (mm)}
#'     \item{awc_ptf_s}{available water capacity from PTF, subsoil (mm)}
#'     \item{awc_ptr}{total available water capacity from PTR, full profile (mm)}
#'     \item{awc_ptr_t}{available water capacity from PTR, topsoil (mm)}
#'     \item{awc_ptr_s}{available water capacity from PTR, subsoil (mm)}
#'     \item{gravel_t, gravel_s}{coarse fragments, topsoil / subsoil (\%)}
#'     \item{clay_t, clay_s}{clay content, topsoil / subsoil (\%)}
#'     \item{silt_t, silt_s}{silt content, topsoil / subsoil (\%)}
#'     \item{sand_t, sand_s}{sand content, topsoil / subsoil (\%)}
#'     \item{carbon_t, carbon_s}{organic carbon content, topsoil / subsoil (\%)}
#'     \item{density_t, density_s}{bulk density, topsoil / subsoil (g cm-3)}
#'   }
#' @export
get_soil_esdac <- function(lat, lon,
                           country = NULL,
                           margin  = 0.1,
                           path    = "") {

  if (!nzchar(path))
    stop("`path` is empty. Set it to the ESDAC parquet store root, e.g. ",
         "'~/Documents/INRAE/Database/data_soil/data/europe/parquet'.")

  ds <- arrow::open_dataset(
    path, partitioning = arrow::schema(COUNTRY = arrow::utf8())
  )

  q <- ds |>
    dplyr::filter(
      lat >= .env$lat - margin, lat <= .env$lat + margin,
      lon >= .env$lon - margin, lon <= .env$lon + margin
    )
  if (!is.null(country)) q <- q |> dplyr::filter(COUNTRY == country)

  data <- dplyr::collect(q)
  if (nrow(data) == 0)
    stop("No grid cell within margin=", margin,
         " of (", lat, ", ", lon, "). Try increasing `margin`.")

  data |>
    dplyr::mutate(.dist = (lat - .env$lat)^2 + (lon - .env$lon)^2) |>
    dplyr::slice_min(.dist, n = 1, with_ties = FALSE) |>
    dplyr::select(-.dist, -dplyr::any_of(c("x", "y", "COUNTRY")))
}

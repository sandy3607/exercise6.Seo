#' Plotting all the defibrillators in the city
#'
#' This function takes one character value as input and returns a tmap-plot.
#'
#' @param cityname a character value - name of the city.
#' @return a tmap plot.
#' @examples
#' \dontrun{
#' plotDefi("München")
#' # Output: plot of defibrillators in Munich
#' plotDefi("Berlin")
#' # Output: plot of defibrillators in Berlin
#' }
#' @importFrom magrittr %>%
plotDefi <- function(cityname) {

  # get bb
  bb_defi <- osmdata::getbb(cityname, featuretype = "city")

  # query for defi
  q_defi <- osmdata::opq(bb_defi) %>%
    osmdata::add_osm_feature(key = "emergency", value = "defibrillator")
  tmap::tmap_mode("view")

  result_defi <- q_defi %>%
    osmdata::osmdata_sf()
  list_defi <- result_defi$osm_points
  # head(list_defi, n=3)

  t<- tmap::tm_shape(list_defi) +
    tmap::tm_dots(id = "geometry")
  print(t)
}

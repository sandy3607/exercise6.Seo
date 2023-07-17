#' Return the coordinates and the distance to the next defibrillator
#'
#' This function takes two numeric values and a character value as input and returns a text.
#' @param cityname a character value - name of the city.
#' @param x A numeric value - x coordinate of the current position.
#' @param y A numeric value - y coordinate of the current position.
#' @return A sentence - coordinates and the distance to the next difibrillator.
#' @examples
#' \dontrun{
#' closestDefi("München",11.50,48.10)
#' # Output: The distance to the next defibrillator is located in 529 meters.The coordinates are: (11.4976269,48.0957883).
#' closestDefi("München",11.60,48.20)
#' # Output: The distance to the next defibrillator is located in 1384 meters.The coordinates are: (11.609247,48.1915003).
#' closestDefi("München",11.70,48.07)
#' # The distance to the next defibrillator is located in 2026 meters.The coordinates are: (11.6825927,48.0645139).
#' }
#' @importFrom magrittr %>%
closestDefi <- function(cityname,x,y){
  #x: x coordinate, y: y coordinate aktueller Standort
  #a: x coordinate from the next defi, b: y coordinate from the next defi
  bestdist = 10000 #initializing best distance
  besta = 0 #initializing x coordinate of the best point
  bestb = 0 #initializing y coordinate of the best point

  # get bb
  bb_defi <- osmdata::getbb(cityname, featuretype = "city")

  # query for defi
  q_defi <- osmdata::opq(bb_defi) %>%
    osmdata::add_osm_feature(key = "emergency", value = "defibrillator")
  tmap::tmap_mode("view")

  result_defi <- q_defi %>%
    osmdata::osmdata_sf()

  list_defi <- result_defi$osm_points



  # this distance-code below is based on haversine calculation.
  # so we can calculate the distance between two points on the Earth surface accurately
  # (more accurate than using the pythagorian theorem - because the Earth is round)
  # This code is written by chatGPT completely.
  distance <- function(lat1, lon1, lat2, lon2) {
    rad <- pi / 180  # Conversion factor from degrees to radians
    earth_radius_km <- 6371  # Earth's radius in kilometers

    dlat <- (lat2 - lat1) * rad
    dlon <- (lon2 - lon1) * rad

    a <- sin(dlat / 2) * sin(dlat / 2) +
      cos(lat1 * rad) * cos(lat2 * rad) *
      sin(dlon / 2) * sin(dlon / 2)

    c <- 2 * atan2(sqrt(a), sqrt(1 - a))

    distance_km <- earth_radius_km * c
    distance_meters <- distance_km * 1000

    return(distance_meters)
  }

  for (i in 1:nrow(list_defi)){
    getcoor <- function(i){
      points_coords <- sf::st_coordinates(list_defi)
      df <- data.frame(a <- points_coords[i,1],
                       b <- points_coords[i,2])
      return(df)
    }
    c<-getcoor(i)
    a = c[1,1]
    b = c[1,2]
    currentdist = distance(x,y,a,b) # haversine
    if (currentdist<bestdist){
      bestdist = currentdist
      besta = a
      bestb = b
    }
  }
  bestdist = round(bestdist, digits = 0)
  outputtext = paste0("The distance to the next defibrillator is located in ", bestdist, " meters.",
                      "The coordinates are: (", besta, ",", bestb,").")
  return (outputtext)
}




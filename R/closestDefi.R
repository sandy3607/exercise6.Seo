closestDefi <- function(x,y){
  #x: x coordinate, y: y coordinate aktueller Standort
  #a: x coordinate from the next defi, b: y coordinate from the next defi
  bestdist = 10000 #initializing best distance
  besta = 0 #initializing x coordinate of the best point
  bestb = 0 #initializing y coordinate of the best point

  for (i in 1:nrow(list_defi)){
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

getcoor <- function(i){
  points_coords <- sf::st_coordinates(list_defi)
  df <- data.frame(a <- points_coords[i,1],
                   b <- points_coords[i,2])
  return(df)
}

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

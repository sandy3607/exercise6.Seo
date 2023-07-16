

plotDefi <- function(cityname) {

  # get bb
  bb_defi <- getbb(cityname, featuretype = "city")

  # query for defi
  q_defi <- opq(bb_defi) %>%
    add_osm_feature(key = "emergency", value = "defibrillator")
  tmap_mode("view")

  result_defi <- q_defi %>%
    osmdata_sf()
  list_defi <- result_defi$osm_points
  # head(list_defi, n=3)

  tm_shape(list_defi) +
    tm_dots(id = "geometry")
}

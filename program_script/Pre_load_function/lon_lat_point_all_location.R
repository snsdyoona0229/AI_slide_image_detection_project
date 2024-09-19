lon_lat_point_all_location <- function(tile_x,
                                       tile_y,
                                       zoom,
                                       tile_size) {
  check_point <- which(array(1, c(tile_size, tile_size)) == 1, arr.ind = TRUE)
  deviation_x <- tile_x * 2^(12 - zoom)
  deviation_y <- tile_y * 2^(12 - zoom)
  
  check_point[,1] <- check_point[,1] + deviation_x
  check_point[,2] <- check_point[,2] + deviation_y
  
  lonlat_location_all <- XY2LonLat(x = check_point[,1], y = check_point[,2], zoom = zoom + 1)
  return(lonlat_location_all)
}
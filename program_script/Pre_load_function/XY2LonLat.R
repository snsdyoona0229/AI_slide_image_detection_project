XY2LonLat <- function(x, y, zoom = 12){
  n <- 2^(zoom-1)
  lon_deg <- (x/256) / n * 360.0 - 180.0
  tmp <- tanh( pi * (1 - 2 * (y/256) / n))
  lat_deg <- asin(tmp) * 180/pi
  data.frame(lon = lon_deg, lat = lat_deg)
}


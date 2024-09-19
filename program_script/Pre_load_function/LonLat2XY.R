LonLat2XY <- function(lon_deg, lat_deg, zoom = 12){
  lon_deg <- lon_deg %>% as.numeric()
  lat_deg <- lat_deg %>% as.numeric()
  n <- 2^(zoom-1)
  X <- ((lon_deg + 180) / 360) * n
  sec <- function(x) 1/cos(x)
  lat_rad <- lat_deg * pi/180
  Y <- (1 - (log(tan(lat_rad) + sec(lat_rad)) / pi)) / 2 * n
  df <- data.frame(
    x = round(256 * X),
    y = round(256 * Y)
  )
  row.names(df) <- NULL
  df
}


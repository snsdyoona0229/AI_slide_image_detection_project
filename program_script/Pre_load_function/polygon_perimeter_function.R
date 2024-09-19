polygon_perimeter_function <- function(x_location, y_location) {
  total_number <- length(x_location)
  x_square <- (x_location[1:(total_number - 1)] - x_location[2:total_number])^2
  y_square <- (y_location[1:(total_number - 1)] - y_location[2:total_number])^2
  all_length <- ((x_square + y_square)^0.5) %>% sum()
  return(all_length)
}

polygon_area_function <- function(x_location, y_location) {
  unit_length <- length(x_location)
  area <- abs(sum(y_location[1:(unit_length - 1)] * x_location[2:unit_length] - x_location[1:(unit_length - 1)] * y_location[2:unit_length])/2)
  return(area)
}
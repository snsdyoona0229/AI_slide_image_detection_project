point_line_function <- function(point1, point2) {
  x_dim <- point2[1] - point1[1]
  y_dim <- point2[2] - point1[2]
  long_dim <- max(abs(x_dim), abs(y_dim))
  if (long_dim == 0) {
    return(data.frame(x = c(point1[1], point2[1]), y = c(point1[2], point2[2])))
  } else {
    if (long_dim == abs(x_dim)) {
      x_location <- point1[1]:point2[1]
      y_location <- round(point1[2] + y_dim/abs(x_dim)*(0:(length(x_location) - 1)))
    } else {
      y_location <- point1[2]:point2[2]
      x_location <- round(point1[1] + x_dim/abs(y_dim)*(0:(length(y_location) - 1)))
    }
    return(data.frame(x = x_location, y = y_location))
  }
}
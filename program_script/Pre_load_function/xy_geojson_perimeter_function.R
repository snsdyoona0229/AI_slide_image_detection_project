xy_geojson_perimeter_function <- function(xy_geojson) {
  perimeter_length <- 0
  if (xy_geojson$geometry$type == "Polygon") {
    if (xy_geojson$geometry$coordinates %>% class() == "list") {
      for (p_list in 1:length(xy_geojson$geometry$coordinates)) {
        origin_location <- xy_geojson$geometry$coordinates[[p_list]]
        perimeter_length <- perimeter_length + polygon_perimeter_function(x_location = origin_location[,1],
                                                                          y_location = origin_location[,2])
      }
      return(perimeter_length)
    } else if (xy_geojson$geometry$coordinates %>% class() == "array") {
      for (p_list in 1:(dim(xy_geojson$geometry$coordinates)[1])) {
        origin_location <- xy_geojson$geometry$coordinates[p_list,,]
        perimeter_length <- perimeter_length + polygon_perimeter_function(x_location = origin_location[,1],
                                                                          y_location = origin_location[,2])
      }
      return(perimeter_length)
    }
  } else if (xy_geojson$geometry$type == "MultiPolygon") {
    for (m_list in 1:length(xy_geojson$geometry$coordinates)) {
      if (xy_geojson$geometry$coordinates[[m_list]] %>% class() == "list") {
        for (p_list in 1:length(xy_geojson$geometry$coordinates[[m_list]])) {
          origin_location <- xy_geojson$geometry$coordinates[[m_list]][[p_list]]
          perimeter_length <- perimeter_length + polygon_perimeter_function(x_location = origin_location[,1],
                                                                            y_location = origin_location[,2])
        }
        return(perimeter_length)
      } else if (xy_geojson$geometry$coordinates[[m_list]] %>% class() == "array") {
        for (p_list in 1:(dim(xy_geojson$geometry$coordinates[[m_list]])[1])) {
          origin_location <- xy_geojson$geometry$coordinates[[m_list]][p_list,,]
          perimeter_length <- perimeter_length + polygon_perimeter_function(x_location = origin_location[,1],
                                                                            y_location = origin_location[,2])
        }
        return(perimeter_length)
      }
    } 
  }
}
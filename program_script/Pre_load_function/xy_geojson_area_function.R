xy_geojson_area_function <- function(xy_geojson) {
  plus_area <- 0
  minus_area <- 0
  if (xy_geojson$geometry$type == "Polygon") {
    if (xy_geojson$geometry$coordinates %>% class() == "list") {
      for (p_list in 1:length(xy_geojson$geometry$coordinates)) {
        origin_location <- xy_geojson$geometry$coordinates[[p_list]]
        if (p_list == 1) {
          plus_area <- plus_area + polygon_area_function(x_location = origin_location[,1],
                                                         y_location = origin_location[,2])
        } else {
          minus_area <- minus_area + polygon_area_function(x_location = origin_location[,1],
                                                           y_location = origin_location[,2])
        }
      }
      return(plus_area - minus_area)
    } else if (xy_geojson$geometry$coordinates %>% class() == "array") {
      for (p_list in 1:(dim(xy_geojson$geometry$coordinates)[1])) {
        origin_location <- xy_geojson$geometry$coordinates[p_list,,]
        if (p_list == 1) {
          plus_area <- plus_area + polygon_area_function(x_location = origin_location[,1],
                                                         y_location = origin_location[,2])
        } else {
          minus_area <- minus_area + polygon_area_function(x_location = origin_location[,1],
                                                           y_location = origin_location[,2])
        }
      }
      return(plus_area - minus_area)
    }
  } else if (xy_geojson$geometry$type == "MultiPolygon") {
    for (m_list in 1:length(xy_geojson$geometry$coordinates)) {
      if (xy_geojson$geometry$coordinates[[m_list]] %>% class() == "list") {
        for (p_list in 1:length(xy_geojson$geometry$coordinates[[m_list]])) {
          origin_location <- xy_geojson$geometry$coordinates[[m_list]][[p_list]]
          if (p_list == 1) {
            plus_area <- plus_area + polygon_area_function(x_location = origin_location[,1],
                                                           y_location = origin_location[,2])
          } else {
            minus_area <- minus_area + polygon_area_function(x_location = origin_location[,1],
                                                             y_location = origin_location[,2])
          }
        }
        return(plus_area - minus_area)
      } else if (xy_geojson$geometry$coordinates[[m_list]] %>% class() == "array") {
        for (p_list in 1:(dim(xy_geojson$geometry$coordinates[[m_list]])[1])) {
          origin_location <- xy_geojson$geometry$coordinates[[m_list]][p_list,,]
          if (p_list == 1) {
            plus_area <- plus_area + polygon_area_function(x_location = origin_location[,1],
                                                           y_location = origin_location[,2])
          } else {
            minus_area <- minus_area + polygon_area_function(x_location = origin_location[,1],
                                                             y_location = origin_location[,2])
          }
        }
        return(plus_area - minus_area)
      }
    } 
  }
}
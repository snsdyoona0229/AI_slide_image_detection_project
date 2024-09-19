xy_geojson_point_in_function <- function(xy_geojson, xy_point) {
  plus_point <- c(0,0,0,0,0,0,0,0,0,0)
  minus_point <- c(0,0,0,0,0,0,0,0,0,0)
  if (xy_geojson$geometry$type == "Polygon") {
    if (xy_geojson$geometry$coordinates %>% class() == "list") {
      for (p_list in 1:length(xy_geojson$geometry$coordinates)) {
        origin_location <- xy_geojson$geometry$coordinates[[p_list]]
        if (p_list == 1) {
          plus_point <- plus_point + polygon_point_in_function(x_location = origin_location[,1],
                                                               y_location = origin_location[,2],
                                                               point_df = xy_point)
        } else {
          minus_point <- minus_point + polygon_point_in_function(x_location = origin_location[,1],
                                                                 y_location = origin_location[,2],
                                                                 point_df = xy_point)
        }
      }
      return(plus_point - minus_point)
    } else if (xy_geojson$geometry$coordinates %>% class() == "array") {
      for (p_list in 1:(dim(xy_geojson$geometry$coordinates)[1])) {
        origin_location <- xy_geojson$geometry$coordinates[p_list,,]
        if (p_list == 1) {
          plus_point <- plus_point + polygon_point_in_function(x_location = origin_location[,1],
                                                               y_location = origin_location[,2],
                                                               point_df = xy_point)
        } else {
          minus_point <- minus_point + polygon_point_in_function(x_location = origin_location[,1],
                                                                 y_location = origin_location[,2],
                                                                 point_df = xy_point)
        }
      }
      return(plus_point - minus_point)
    }
  } else if (xy_geojson$geometry$type == "MultiPolygon") {
    for (m_list in 1:length(xy_geojson$geometry$coordinates)) {
      if (xy_geojson$geometry$coordinates[[m_list]] %>% class() == "list") {
        for (p_list in 1:length(xy_geojson$geometry$coordinates[[m_list]])) {
          origin_location <- xy_geojson$geometry$coordinates[[m_list]][[p_list]]
          if (p_list == 1) {
            plus_point <- plus_point + polygon_point_in_function(x_location = origin_location[,1],
                                                                 y_location = origin_location[,2],
                                                                 point_df = xy_point)
          } else {
            minus_point <- minus_point + polygon_point_in_function(x_location = origin_location[,1],
                                                                   y_location = origin_location[,2],
                                                                   point_df = xy_point)
          }
        }
        return(plus_point - minus_point)
      } else if (xy_geojson$geometry$coordinates[[m_list]] %>% class() == "array") {
        for (p_list in 1:(dim(xy_geojson$geometry$coordinates[[m_list]])[1])) {
          origin_location <- xy_geojson$geometry$coordinates[[m_list]][p_list,,]
          if (p_list == 1) {
            plus_point <- plus_point + polygon_point_in_function(x_location = origin_location[,1],
                                                                 y_location = origin_location[,2],
                                                                 point_df = xy_point)
          } else {
            minus_point <- minus_point + polygon_point_in_function(x_location = origin_location[,1],
                                                                   y_location = origin_location[,2],
                                                                   point_df = xy_point)
          }
        }
        return(plus_point - minus_point)
      }
    } 
  }
}

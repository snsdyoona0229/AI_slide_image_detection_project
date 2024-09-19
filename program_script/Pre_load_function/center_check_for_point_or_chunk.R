center_check_for_point_or_chunk <- function(center_x_all, center_y_all, zoom_level, tile_size, point_vs_chunk) {

  if (point_vs_chunk == "point") {
    all_point_range <- (1:tile_size - tile_size/2) * 2^(12 - zoom_level)
    all_check_point <- lapply(1:length(center_x_all), function(check_point_each) {
      point_location <- XY2LonLat(x = center_x_all[check_point_each] + rep(all_point_range, tile_size),
                                  y = center_y_all[check_point_each] + sapply(all_point_range, function(y) {rep(y, tile_size)}) %>% as.numeric(),
                                  zoom = 12 + 1) %>% as.matrix()
      lapply(1:nrow(point_location), function(x) {
        lawn_point(point_location[x,])
      }) %>% lawn_featurecollection()
    })
  } else if (point_vs_chunk == "chunk") {
    all_point_range <- (base::c(tile_size, 1) - tile_size/2) * 2^(12 - zoom_level)
    all_check_polygon <- lapply(1:length(center_x_all), function(check_polygon_each) {
      poly_location <- XY2LonLat(x = base::c(center_x_all[check_polygon_each] + all_point_range[1],
                                             center_x_all[check_polygon_each] + all_point_range[2],
                                             center_x_all[check_polygon_each] + all_point_range[2],
                                             center_x_all[check_polygon_each] + all_point_range[1],
                                             center_x_all[check_polygon_each] + all_point_range[1]),
                                 y = base::c(center_y_all[check_polygon_each] + all_point_range[1],
                                             center_y_all[check_polygon_each] + all_point_range[1],
                                             center_y_all[check_polygon_each] + all_point_range[2],
                                             center_y_all[check_polygon_each] + all_point_range[2],
                                             center_y_all[check_polygon_each] + all_point_range[1]),
                                 zoom = 12 + 1)
      
      lawn_polygon(list(list(
        base::c(poly_location[1,1], poly_location[1,2]),
        base::c(poly_location[2,1], poly_location[2,2]),
        base::c(poly_location[3,1], poly_location[3,2]),
        base::c(poly_location[4,1], poly_location[4,2]),
        base::c(poly_location[5,1], poly_location[5,2])
      )))
    })
  }
  
  
}


#lapply(1:length(center_x_all), function(check_point_each) {
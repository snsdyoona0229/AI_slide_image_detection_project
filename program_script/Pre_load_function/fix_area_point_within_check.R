fix_area_point_within_check <- function(points_xy, 
                                        length_size, 
                                        pair_down_id,
                                        point1, 
                                        point2,
                                        area_type){
  if (area_type == "square") {
    if (abs(point1[1] - point2[1]) <= length_size & abs(point1[2] - point2[2]) <= length_size) {
      edge1 <- c(point1[1], point2[2])
      edge2 <- c(point2[1], point1[2])
      
      center1 <- c()
      
      if (edge1[1] < point2[1]) {
        center1[1] <- edge1[1] + length_size/2
      } else {
        center1[1] <- edge1[1] - length_size/2
      }
      
      if (edge1[2] < point1[2]) {
        center1[2] <- edge1[2] + length_size/2
      } else {
        center1[2] <- edge1[2] - length_size/2
      }
      
      center2 <- c()
      
      if (edge2[1] < point1[1]) {
        center2[1] <- edge2[1] + length_size/2
      } else {
        center2[1] <- edge2[1] - length_size/2
      }
      
      if (edge2[2] < point2[2]) {
        center2[2] <- edge2[2] + length_size/2
      } else {
        center2[2] <- edge2[2] - length_size/2
      }
      
      point_within1 <- point_xy %>% 
        filter(x >= (center1[1] - length_size/2)) %>% 
        filter(x <= (center1[1] + length_size/2)) %>% 
        filter(y >= (center1[2] - length_size/2)) %>% 
        filter(y <= (center1[2] + length_size/2)) 
      
      point_within2 <- point_xy %>% 
        filter(x >= (center2[1] - length_size/2)) %>% 
        filter(x <= (center2[1] + length_size/2)) %>% 
        filter(y >= (center2[2] - length_size/2)) %>% 
        filter(y <= (center2[2] + length_size/2))
      
      if (nrow(point_within1) >= nrow(point_within2)) {
        return(
          data.frame(
            pair_down_id = pair_down_id,
            pair_center_x = center1[1],
            pair_center_y = center1[2],
            max_number = nrow(point_within1)
          )
        )
      } else {
        return(
          data.frame(
            pair_down_id = pair_down_id,
            pair_center_x = center2[1],
            pair_center_y = center2[2],
            max_number = nrow(point_within2)
          )
        )
      }
    } else {
      return(
        data.frame(
          pair_down_id = pair_down_id,
          pair_center_x = NA,
          pair_center_y = NA,
          max_number = 0
        )
      )
    }
  }
}

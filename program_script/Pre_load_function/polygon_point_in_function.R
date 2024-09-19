polygon_point_in_function <- function(x_location, y_location, point_df) {
  in_point <- pointinpolygon(A = list(x = x_location, y = y_location), P = list(x = point_df$point_lon,y = point_df$point_lat))
  point_return <- point_df[in_point == 1,]$point_group %>% str_replace("P_", "") %>% as.integer %>% factor(levels = 1:10) %>% summary() %>% unname()
  return(point_return)
}

geojson_to_done_check_list_function <- function(geojson, 
                                                x_chunk, y_chunk, 
                                                overlap_pixel, 
                                                batch_width_size, 
                                                deviation_x, deviation_y, 
                                                basal_layer,
                                                update_polygon_class) {
  
  setDTthreads(thread= parallel::detectCores())	
  
  check_left_up_pixel <-  XY2LonLat(2 * overlap_pixel + deviation_x, 
                                    2 * overlap_pixel + deviation_y,
                                    zoom = basal_layer + 1)
  #print(check_left_up_pixel)
  check_right_down_pixel <- XY2LonLat(batch_width_size + deviation_x - 2 * overlap_pixel, 
                                      batch_width_size + deviation_y - 2 * overlap_pixel,
                                      zoom = basal_layer + 1)
  #print(check_left_up_pixel)
  
  geojson_bbox <- geojson %>% lapply(lawn_bbox) 
  
  observe_df <- data.table(
    file = NA,
    name = names(geojson),
    x_chunk = x_chunk,
    y_chunk = y_chunk,
    id = names(geojson),
    left = geojson_bbox %>% sapply(function(x){min(x[c(1,3)])}),
    right = geojson_bbox %>% sapply(function(x){max(x[c(1,3)])}),
    up = geojson_bbox %>% sapply(function(x){max(x[c(2,4)])}),
    down = geojson_bbox %>% sapply(function(x){min(x[c(2,4)])}),
    check_left = NA,
    check_right = NA,
    check_up = NA,
    check_down = NA,
    original_chunk = NA,
    stringsAsFactors = FALSE
  )


  
  observe_df$check_left <- observe_df$left < check_left_up_pixel$lon
  observe_df$check_right <- observe_df$right > check_right_down_pixel$lon
  observe_df$check_up <- observe_df$up > check_left_up_pixel$lat
  observe_df$check_down <- observe_df$down < check_right_down_pixel$lat
  observe_df$file <- paste0(update_polygon_class, "_", deviation_x, "_", deviation_y, ".rds")
  
  observe_df$check_left <- sapply(1:nrow(observe_df), function(x){
    if (as.logical(observe_df$check_left[x])) {
      paste0("_",
             observe_df$x_chunk[x] %>% as.integer() - 1,
             "_",
             observe_df$y_chunk[x],
             "_")
    } else {
      NA
    }
  })
  
  observe_df$check_right <- sapply(1:nrow(observe_df), function(x){
    if (as.logical(observe_df$check_right[x])) {
      paste0("_",
             observe_df$x_chunk[x] %>% as.integer() + 1,
             "_",
             observe_df$y_chunk[x],
             "_")
    } else {
      NA
    }
  })
  
  observe_df$check_up <- sapply(1:nrow(observe_df), function(x){
    if (as.logical(observe_df$check_up[x])) {
      paste0("_",
             observe_df$x_chunk[x],
             "_",
             observe_df$y_chunk[x] %>% as.integer() - 1,
             "_")
    } else {
      NA
    }
  })
  
  observe_df$check_down <- sapply(1:nrow(observe_df), function(x){
    if (as.logical(observe_df$check_down[x])) {
      paste0("_",
             observe_df$x_chunk[x],
             "_",
             observe_df$y_chunk[x] %>% as.integer() + 1,
             "_")
    } else {
      NA
    }
  })
  
  observe_df$original_chunk <-paste0(
    "_",
    observe_df$x_chunk,
    "_",
    observe_df$y_chunk,
    "_"
  )
  
  need_check_poly <- observe_df %>% filter((is.na(check_left) & is.na(check_right) & is.na(check_up) & is.na(check_down)) == FALSE)
  no_need_check_poly <- observe_df %>% filter(is.na(check_left) & is.na(check_right) & is.na(check_up) & is.na(check_down))
  
  #pred_result <- unlist(pred_result, recursive = FALSE) %>% geojson_merge_function(method = "exact")
  
  export_list <- list("Check" = list(geojson[need_check_poly$name], need_check_poly),
                      "Done" = list(geojson[no_need_check_poly$name], no_need_check_poly))
  
  return(export_list)
}
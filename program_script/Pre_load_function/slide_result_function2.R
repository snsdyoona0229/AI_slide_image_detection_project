slide_result_function2 <- function(batch_result_location, 
                                   deviation_x, 
                                   deviation_y, 
                                   basal_layer, 
                                   overlap_pixel,
                                   polygon_threshold, 
                                   point_threshold,
                                   polygon_simplify,
                                   modified_label_table,
                                   temp_AI,
                                   check_file_resolve) {
  
  batch_result <- readRDS(batch_result_location) %>% abind::abind(along = 1)
  
  batch_size <- dim(batch_result)[1]
  
  future_lapply(1:batch_size, function(batch_number) {
    for (i in 1:nrow(modified_label_table)) {
      #i <- 1
      #batch_number <- 9
      if (modified_label_table$use_in_label[i] == "Y") {
        if (modified_label_table$layer_option[i] == "G") {
          
          pred_result <- batch_result[batch_number,,,i] %>% 
            replace(which(batch_result[batch_number,,,i] <= polygon_threshold), 0) %>% 
            replace(which(batch_result[batch_number,,,i] > polygon_threshold), 1) %>% 
            fillHull() %>% bwlabel()
          
          if (max(pred_result) > 0) {
            pred_result <- lapply(ocontour(pred_result), function(x){
              XY2LonLat(x[,1] + deviation_x[batch_number], 
                        x[,2] + deviation_y[batch_number],
                        zoom = basal_layer + 1)
            })
            
            blank_geo <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
            
            pred_result <- lapply(pred_result, function(x){
              lapply(1:nrow(x), function(y){
                list(x[y,1], x[y,2])
              })
            })
            
            update_polygon_class <- paste0(modified_label_table$Primary_layer_ID[i],
                                           "_",
                                           modified_label_table$Secondary_layer_ID[i],
                                           "_",
                                           modified_label_table$Tertiary_layer_ID[i],
                                           "_",
                                           modified_label_table$layer_option[i])
            
            new_polygon_id <- sample(1:100000, length(pred_result))
            
            pred_result <- lapply(1:length(pred_result), function(x){
              id_num <- new_polygon_id[x]
              data_in <- blank_geo
              data_in$geometry$coordinates <- list(pred_result[[x]])
              data_in$properties$layerId <- as.character(id_num)
              data_in$properties$edit_id <- as.character(id_num)
              data_in$properties$`_leaflet_id` <- id_num
              data_in
            })
            
            names(pred_result) <- new_polygon_id %>% as.integer()
            
            pred_result <- lapply(pred_result, function(x){
              if (lawn_area(x) >= 1) {
                x$geometry$coordinates[[1]][[x$geometry$coordinates[[1]] %>% length() + 1]] <- x$geometry$coordinates[[1]][[1]]
                lawn_simplify(x, tolerance = polygon_simplify)
              } else {
                NULL
              }
            })
            
            null_case <- sapply(pred_result, length)
            null_case <- which(null_case == 0)
            
            if (length(null_case) != 0) {
              pred_result <- pred_result[-null_case]
            }
            
            if (length(pred_result) > 0) {
              pred_result <- polygon_correction_function(pred_result)
              
              
              for (ii in 1:length(pred_result)) {
                pred_result[[ii]] <- lawn_union(pred_result[[ii]], pred_result[[ii]]) %>% geojson_split_function()
              }
              pred_result <- pred_result %>% unlist(recursive = FALSE)
              
              
              batch_width_size <- dim(batch_result)[2]
              
              check_left_up_pixel <-  XY2LonLat(2 * overlap_pixel + deviation_x[batch_number], 
                                                2 * overlap_pixel + deviation_y[batch_number],
                                                zoom = basal_layer + 1)
              check_right_down_pixel <- XY2LonLat(batch_width_size + deviation_x[batch_number] - 2 * overlap_pixel, 
                                                  batch_width_size + deviation_y[batch_number] - 2 * overlap_pixel,
                                                  zoom = basal_layer + 1)
              
              pred_result_bbox <- pred_result %>% lapply(lawn_bbox) 
              
              observe_df <- data.frame(
                file = NA,
                name = names(pred_result),
                x_chunk = (deviation_x[batch_number] + overlap_pixel)/(batch_width_size - 2 * overlap_pixel),
                y_chunk = (deviation_y[batch_number] + overlap_pixel)/(batch_width_size - 2 * overlap_pixel),
                id = NA,
                left = pred_result_bbox %>% sapply(function(x){min(x[c(1,3)])}),
                right = pred_result_bbox %>% sapply(function(x){max(x[c(1,3)])}),
                up = pred_result_bbox %>% sapply(function(x){max(x[c(2,4)])}),
                down = pred_result_bbox %>% sapply(function(x){min(x[c(2,4)])}),
                check_left = NA,
                check_right = NA,
                check_up = NA,
                check_down = NA,
                original_chunk = NA,
                stringsAsFactors = FALSE
              )
              
              observe_df$id <- paste0("_",
                                      observe_df$x_chunk,
                                      "_",
                                      observe_df$y_chunk,
                                      "_",
                                      observe_df$name)
              
              observe_df$check_left <- observe_df$left <= check_left_up_pixel$lon
              observe_df$check_right <- observe_df$right >= check_right_down_pixel$lon
              observe_df$check_up <- observe_df$up >= check_left_up_pixel$lat
              observe_df$check_down <- observe_df$down <= check_right_down_pixel$lat
              observe_df$file <- paste0(update_polygon_class, "_", deviation_x[batch_number], "_", deviation_y[batch_number], ".rds")
              
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
              
              observe_df$original_chunk <- paste0(
                "_",
                observe_df$x_chunk,
                "_",
                observe_df$y_chunk,
                "_"
              )
              
              need_check_poly <- observe_df %>% filter((is.na(check_left) & is.na(check_right) & is.na(check_up) & is.na(check_down)) == FALSE)
              no_need_check_poly <- observe_df %>% filter(is.na(check_left) & is.na(check_right) & is.na(check_up) & is.na(check_down))
              
              #pred_result <- unlist(pred_result, recursive = FALSE) %>% geojson_merge_function(method = "exact")
              
              export_list <- list("Check" = list(pred_result[need_check_poly$name], need_check_poly),
                                  "Done" = list(pred_result[no_need_check_poly$name], no_need_check_poly))
              
              saveRDS(export_list, 
                      paste0(temp_AI, "/", update_polygon_class, "_", deviation_x[batch_number], "_", deviation_y[batch_number], ".rds"))
            }
          }
          #saveRDS(pred_result, paste0("test", batch_number, ".rds"))
        } else {
          pred_result <- batch_result[batch_number,,,i] %>% 
            replace(which(batch_result[batch_number,,,i] <= point_threshold), 0) %>% 
            replace(which(batch_result[batch_number,,,i] > point_threshold), 1) %>% 
            fillHull() %>% 
            bwlabel() %>% 
            computeFeatures.moment()
          
          if (length(pred_result) > 0) {
            
            pred_result <- XY2LonLat(x = pred_result[,1] + deviation_x[batch_number], 
                                     y = pred_result[,2] + deviation_y[batch_number], 
                                     zoom = basal_layer + 1)
            
            blank_point <- readRDS("store_data_for_program/blank_leaflet_points.rds")
            
            update_point_class <- paste0(modified_label_table$Primary_layer_ID[i],
                                         "_",
                                         modified_label_table$Secondary_layer_ID[i],
                                         "_",
                                         modified_label_table$Tertiary_layer_ID[i],
                                         "_",
                                         modified_label_table$layer_option[i])
            
            point_row <- nrow(pred_result)
            
            blank_point[1:point_row, 1] <- modified_label_table$layer_option[i]
            blank_point[1:point_row, 3] <- pred_result$lon
            blank_point[1:point_row, 4] <- pred_result$lat
            blank_point[1:point_row, 5] <- modified_label_table$Primary_layer_ID[i]
            blank_point[1:point_row, 6] <- modified_label_table$Secondary_layer_ID[i]
            blank_point[1:point_row, 7] <- modified_label_table$Tertiary_layer_ID[i]
            blank_point[, 2] <- paste0(blank_point[, 1], "_", blank_point[, 3], "_", blank_point[, 4])
            
            saveRDS(blank_point, paste0(temp_AI, "/", update_point_class, "_", deviation_x[batch_number], "_", deviation_y[batch_number], ".rds"))
          }
        }
      }
    }
  }, future.seed = TRUE)
  file.remove(check_file_resolve)
  file.remove(batch_result_location)
}
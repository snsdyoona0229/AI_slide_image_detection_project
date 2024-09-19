FISH_1p_19q_AI_run_function <- function(image_result, 
                                        basal_layer = 11, 
                                        polygon_simplify = 0.00005,
                                        result_type = c("P_1", "P_2", "G"),
                                        temp_AI) {
  
  for (i in 1:length(result_type)) {
    if (result_type[i] == "G") {
      
      pred_result <- image_result[1,,,i] %>% bwlabel()
      
      if (max(pred_result) > 0) {
        pred_result <- lapply(ocontour(pred_result), function(x){
          XY2LonLat(x[,1], 
                    x[,2],
                    zoom = basal_layer + 1)
        })
        
        blank_geo <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
        
        pred_result <- lapply(pred_result, function(x){
          lapply(1:nrow(x), function(y){
            list(x[y,1], x[y,2])
          })
        })
        
        update_polygon_class <- paste0(1,
                                       "_",
                                       1,
                                       "_",
                                       1,
                                       "_",
                                       result_type[i])
        
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
          #for (i in 1:length(pred_result)) {
          #  pred_result[[i]] <- lawn_union(pred_result[[i]], pred_result[[i]]) %>% geojson_split_function()
          #}
          #pred_result <- unlist(pred_result, recursive = FALSE) %>% geojson_merge_function(method = "exact")
          saveRDS(list("1_1_1" = pred_result), paste0(temp_AI, "/polygons.rds"))
        }
      }
      #saveRDS(pred_result, paste0("test", 1, ".rds"))
    } else {
      #i <- 1
      pred_result <- image_result[1,,,i] %>% bwlabel() %>% computeFeatures.moment()
      
      if (length(pred_result) > 0) {
        
        pred_result <- XY2LonLat(x = pred_result[,1], 
                                 y = pred_result[,2], 
                                 zoom = basal_layer + 1)
        
        if (file.exists(paste0(temp_AI, "/points.rds")) == FALSE) {
          blank_point <- readRDS("store_data_for_program/blank_leaflet_points.rds")
          
          update_point_class <- paste0(1,
                                       "_",
                                       1,
                                       "_",
                                       1,
                                       "_",
                                       result_type[i])
          
          point_row <- nrow(pred_result)
          
          blank_point[1:point_row, 1] <- result_type[i]
          blank_point[1:point_row, 3] <- pred_result$lon
          blank_point[1:point_row, 4] <- pred_result$lat
          blank_point[1:point_row, 5] <- 1
          blank_point[1:point_row, 6] <- 1
          blank_point[1:point_row, 7] <- 1
          blank_point[, 2] <- paste0(blank_point[, 1], "_", blank_point[, 3], "_", blank_point[, 4])
          
          saveRDS(blank_point, paste0(temp_AI, "/points.rds"))
        } else {
          ori_point <- readRDS(paste0(temp_AI, "/points.rds"))
          blank_point <- readRDS("store_data_for_program/blank_leaflet_points.rds")
          
          update_point_class <- paste0(1,
                                       "_",
                                       1,
                                       "_",
                                       1,
                                       "_",
                                       result_type[i])
          
          point_row <- nrow(pred_result)
          
          blank_point[1:point_row, 1] <- result_type[i]
          blank_point[1:point_row, 3] <- pred_result$lon
          blank_point[1:point_row, 4] <- pred_result$lat
          blank_point[1:point_row, 5] <- 1
          blank_point[1:point_row, 6] <- 1
          blank_point[1:point_row, 7] <- 1
          blank_point[, 2] <- paste0(blank_point[, 1], "_", blank_point[, 3], "_", blank_point[, 4])
          blank_point <- rbind(blank_point, ori_point)
          
          saveRDS(blank_point, paste0(temp_AI, "/points.rds"))
        }
      }
    }
  }
  #file.remove(check_file_resolve)
}

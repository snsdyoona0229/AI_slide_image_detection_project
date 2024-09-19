slide_result_function <- function(batch_result, 
                                  deviation_x, 
                                  deviation_y, 
                                  basal_layer, 
                                  overlap_pixel,
                                  #polygon_threshold, 
                                  #point_threshold,
                                  #polygon_simplify,
                                  modified_label_table,
                                  temp_AI,
                                  check_file_resolve) {
 
  
batch_size <- dim(batch_result)[1]

lapply(1:batch_size, function(batch_number) {
#    for (i in 1:nrow(modified_label_table)) {
     lapply(1:nrow(modified_label_table), function(i) {
	  #blas_set_num_threads(parallel::detectCores())
	 
	  setDTthreads(thread= parallel::detectCores())

	  
 
      if (modified_label_table$Use_in_label[i] == "Y") {
        if (str_detect(modified_label_table$Inference_G[i], "G")) {
          if (modified_label_table$Fill_hole[i] == "Y") {
            shapes <- batch_result[batch_number,,,i] %>% 
              replace(which(batch_result[batch_number,,,i] <= modified_label_table$Threshold[i]), 0) %>% 
              replace(which(batch_result[batch_number,,,i] > modified_label_table$Threshold[i]), 1) %>% fillHull()
            
            outer <- shapes %>% bwlabel()
            
            if (modified_label_table$Ignore_pixel_min[i] > 0) {
              pixel_condiiton <- outer %>% computeFeatures.shape() %>% as.data.table()
              pixel_condiiton <- pixel_condiiton$s.area
              pixel_condiiton <- which(pixel_condiiton <= modified_label_table$Ignore_pixel_min[i])
              #outer[which(outer %in% pixel_condiiton)] <- 0
			  outer[which(any(pixel_condiiton ==outer))] <- 0
              outer <- outer %>% bwlabel()
            }
           
            if (modified_label_table$Ignore_pixel_max[i] != "-") {
              pixel_condiiton <- outer %>% computeFeatures.shape() %>% as.data.table()
              pixel_condiiton <- pixel_condiiton$s.area
              pixel_condiiton <- which(pixel_condiiton >= as.numeric(modified_label_table$Ignore_pixel_max[i]))
              #outer[which(outer %in% pixel_condiiton)] <- 0
			  outer[which(any(pixel_condiiton ==outer))] <- 0
              outer <- outer %>% bwlabel()
            }
            
            if (max(outer) > 0) {
              outer_contour <- outer %>% ocontour()
              outer_contour <- lapply(outer_contour, function(x){
                XY2LonLat(x[,1] + deviation_x[batch_number], 
                          x[,2] + deviation_y[batch_number],
                          zoom = basal_layer + 1)
              })
              outer_contour <- lapply(outer_contour, function(x){
                raw_poly <- lapply(1:nrow(x), function(y){
                  list(x[y,1], x[y,2])
                })
                c(raw_poly, list(list(x[1,1], x[1,2])))
              })
              
              update_polygon_class <- modified_label_table$Inference_G[i]
              
              batch_width_size <- dim(batch_result)[2]
              x_chunk <- (deviation_x[batch_number] + overlap_pixel)/(batch_width_size - 2 * overlap_pixel)
              y_chunk <- (deviation_y[batch_number] + overlap_pixel)/(batch_width_size - 2 * overlap_pixel)
              current_chunk <- paste0("_", x_chunk, "_", y_chunk, "_")
              
              outer_geojson <- contour_to_geojson_function(contour = outer_contour,
                                                           polygon_simplify = modified_label_table$Polygon_simplify[i] %>% as.numeric(),
                                                           current_chunk = current_chunk)
              multi_poly_outer_id <- names(outer_geojson)[names(outer_geojson) %>% str_detect("number2,")]
              
              if (length(multi_poly_outer_id) > 0) {
                multi_poly_outer_id <- multi_poly_outer_id %>% sapply(function(x){
                  (x %>% str_split("out,id|,number") %>% unlist())[2] %>% as.integer()
                }) %>% unname() 
                
                outer_poly_belong_multi_inner_id <- lapply(multi_poly_outer_id, function(x){
                  names(outer_geojson)[names(outer_geojson) %>% str_detect(paste0(",id", x, ",number"))] %>% str_sort()
                })
                
                for (ii in 1:length(multi_poly_outer_id)) {
                  #ii <- 13
                  outer_geojson_id_max <- outer_geojson %>% names() %>% str_split("out,id|,number") %>% sapply(function(x){x[2]}) %>% as.integer() %>% max()
                  new_outer_geojson_id <- str_split(outer_poly_belong_multi_inner_id[[ii]], ",id") %>% 
                    sapply(function(x){x[1]}) %>% 
                    paste0(",id", 
                           (outer_geojson_id_max + 1):(outer_geojson_id_max + length(outer_poly_belong_multi_inner_id[[ii]])),
                           ",number1")
                  for (j in 1:length(new_outer_geojson_id)) {
                    names(outer_geojson)[which(names(outer_geojson) == outer_poly_belong_multi_inner_id[[ii]][j])] <- new_outer_geojson_id[j]
				  }
                }
              }
            } else {
              outer_geojson <- list()
            }
            inner_geojson <- list()
          } else if (modified_label_table$Fill_hole[i] == "N") {
            shapes <- batch_result[batch_number,,,i] %>% 
              replace(which(batch_result[batch_number,,,i] <= modified_label_table$Threshold[i]), 0) %>% 
              replace(which(batch_result[batch_number,,,i] > modified_label_table$Threshold[i]), 1)
            
            outer <- shapes %>% bwlabel()
            inner <- (shapes %>% fillHull() - shapes) %>% bwlabel()
            
            if (modified_label_table$Ignore_pixel_min[i] > 0) {
              pixel_condiiton <- outer %>% computeFeatures.shape() %>% as.data.table()
              pixel_condiiton <- pixel_condiiton$s.area
              pixel_condiiton <- which(pixel_condiiton <= modified_label_table$Ignore_pixel_min[i])
              #outer[which(outer %in% pixel_condiiton)] <- 0
			  outer[which(any(pixel_condiiton ==outer))] <- 0
              outer <- outer %>% bwlabel()
              
              pixel_condiiton <- inner %>% computeFeatures.shape() %>% as.data.table()#
              pixel_condiiton <- pixel_condiiton$s.area
              pixel_condiiton <- which(pixel_condiiton <= modified_label_table$Ignore_pixel_min[i])
             # inner[which(inner %in% pixel_condiiton)] <- 0
			 inner[which(any(pixel_condiiton ==inner))] <- 0
              inner <- inner %>% bwlabel()
            }
            
            if (modified_label_table$Ignore_pixel_max[i] != "-") {
              pixel_condiiton <- outer %>% computeFeatures.shape() %>% as.data.table()
              pixel_condiiton <- pixel_condiiton$s.area
              pixel_condiiton <- which(pixel_condiiton >= as.numeric(modified_label_table$Ignore_pixel_max[i]))
             # outer[which(outer %in% pixel_condiiton)] <- 0
			 outer[which(any(pixel_condiiton ==outer))] <- 0
              outer <- outer %>% bwlabel()
			  
			  
              pixel_condiiton <- inner %>% computeFeatures.shape() %>% as.data.table()
              pixel_condiiton <- pixel_condiiton$s.area
              pixel_condiiton <- which(pixel_condiiton >= as.numeric(modified_label_table$Ignore_pixel_max[i]))
             # inner[which(inner %in% pixel_condiiton)] <- 0
             inner[which(any(pixel_condiiton ==inner))] 
              inner <- inner %>% bwlabel()
            }
            
            if (max(outer) > 0) {
              outer_contour <- outer %>% ocontour()
              outer_contour <- lapply(outer_contour, function(x){
                XY2LonLat(x[,1] + deviation_x[batch_number], 
                          x[,2] + deviation_y[batch_number],
                          zoom = basal_layer + 1)
              })
              
              outer_contour <- lapply(outer_contour, function(x){
                raw_poly <- lapply(1:nrow(x), function(y){
                  list(x[y,1], x[y,2])
                })
                c(raw_poly, list(list(x[1,1], x[1,2])))
              })
              inner_contour <- inner %>% ocontour()
              inner_belong <- inner_contour %>% lapply(function(x){
                x[1,]
              }) %>% abind::abind(along = 0)
              inner_belong <- (outer %>% dilate(kern = makeBrush(5, shape = "diamond")))[inner_belong]
              inner_contour <- inner_contour[which(inner_belong > 0)]
              inner_belong <- inner_belong[which(inner_belong > 0)]
              inner_contour <- lapply(inner_contour, function(x){
                XY2LonLat(x[,1] + deviation_x[batch_number], 
                          x[,2] + deviation_y[batch_number],
                          zoom = basal_layer + 1)
              })
              inner_contour <- lapply(inner_contour, function(x){
                raw_poly <- lapply(1:nrow(x), function(y){
                  list(x[y,1], x[y,2])
                })
                c(raw_poly, list(list(x[1,1], x[1,2])))
              })
              
              update_polygon_class <- modified_label_table$Inference_G[i]
              
              batch_width_size <- dim(batch_result)[2]
              x_chunk <- (deviation_x[batch_number] + overlap_pixel)/(batch_width_size - 2 * overlap_pixel)
              y_chunk <- (deviation_y[batch_number] + overlap_pixel)/(batch_width_size - 2 * overlap_pixel)
              current_chunk <- paste0("_", x_chunk, "_", y_chunk, "_")
              
              outer_geojson <- contour_to_geojson_function(contour = outer_contour,
                                                           polygon_simplify = modified_label_table$Polygon_simplify[i] %>% as.numeric(),
                                                           current_chunk = current_chunk)
              
              inner_geojson <- contour_to_geojson_function(contour = inner_contour,
                                                           polygon_simplify = modified_label_table$Polygon_simplify[i] %>% as.numeric(),
                                                           current_chunk = current_chunk, 
                                                           inner_belong = inner_belong)
              
              multi_poly_outer_id <- names(outer_geojson)[names(outer_geojson) %>% str_detect("number2,")]
              if (length(multi_poly_outer_id) > 0) {
                multi_poly_outer_id <- multi_poly_outer_id %>% sapply(function(x){
                  (x %>% str_split("out,id|,number") %>% unlist())[2] %>% as.integer()
                }) %>% unname() 
                
                inner_poly_belong_multi_outer_id <- lapply(multi_poly_outer_id, function(x){
                  names(inner_geojson)[names(inner_geojson) %>% str_detect(paste0(",belong", x, ","))]
                })
                
                outer_poly_belong_multi_inner_id <- lapply(multi_poly_outer_id, function(x){
                  names(outer_geojson)[names(outer_geojson) %>% str_detect(paste0(",id", x, ",number"))] %>% str_sort()
                })
                
                for (ii in 1:length(multi_poly_outer_id)) {
                  #ii <- 13
                  outer_geojson_id_max <- outer_geojson %>% names() %>% str_split("out,id|,number") %>% sapply(function(x){x[2]}) %>% as.integer() %>% max()
                  new_outer_geojson_id <- str_split(outer_poly_belong_multi_inner_id[[ii]], ",id") %>% 
                    sapply(function(x){x[1]}) %>% 
                    paste0(",id", 
                           (outer_geojson_id_max + 1):(outer_geojson_id_max + length(outer_poly_belong_multi_inner_id[[ii]])),
                           ",number1")
                  for (j in 1:length(new_outer_geojson_id)) {
                    names(outer_geojson)[which(names(outer_geojson) == outer_poly_belong_multi_inner_id[[ii]][j])] <- new_outer_geojson_id[j]
				  }
                  if (length(inner_poly_belong_multi_outer_id[[ii]]) > 0) {
                    #inner_first_point <- 
                    inner_geojson_point <- inner_geojson[inner_poly_belong_multi_outer_id[[ii]]] %>% lapply(function(x){(x %>% lawn_coordall())[1,]}) %>% abind::abind(along = 0) %>% unname()
                    inner_df <- data.table(
                      lon = inner_geojson_point[,1],
                      lat = inner_geojson_point[,2],
                      original = inner_poly_belong_multi_outer_id[[ii]],
                      belong = NA,
                      final = NA,
                      stringsAsFactors = FALSE
                    )
                    for (j in 1:length(new_outer_geojson_id)) {
                      outer_geojson_point <- outer_geojson[[new_outer_geojson_id[j]]] %>% lawn_coordall()
                      #print(j)
                      #print(dim(outer_geojson_point))
                      poly_condition <- pointinpolygon(P = list(x = inner_geojson_point[,1], y = inner_geojson_point[,2]),
                                                       A = list(x = outer_geojson_point[,1], y = outer_geojson_point[,2]))
                      if (sum(poly_condition) > 0) {
                        inner_df$belong[which(poly_condition == 1)] <- new_outer_geojson_id[j]
                      }
                    }
                    inner_df$final <- paste0(
                      inner_df$original %>% str_split(",belong") %>% sapply(function(x){x[1]}),
                      ",belong",
                      inner_df$belong %>% str_split(",id|,number") %>% sapply(function(x){x[2]}),
                      ","
                    )
                    #print(inner_df$original[which(is.na(inner_df$belong))])
                    inner_geojson[inner_df$original[which(is.na(inner_df$belong))]] <- NULL
                    names(inner_geojson[inner_df$original[which(is.na(inner_df$belong)) == FALSE]]) <- inner_df$final[which(is.na(inner_df$belong) == FALSE)]
                  }
                }
              }
            } else {
              outer_geojson <- list()
              inner_geojson <- list()
            }
          }
          
          if (length(outer_geojson) > 0) {
            outer_export_list <- geojson_to_done_check_list_function(geojson = outer_geojson,
                                                                     x_chunk = x_chunk,
                                                                     y_chunk = y_chunk,
                                                                     overlap_pixel = overlap_pixel,
                                                                     batch_width_size = batch_width_size,
                                                                     deviation_x = deviation_x[batch_number],
                                                                     deviation_y = deviation_y[batch_number],
                                                                     basal_layer = basal_layer,
                                                                     update_polygon_class = update_polygon_class)
          } else {
            outer_export_list <- list("Check" = list(NULL, NULL),
                                      "Done" = list(NULL, NULL))
          }
          
          if (length(inner_geojson) > 0) {
            inner_export_list <- geojson_to_done_check_list_function(geojson = inner_geojson,
                                                                     x_chunk = x_chunk,
                                                                     y_chunk = y_chunk,
                                                                     overlap_pixel = overlap_pixel,
                                                                     batch_width_size = batch_width_size,
                                                                     deviation_x = deviation_x[batch_number],
                                                                     deviation_y = deviation_y[batch_number],
                                                                     basal_layer = basal_layer,
                                                                     update_polygon_class = update_polygon_class)
          } else {
            inner_export_list <- list("Check" = list(NULL, NULL),
                                      "Done" = list(NULL, NULL))
          }
          
          if (length(outer_geojson) > 0) {
            saveRDS(
              list(
                "outer" = outer_export_list,
                "inner" = inner_export_list
              ), 
              paste0(temp_AI, "/", update_polygon_class, "_", deviation_x[batch_number], "_", deviation_y[batch_number], ".rds"))
          }
        }
        
        if (str_detect(modified_label_table$Inference_P[i], "P")) {
          pred_result <- batch_result[batch_number,,,i]
          
          if (overlap_pixel != 0) {
            pred_result[1:overlap_pixel,] <- 0
            pred_result[(dim(pred_result)[1] - overlap_pixel + 1):(dim(pred_result)[1]),] <- 0
            pred_result[,1:overlap_pixel] <- 0
            pred_result[,(dim(pred_result)[2] - overlap_pixel + 1):(dim(pred_result)[2])] <- 0
          }
          
          if (modified_label_table$Fill_hole[i] == "Y") {
            pred_result <- pred_result %>% 
              replace(which(pred_result <= modified_label_table$Threshold[i]), 0) %>% 
              replace(which(pred_result > modified_label_table$Threshold[i]), 1) %>% 
              fillHull() %>% 
              bwlabel() 
          } else if (modified_label_table$Fill_hole[i] == "N") {
            pred_result <- pred_result %>% 
              replace(which(pred_result <= modified_label_table$Threshold[i]), 0) %>% 
              replace(which(pred_result > modified_label_table$Threshold[i]), 1) %>% 
              bwlabel() 
          }
          
          if (modified_label_table$Ignore_pixel_min[i] > 0) {
            pixel_condiiton <- pred_result %>% computeFeatures.shape() %>% as.data.table()
            pixel_condiiton <- pixel_condiiton$s.area
            pixel_condiiton <- which(pixel_condiiton <= modified_label_table$Ignore_pixel_min[i])
            #pred_result[which(pred_result %in% pixel_condiiton)] <- 0
			pred_result[which(any(pixel_condiiton ==pred_result))] <- 0
            pred_result <- pred_result %>% bwlabel()
          }
          
          pred_result <- pred_result %>% computeFeatures.moment()
          
          if (length(pred_result) > 0) {
            pred_result <- XY2LonLat(x = pred_result[,1] + deviation_x[batch_number], 
                                     y = pred_result[,2] + deviation_y[batch_number], 
                                     zoom = basal_layer + 1)
            
            blank_point <- readRDS("store_data_for_program/blank_leaflet_points.rds")
            
            update_point_class <- modified_label_table$Inference_P[i]
            
            point_row <- nrow(pred_result)
            
            blank_point[1:point_row, 1] <- (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[4:5] %>% paste0(collapse = "_")
            blank_point[1:point_row, 3] <- pred_result$lon
            blank_point[1:point_row, 4] <- pred_result$lat
            blank_point[1:point_row, 5] <- (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[1]
            blank_point[1:point_row, 6] <- (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[2]
            blank_point[1:point_row, 7] <- (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[3]
            blank_point[, 2] <- paste0(blank_point[, 1], "_", blank_point[, 3], "_", blank_point[, 4])
            
            saveRDS(blank_point, paste0(temp_AI, "/", update_point_class, "_", deviation_x[batch_number], "_", deviation_y[batch_number], ".rds"))
          }
        }
        
        if (str_detect(modified_label_table$Inference_H[i], "H")) {
          shapes <- batch_result[batch_number,,,i] %>% 
            replace(which(batch_result[batch_number,,,i] <= modified_label_table$Threshold[i]), 0)
          
          if (overlap_pixel != 0) {
            shapes <- shapes[(overlap_pixel + 1):(dim(shapes)[1] - overlap_pixel),
                             (overlap_pixel + 1):(dim(shapes)[2] - overlap_pixel)]
          }
          
          if (max(shapes) > 0) {
            tile_size <- dim(batch_result)[2] - overlap_pixel * 2
            shirnk_level <- 2^(basal_layer - 5)
            final_size <- tile_size/shirnk_level
            shapes <- shapes %>% resize(w = final_size, h = final_size)
            update_heat_class <- modified_label_table$Inference_H[i]
            saveRDS(list(shapes,
                         c((deviation_x[batch_number] + overlap_pixel)/shirnk_level + 1, (deviation_x[batch_number] + overlap_pixel)/shirnk_level + final_size),
                         c((deviation_y[batch_number] + overlap_pixel)/shirnk_level + 1, (deviation_y[batch_number] + overlap_pixel)/shirnk_level + final_size)
            ), 
            paste0(temp_AI, "/", update_heat_class, "_", deviation_x[batch_number], "_", deviation_y[batch_number], ".rds"))
          }
        }
      }
    })
  })#, future.seed = TRUE)
  file.remove(check_file_resolve)
  #parallel::stopCluster(cl)
  
}

LonLat2XY <- function(lon_deg, lat_deg, zoom = 12){
  lon_deg <- lon_deg %>% as.numeric()
  lat_deg <- lat_deg %>% as.numeric()
  n <- 2^(zoom-1)
  X <- ((lon_deg + 180) / 360) * n
  sec <- function(x) 1/cos(x)
  lat_rad <- lat_deg * pi/180
  Y <- (1 - (log(tan(lat_rad) + sec(lat_rad)) / pi)) / 2 * n
  df <- data.frame(
    x = round(256 * X),
    y = round(256 * Y)
  )
  row.names(df) <- NULL
  df
}

XY2LonLat <- function(x, y, zoom = 12){
  n <- 2^(zoom-1)
  lon_deg <- (x/256) / n * 360.0 - 180.0
  tmp <- tanh( pi * (1 - 2 * (y/256) / n))
  lat_deg <- asin(tmp) * 180/pi
  data.frame(lon = lon_deg, lat = lat_deg)
}

point_line_function <- function(point1, point2) {
  x_dim <- point2[1] - point1[1]
  y_dim <- point2[2] - point1[2]
  long_dim <- max(abs(x_dim), abs(y_dim))
  if (long_dim == 0) {
    return(data.frame(x = c(point1[1], point2[1]), y = c(point1[2], point2[2])))
  } else {
    if (long_dim == abs(x_dim)) {
      x_location <- point1[1]:point2[1]
      y_location <- round(point1[2] + y_dim/abs(x_dim)*(0:(length(x_location) - 1)))
    } else {
      y_location <- point1[2]:point2[2]
      x_location <- round(point1[1] + x_dim/abs(y_dim)*(0:(length(y_location) - 1)))
    }
    return(data.frame(x = x_location, y = y_location))
  }
}

geojson_merge_function <- function(polygon_list, method = "blur") {
  if (method == "blur") {
    return(polygon_list %>% unname() %>% lawn_featurecollection() %>% lawn_merge())
  } else if (method == "exact") {
    poly_u <- polygon_list[[1]]
    for (i in 1:length(polygon_list)) {
      poly_u <- lawn_union(poly_u, polygon_list[[i]])
    }
    return(poly_u)
  }
}

geojson_split_function <- function(multipolygon) {
  if (multipolygon$geometry$type == "Polygon") {
    return(list(multipolygon))
  } else {
    polygon_number <- multipolygon$geometry$coordinates %>% length()
    blank_geo <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
    new_polygon_id <- sample(1:10000000, polygon_number)
    split_result <- lapply(1:polygon_number, function(x){
      id_num <- new_polygon_id[x]
      data_in <- blank_geo
      data_in$geometry$coordinates <- multipolygon$geometry$coordinates[[x]]
      data_in$properties$layerId <- as.character(id_num)
      data_in$properties$edit_id <- as.character(id_num)
      data_in$properties$`_leaflet_id` <- id_num
      class(data_in) <- "polygon"
      data_in
    })
    names(split_result) <- new_polygon_id %>% as.integer()
    return(split_result)
  }
}



polygon_correction_function <- function(polygon_list, method = "one") {
  polygon_number <- length(polygon_list)
  for (j in 1:polygon_number) {
    #j <- 3
    if (class(polygon_list[[j]]$geometry$coordinates) == "list" & (polygon_list[[j]]$geometry$coordinates %>% length()) == 1) {
      polygon_length <- length(polygon_list[[j]]$geometry$coordinates[[1]])
      if (polygon_list[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygon_list[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
        polygon_list[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygon_list[[j]]$geometry$coordinates[[1]][[1]]
      }
      if ((polygon_list[[j]] %>% lawn_kinks())$features %>% length() != 0) {
        all_poly <- polygon_list[[j]] %>% lawn_coordall()
        clip_poly <- polysimplify(list(list(x = all_poly[,1], y = all_poly[,2])))
        pointer_to_max <- list(polygon_number = 0, point_number = 0)
        for (l in 1:length(clip_poly)) {
          if (clip_poly[[l]]$x %>% length() > pointer_to_max$point_number) {
            pointer_to_max$polygon_number <- l
            pointer_to_max$point_number <- clip_poly[[l]]$x %>% length()
          }
        }
        clip_poly <- clip_poly[[pointer_to_max$polygon_number]] %>% abind::abind(along = 2) %>% unname()
        choose_poly <- lapply(1:nrow(clip_poly), function(x){
          list(clip_poly[x,1], clip_poly[x,2])
        })
        polygon_list[[j]]$geometry$coordinates <- list(choose_poly)
      }
      polygon_length <- length(polygon_list[[j]]$geometry$coordinates[[1]])
      if (polygon_list[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygon_list[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
        polygon_list[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygon_list[[j]]$geometry$coordinates[[1]][[1]]
      }
    } else if (class(polygon_list[[j]]$geometry$coordinates) == "array") {
      polygon_length <- nrow(polygon_list[[j]]$geometry$coordinates[1,,])
      if (polygon_list[[j]]$geometry$coordinates[1,1,] %>% paste0(collapse = "_") != polygon_list[[j]]$geometry$coordinates[1,polygon_length,] %>% paste0(collapse = "_")) {
        polygon_list[[j]]$geometry$coordinates <- abind(polygon_list[[j]]$geometry$coordinates, 
                                                        polygon_list[[j]]$geometry$coordinates[1,1,] %>% array(c(1,1,2)),
                                                        along = 2) %>% unname()
      }
      if ((polygon_list[[j]] %>% lawn_kinks())$features %>% length() != 0) {
        all_poly <- polygon_list[[j]] %>% lawn_coordall()
        clip_poly <- polysimplify(list(list(x = all_poly[,1], y = all_poly[,2])))
        pointer_to_max <- list(polygon_number = 0, point_number = 0)
        for (l in 1:length(clip_poly)) {
          if (clip_poly[[l]]$x %>% length() > pointer_to_max$point_number) {
            pointer_to_max$polygon_number <- l
            pointer_to_max$point_number <- clip_poly[[l]]$x %>% length()
          }
        }
        clip_poly <- clip_poly[[pointer_to_max$polygon_number]] %>% abind::abind(along = 2) %>% unname()
        clip_poly <- abind::abind(clip_poly, clip_poly[1,] %>% array(c(1,2)), along = 1) %>% array(c(1,nrow(clip_poly) + 1, 2)) %>% unname()
        polygon_list[[j]]$geometry$coordinates <- clip_poly
      }
    } else if (class(polygon_list[[j]]$geometry$coordinates) == "list" & (polygon_list[[j]]$geometry$coordinates %>% length()) > 1) {
      for (loop_check in 1:length(polygon_list[[j]]$geometry$coordinates)) {
        polygon_length <- nrow(polygon_list[[j]]$geometry$coordinates[[loop_check]])
        if (polygon_list[[j]]$geometry$coordinates[[loop_check]][1,] %>% paste0(collapse = "_") != polygon_list[[j]]$geometry$coordinates[[loop_check]][polygon_length,] %>% paste0(collapse = "_")) {
          polygon_list[[j]]$geometry$coordinates[[loop_check]] <- abind(polygon_list[[j]]$geometry$coordinates[[loop_check]], 
                                                                        polygon_list[[j]]$geometry$coordinates[[loop_check]][1,] %>% array(c(1,2)),
                                                                        along = 1) %>% unname()
        }
      }
      if ((polygon_list[[j]] %>% lawn_kinks())$features %>% length() != 0) {
        for (loop_check in 1:length(polygon_list[[j]]$geometry$coordinates)) {
          all_poly <- polygon_list[[j]]$geometry$coordinates[[loop_check]]
          clip_poly <- polysimplify(list(list(x = all_poly[,1], y = all_poly[,2])))
          pointer_to_max <- list(polygon_number = 0, point_number = 0)
          for (l in 1:length(clip_poly)) {
            if (clip_poly[[l]]$x %>% length() > pointer_to_max$point_number) {
              pointer_to_max$polygon_number <- l
              pointer_to_max$point_number <- clip_poly[[l]]$x %>% length()
            }
          }
          clip_poly <- clip_poly[[pointer_to_max$polygon_number]] %>% abind::abind(along = 2) %>% unname()
          clip_poly <- abind::abind(clip_poly, clip_poly[1,] %>% array(c(1,2)), along = 1) %>% array(c(nrow(clip_poly) + 1, 2)) %>% unname()
          polygon_list[[j]]$geometry$coordinates[[loop_check]] <- clip_poly
        }
      }
    }
  }
  return(polygon_list)
}

slide_result_function <- function(batch_result, 
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
  
  batch_size <- dim(batch_result)[1]
  
  future_lapply(1:batch_size, function(batch_number) {
    for (i in 1:nrow(modified_label_table)) {
      #i <- 1
      #batch_number <- 1
      if (modified_label_table$use_in_label[i] == "Y") {
        if (modified_label_table$layer_option[i] == "G") {
          shapes <- batch_result[batch_number,,,i] %>% 
            replace(which(batch_result[batch_number,,,i] <= polygon_threshold), 0) %>% 
            replace(which(batch_result[batch_number,,,i] > polygon_threshold), 1)
          
          outer <- shapes %>% bwlabel()
          inner <- (shapes %>% fillHull() - shapes) %>% bwlabel()
          
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
            
            update_polygon_class <- paste0(modified_label_table$Primary_layer_ID[i],
                                           "_",
                                           modified_label_table$Secondary_layer_ID[i],
                                           "_",
                                           modified_label_table$Tertiary_layer_ID[i],
                                           "_",
                                           modified_label_table$layer_option[i])
            
            batch_width_size <- dim(batch_result)[2]
            x_chunk <- (deviation_x[batch_number] + overlap_pixel)/(batch_width_size - 2 * overlap_pixel)
            y_chunk <- (deviation_y[batch_number] + overlap_pixel)/(batch_width_size - 2 * overlap_pixel)
            current_chunk <- paste0("_", x_chunk, "_", y_chunk, "_")
            
            outer_geojson <- contour_to_geojson_function(contour = outer_contour,
                                                         polygon_simplify = polygon_simplify,
                                                         current_chunk = current_chunk)
            
            inner_geojson <- contour_to_geojson_function(contour = inner_contour,
                                                         polygon_simplify = polygon_simplify,
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
                  inner_df <- data.frame(
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
                  names(inner_geojson[inner_df$original[which(is.na(inner_df$belong) == FALSE)]]) <- inner_df$final[which(is.na(inner_df$belong) == FALSE)]
                }
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
            
            saveRDS(
              list(
                "outer" = outer_export_list,
                "inner" = inner_export_list
              ), 
              paste0(temp_AI, "/", update_polygon_class, "_", deviation_x[batch_number], "_", deviation_y[batch_number], ".rds"))
          }
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
}


step1_cluster_geojson_merge <- function(all_polygon_df){
  #names(all_polygon_df) <- basename(all_polygon_df_file_location)
  
  done_poly <- lapply(all_polygon_df, function(x) {
    x$Done[[1]]
  }) %>% unname() %>% unlist(recursive = FALSE)
  
  check_table <- lapply(all_polygon_df, function(x) {
    x$Check[[2]]
  }) %>% abind::abind(along = 1) %>% as.data.frame()
  
  check_poly <- lapply(all_polygon_df, function(x) {
    x$Check[[1]]
  }) %>% unname() %>% unlist(recursive = FALSE)
  
  names(check_poly) <- check_table$id
  
  if (length(check_poly) > 0) {
    check_group <- lapply(1:nrow(check_table), function(x){
      check <- c(check_table$check_left[x],
                 check_table$check_right[x],
                 check_table$check_up[x],
                 check_table$check_down[x])
      list(check = check[!is.na(check)],
           origin = check_table$original_chunk[x])
    })
    
    names(check_group) <- check_table$id
    
    check_cluster <- lapply(1:length(check_group), function(x){
      #x <- 1
      #origin_chunk <- check_group[[x]]$origin
      #check_chunk <- check_group[[x]]$check
      
      cluster_poly <- c(names(check_group)[x])
      total_check_chunk <- check_group[[x]]$check
      check_name <- check_group[[x]]$origin
      for (i in total_check_chunk) {
        #i <- "_1_5_"
        if ((sapply(check_group, function(x){(str_detect(x$origin, i) %>% sum()) > 0}) %>% sum()) > 0) {
          total_check_chunk <- total_check_chunk[total_check_chunk != i]
          check_number_use <- sapply(check_group, function(x){(str_detect(x$origin, i) %>% sum()) > 0}) %>% which()
          check_number_use <- check_number_use[check_number_use > x]
          total_check_poly <- check_poly[check_number_use]
          if (length(total_check_poly) > 0) {
            total_check_poly <- total_check_poly[sapply(check_group[names(total_check_poly)], function(xxx){(intersect(xxx$check, check_name) %>% length()) > 0}) %>% which()]
            if (length(total_check_poly) > 0) {
              cluster_poly <- c(cluster_poly, names(total_check_poly))
            }
          }
        }
      }
      list(Merge_id = cluster_poly, 
           Still_check = total_check_chunk,
           Original_chunk = check_group[[x]]$origin)
    })
    
    names(check_cluster) <- check_table$id
    
    
    #neo_site_cluster <- list()
    
    #merge_name_id <- list()
    site_cluster <- lapply(check_cluster, function(x){
      merge_site <- sapply(check_cluster, function(xx){sum(xx$Merge_id %in% x$Merge_id) > 0}) %>% which()
    })
    
    
    for (i in 1:length(site_cluster)) {
      all_site <- sapply(site_cluster, function(x){sum(x %in% i) > 0}) %>% which()
      site_cluster[[all_site[1]]] <- site_cluster[all_site] %>% unlist() %>% unique()
      
      if (length(all_site) != 1) {
        site_cluster[all_site[2:length(all_site)]] <- NULL
      }
    }
    
    
    per_merge_list <- lapply(1:length(site_cluster), function(x){
      list(Merge_id = check_cluster[site_cluster[[x]]] %>% sapply(function(xx){xx$Merge_id}) %>% unlist() %>% unique(), 
           Still_check = check_cluster[site_cluster[[x]]] %>% sapply(function(xx){xx$Still_check}) %>% unlist() %>% unique(),
           Original_chunk = check_cluster[site_cluster[[x]]] %>% sapply(function(xx){xx$Original_chunk}) %>% unlist() %>% unique(),
           Polygon = check_poly[site_cluster[[x]]])
    })
    
    
    merge_list <- future_lapply(per_merge_list, function(x){
      list(Merge_id = x$Merge_id, 
           Still_check = x$Still_check,
           Original_chunk = x$Original_chunk,
           Polygon = x$Polygon %>% geojson_merge_function(method = "exact"))
    }, future.seed = NULL)
    
    names(merge_list) <- names(site_cluster)
    
    done_merge_poly <- sapply(merge_list, function(x){
      if (length(x$Still_check) == 0) {
        TRUE
      } else {
        FALSE
      }
    }) 
    
    keep_check_list <- merge_list[done_merge_poly == FALSE]
    done_check_list <- merge_list[done_merge_poly]
    done_poly <- c(done_poly, lapply(done_check_list, function(x){
      x$Polygon
    }))
    
    return(list(Done = done_poly,
                Check = keep_check_list))
  } else {
    return(list(Done = done_poly,
                Check = NULL))
  }
}


step2_cluster_geojson_merge <- function(all_polygon_df, last_result){
  pre_step2_result <- step1_cluster_geojson_merge(all_polygon_df = all_polygon_df)
  
  done_poly <- c(last_result$Done, pre_step2_result$Done)
  
  check_group <- c(last_result$Check, pre_step2_result$Check)
  check_poly <- lapply(check_group, function(x){x$Polygon})
  
  check_cluster <- lapply(1:length(check_group), function(x){
    #x <- 10
    #origin_chunk <- check_group[[x]]$origin
    #check_chunk <- check_group[[x]]$check
    
    cluster_poly <- check_group[[x]]$Merge_id
    total_check_chunk <- check_group[[x]]$Still_check
    check_name <- check_group[[x]]$Original_chunk
    for (i in total_check_chunk) {
      #i <- "_1_5_"
      if ((sapply(check_group, function(x){((x$Original_chunk %in% i) %>% sum()) > 0}) %>% sum()) > 0) {
        total_check_chunk <- total_check_chunk[total_check_chunk != i]
        check_number_use <- sapply(check_group, function(x){((x$Original_chunk %in% i) %>% sum()) > 0}) %>% which()
        check_number_use <- check_number_use[check_number_use > x]
        total_check_poly <- check_poly[check_number_use]
        if (length(total_check_poly) > 0) {
          total_check_poly <- total_check_poly[sapply(check_group[names(total_check_poly)], function(xxx){(intersect(xxx$Still_check, check_name) %>% length()) > 0}) %>% which()]
          if (length(total_check_poly) > 0) {
            cluster_poly <- c(cluster_poly, names(total_check_poly))
          }
        }
      }
    }
    list(Merge_id = cluster_poly, 
         Still_check = total_check_chunk,
         Original_chunk = check_group[[x]]$origin)
  })
  
  names(check_cluster) <- names(check_group)
  
  site_cluster <- lapply(check_cluster, function(x){
    merge_site <- sapply(check_cluster, function(xx){sum(xx$Merge_id %in% x$Merge_id) > 0}) %>% which()
  })
  
  for (i in 1:length(site_cluster)) {
    all_site <- sapply(site_cluster, function(x){sum(x %in% i) > 0}) %>% which()
    site_cluster[[all_site[1]]] <- site_cluster[all_site] %>% unlist() %>% unique()
    
    if (length(all_site) != 1) {
      site_cluster[all_site[2:length(all_site)]] <- NULL
    }
  }
  
  per_merge_list <- lapply(1:length(site_cluster), function(x){
    list(Merge_id = check_cluster[site_cluster[[x]]] %>% sapply(function(xx){xx$Merge_id}) %>% unlist() %>% unique(), 
         Still_check = check_cluster[site_cluster[[x]]] %>% sapply(function(xx){xx$Still_check}) %>% unlist() %>% unique(),
         Original_chunk = check_cluster[site_cluster[[x]]] %>% sapply(function(xx){xx$Original_chunk}) %>% unlist() %>% unique(),
         Polygon = check_poly[site_cluster[[x]]])
  })
  
  merge_list <- future_lapply(per_merge_list, function(x){
    list(Merge_id = x$Merge_id, 
         Still_check = x$Still_check,
         Original_chunk = x$Original_chunk,
         Polygon = x$Polygon %>% geojson_merge_function(method = "exact"))
  }, future.seed = NULL)
  
  names(merge_list) <- names(site_cluster)
  
  done_merge_poly <- sapply(merge_list, function(x){
    if (length(x$Still_check) == 0) {
      TRUE
    } else {
      FALSE
    }
  }) 
  
  keep_check_list <- merge_list[done_merge_poly == FALSE]
  done_check_list <- merge_list[done_merge_poly]
  done_poly <- c(done_poly, lapply(done_check_list, function(x){
    x$Polygon
  }))
  
  return(list(Done = done_poly,
              Check = keep_check_list))
}




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

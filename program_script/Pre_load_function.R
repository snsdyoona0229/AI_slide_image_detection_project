geojson_merge_function <- function(polygon_list) {
  return(polygon_list %>% unname() %>% lawn_featurecollection() %>% lawn_merge())
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
    return(data.frame(x = base::c(point1[1], point2[1]), y = base::c(point1[2], point2[2])))
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
    all_check_polygon <- lapply(1:length(center_x_all), function(check_polygon_each) {
      all_point_range <- (base::c(tile_size, 1) - tile_size/2) * 2^(12 - zoom_level[check_polygon_each])
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

polygon_to_raster_function <- function(origin_chunk,
                                       annotation_chunk,
                                       tile_size,
                                       zoom_level) {
  blank_image <- array(0, base::c(tile_size, tile_size))
  
  region_border <- lawn_coordall(origin_chunk)
  region_border <- LonLat2XY(lon_deg = region_border[,1],
                             lat_deg = region_border[,2],
                             zoom = zoom_level + 1)
  edge_x <- min(region_border$x) - 1
  edge_y <- min(region_border$y) - 1
  
  if (is.null(annotation_chunk$geometry$type)) {
    return(blank_image)
  } else if (annotation_chunk$geometry$type == "Polygon") {
    all_point_use <- lawn_coordall(annotation_chunk)
    all_point_use <- LonLat2XY(lon_deg = all_point_use[,1],
                               lat_deg = all_point_use[,2],
                               zoom = zoom_level + 1)
    all_point_use$x <- all_point_use$x - edge_x
    all_point_use$y <- all_point_use$y - edge_y
    all_point_use <- all_point_use %>% as.matrix()
    fill_point <- lapply(1:(nrow(all_point_use) - 1), function(x) {
      point_line_function(all_point_use[x,], all_point_use[x+1,])
    }) %>% abind::abind(along = 1)
    fill_point <- replace(fill_point, which(fill_point > tile_size), tile_size)
    fill_point <- replace(fill_point, which(fill_point < 1), 1)
    #print(fill_point)
    blank_image[fill_point] <- 1
    blank_image <- blank_image %>% fillHull()
    return(blank_image)
  } else if (annotation_chunk$geometry$type == "MultiPolygon") {
    temp_blank1 <- blank_image
    temp_blank2 <- blank_image
    fill_point <- list()
    empty_point <- list()
    
    for (check_poly in 1:length(annotation_chunk$geometry$coordinates)) {
      if (annotation_chunk$geometry$coordinates[[check_poly]] %>% class() == "list") {
        fill_point[[length(fill_point) + 1]] <- annotation_chunk$geometry$coordinates[[check_poly]][[1]]
        for (kp in 2:length(annotation_chunk$geometry$coordinates[[check_poly]])) {
          empty_point[[length(empty_point) + 1]] <- annotation_chunk$geometry$coordinates[[check_poly]][[kp]]
        }
      } else if (annotation_chunk$geometry$coordinates[[check_poly]] %>% class() == "array") {
        fill_point[[length(fill_point) + 1]] <- annotation_chunk$geometry$coordinates[[check_poly]][1,,]
      }
    }
    
    fill_point <- lapply(fill_point, function(x){
      all_point_use <- LonLat2XY(lon_deg = x[,1],
                                 lat_deg = x[,2],
                                 zoom = zoom_level + 1)
      all_point_use$x <- all_point_use$x - edge_x
      all_point_use$y <- all_point_use$y - edge_y
      all_point_use <- all_point_use %>% as.matrix()
      fill_point2 <- lapply(1:(nrow(all_point_use) - 1), function(x) {
        point_line_function(all_point_use[x,], all_point_use[x+1,])
      }) %>% abind::abind(along = 1)
      fill_point2 <- replace(fill_point2, which(fill_point2 > tile_size), tile_size)
      fill_point2 <- replace(fill_point2, which(fill_point2 < 1), 1)
      fill_point2
    })
    
    empty_point <- lapply(empty_point, function(x){
      all_point_use <- LonLat2XY(lon_deg = x[,1],
                                 lat_deg = x[,2],
                                 zoom = zoom_level + 1)
      all_point_use$x <- all_point_use$x - edge_x
      all_point_use$y <- all_point_use$y - edge_y
      all_point_use <- all_point_use %>% as.matrix()
      empty_point <- lapply(1:(nrow(all_point_use) - 1), function(x) {
        point_line_function(all_point_use[x,], all_point_use[x+1,])
      }) %>% abind::abind(along = 1)
      empty_point <- replace(empty_point, which(empty_point > tile_size), tile_size)
      empty_point <- replace(empty_point, which(empty_point < 1), 1)
      empty_point
    })
    
    if (length(fill_point) > 0) {
      for (fi_poly in 1:length(fill_point)) {
        temp_blank1 <- temp_blank2
        temp_blank1[fill_point[[fi_poly]]] <- 1
        blank_image <- blank_image + temp_blank1 %>% fillHull()
      }
    }
    
    if (length(empty_point) > 0) {
      for (en_poly in 1:length(empty_point)) {
        temp_blank1 <- temp_blank2
        temp_blank1[empty_point[[en_poly]]] <- 1
        blank_image <- blank_image - temp_blank1 %>% fillHull()
      }
    }
    return(blank_image)
  }
}

point_to_raster_function <- function(origin_chunk,
                                     annotation_chunk,
                                     tile_size,
                                     zoom_level,
                                     point_size) {
  blank_image <- array(0, base::c(tile_size,tile_size))
  
  region_border <- lawn_coordall(origin_chunk)
  
  check_use_point <- annotation_chunk %>% filter(point_lon >= min(region_border[,1]),
                                                 point_lon <= max(region_border[,1]),
                                                 point_lat >= min(region_border[,2]),
                                                 point_lat <= max(region_border[,2]))
  
  if (nrow(check_use_point) == 0) {
    return(blank_image)
  } else {
    region_border <- LonLat2XY(lon_deg = region_border[,1],
                               lat_deg = region_border[,2],
                               zoom = zoom_level + 1)
    edge_x <- min(region_border$x) - 1
    edge_y <- min(region_border$y) - 1
    
    all_point_use <- LonLat2XY(lon_deg = check_use_point$point_lon,
                               lat_deg = check_use_point$point_lat,
                               zoom = zoom_level + 1)
    all_point_use$x <- all_point_use$x - edge_x
    all_point_use$y <- all_point_use$y - edge_y
    all_point_use <- all_point_use %>% as.matrix()
    blank_image[all_point_use] <- 1
    blank_image <- blank_image %>% dilate(kern = makeBrush(size = point_size, shape = "disc"))
    return(blank_image)
  }
}

get_train_future_batch_function <- function(batch_size,
                                            input_size,
                                            layer_output_number,
                                            info_option,
                                            all_pyramid_layer,
                                            simplify_table) {
  check_point_use_file <- readRDS("Point_annotation_pool/check_point_use_file.rds")
  check_polygon_use_file <- readRDS("Polygon_annotation_pool/check_polygon_use_file.rds")
  
  sample_number <- sample(1:nrow(simplify_table), size = batch_size, replace = TRUE, prob = simplify_table$probability)
  sample_table <- simplify_table[sample_number,]
  
  point_annotation <- lapply(sample_table$slide %>% basename(), function(x){
    readRDS(paste0("Point_annotation_pool/", x))
  })
  polygon_annotation <- lapply(sample_table$slide %>% basename(), function(x){
    readRDS(paste0("Polygon_annotation_pool/", x))
  })
  
  point_select <- lapply(1:nrow(sample_table), function(x) {
    look_table <- readRDS(paste0("Sample_pool/", sample_table$type[x], "/", sample_table$slide[x] %>% basename()))
    sampale_number <- sample(1:nrow(look_table), 1)
    look_table[sampale_number,]
  }) %>% abind::abind(along = 1) %>% as.data.frame()
  point_select_xy <- LonLat2XY(lon_deg = point_select$lon, lat_deg = point_select$lat, zoom = 12 + 1)
  
  
  output_array <- array(0, base::c(batch_size, input_size, input_size, layer_output_number))
  if (info_option$slide_input_type == "Merge train") {
    train_array <- array(0, base::c(batch_size, input_size, input_size, 3 * length(all_pyramid_layer)))
  } else {
    train_array <- array(0, base::c(batch_size, input_size, input_size, 3))
  }
  
  rotate_number <- sample((0:(info_option$image_rotate * 3)) * 90, batch_size, replace = TRUE)
  flip_flop_number <- sample(0:info_option$image_flip_flop, batch_size, replace = TRUE)
  
  if (info_option$slide_input_type == "Merge train") {
    split_output_layer <- max(all_pyramid_layer)
    split_output_layer <- rep(split_output_layer, batch_size)
    point_select_xy$x <- point_select_xy$x + sample(random_deviation, length(point_select_xy$x), replace = TRUE) * 2^(12 - output_layer)
    point_select_xy$y <- point_select_xy$y + sample(random_deviation, length(point_select_xy$y), replace = TRUE) * 2^(12 - output_layer)
    
    for (i in 1:nrow(point_select)) {
      train_array[i,,,] <- NDPI_center_RGB_pyramid_batch_Num(file_name = point_select$slide_name[i],
                                                             x_pixel = point_select_xy$x[i],
                                                             y_pixel = point_select_xy$y[i],
                                                             all_pyramid_layer = all_pyramid_layer,
                                                             tilesize = input_size)
    }
  } else {
    sample_train_weight <- 4 ^ (all_pyramid_layer %>% - min(all_pyramid_layer))
    if (length(all_pyramid_layer) == 1) {
      split_output_layer <- rep(all_pyramid_layer, batch_size)
      point_select_xy$x <- point_select_xy$x + sample(random_deviation, length(point_select_xy$x), replace = TRUE) * 2^(12 - output_layer)
      point_select_xy$y <- point_select_xy$y + sample(random_deviation, length(point_select_xy$y), replace = TRUE) * 2^(12 - output_layer)
      
      for (i in 1:nrow(point_select)) {
        train_array[i,,,] <- NDPI_center_RGB_pyramid_batch_Num(file_name = point_select$slide_name[i],
                                                               x_pixel = point_select_xy$x[i],
                                                               y_pixel = point_select_xy$y[i],
                                                               all_pyramid_layer = all_pyramid_layer,
                                                               tilesize = input_size)
      }
    } else {
      split_output_layer <- sample(all_pyramid_layer, 
                                   size = batch_size, 
                                   prob = sample_train_weight, 
                                   replace = TRUE)
      point_select_xy$x <- point_select_xy$x + sample(random_deviation, length(point_select_xy$x), replace = TRUE) * 2^(12 - split_output_layer)
      point_select_xy$y <- point_select_xy$y + sample(random_deviation, length(point_select_xy$y), replace = TRUE) * 2^(12 - split_output_layer)
      
      for (i in 1:nrow(point_select)) {
        train_array[i,,,] <- NDPI_center_RGB_pyramid_batch_Num(file_name = point_select$slide_name[i],
                                                               x_pixel = point_select_xy$x[i],
                                                               y_pixel = point_select_xy$y[i],
                                                               all_pyramid_layer = split_output_layer[i],
                                                               tilesize = input_size)
      }
    }
  }
  
  tile_chunk <- center_check_for_point_or_chunk(center_x_all = point_select_xy$x,
                                                center_y_all = point_select_xy$y,
                                                zoom_level = split_output_layer,
                                                tile_size = input_size,
                                                point_vs_chunk = "chunk")
  
  if (nrow(check_point_use_file) > 0) {
    for (k in 1:nrow(check_point_use_file)) {
      #k <- 1
      check_layer <- lapply(point_annotation, function(x){
        use_point_table <- x %>% filter(point_primary == check_point_use_file$Primary_point_class[k],
                                        point_secondary == check_point_use_file$Secondary_point_class[k],
                                        point_tertiary == check_point_use_file$Tertiary_point_class[k],
                                        point_group == paste0("P_" ,check_point_use_file$Point_number[k]))
        use_point_table$point_lon <- use_point_table$point_lon %>% as.numeric()
        use_point_table$point_lat <- use_point_table$point_lat %>% as.numeric()
        use_point_table
      })
      
      for (use_batch_size in 1:batch_size) {
        if (nrow(check_layer[[use_batch_size]] > 0)) {
          output_array[use_batch_size,
                       ,
                       ,
                       k] <- point_to_raster_function(origin_chunk = tile_chunk[[use_batch_size]],
                                                      annotation_chunk = check_layer[[use_batch_size]],
                                                      tile_size = input_size,
                                                      zoom_level = split_output_layer[use_batch_size],
                                                      point_size = check_point_use_file$Point_size[k])
        }
      }
    }
  }
  
  if (nrow(check_polygon_use_file) > 0) {
    for (k in 1:nrow(check_polygon_use_file)) {
      #k <- 1
      check_layer <- paste0(check_polygon_use_file$Primary_polygon_class[k],
                            "_",
                            check_polygon_use_file$Secondary_polygon_class[k],
                            "_",
                            check_polygon_use_file$Tertiary_polygon_class[k])
      
      for (use_batch_size in 1:batch_size) {
        if (is.null(polygon_annotation[[use_batch_size]][[1]][[check_layer]])) {
          
        } else if (length(polygon_annotation[[use_batch_size]][[1]][[check_layer]]) == 0) {
          
        } else {
          current_polygon_data <- polygon_annotation[[use_batch_size]][[3]][[check_layer]]
          new_polygon_bbox <- lawn_bbox(tile_chunk[[use_batch_size]])
          current_polygon_matrix <- matrix(data = base::c((new_polygon_bbox[1] > current_polygon_data[,1]),
                                                          (new_polygon_bbox[1] > current_polygon_data[,3]),
                                                          (new_polygon_bbox[3] > current_polygon_data[,1]),
                                                          (new_polygon_bbox[3] > current_polygon_data[,3]),
                                                          (new_polygon_bbox[2] > current_polygon_data[,2]),
                                                          (new_polygon_bbox[2] > current_polygon_data[,4]),
                                                          (new_polygon_bbox[4] > current_polygon_data[,2]),
                                                          (new_polygon_bbox[4] > current_polygon_data[,4])
          ), ncol = 8, byrow = FALSE)
          check_matrix_1 <- current_polygon_matrix[,1] + current_polygon_matrix[,2] + current_polygon_matrix[,3] + current_polygon_matrix[,4]
          check_matrix_2 <- current_polygon_matrix[,5] + current_polygon_matrix[,6] + current_polygon_matrix[,7] + current_polygon_matrix[,8]
          check_matrix_1 <- (check_matrix_1 > 0) & (check_matrix_1 < 4)
          check_matrix_2 <- (check_matrix_2 > 0) & (check_matrix_2 < 4)
          use_matrix <- which(check_matrix_1 & check_matrix_2)
          
          if (length(use_matrix) > 0) {
            overlap_region <- lawn_difference(tile_chunk[[use_batch_size]], polygon_annotation[[use_batch_size]][[1]][[check_layer]][[use_matrix[1]]])
            if (length(use_matrix) > 1) {
              for (clip_number in 2:length(use_matrix)) {
                if (length(overlap_region) != 0) {
                  overlap_region <- lawn_difference(overlap_region, polygon_annotation[[use_batch_size]][[1]][[check_layer]][[use_matrix[clip_number]]])
                }
              }
            }
            if (length(overlap_region) == 0) {
              output_array[use_batch_size,
                           ,
                           ,
                           k + nrow(check_point_use_file)] <- 1
            } else {
              overlap_chunk <- lawn_difference(tile_chunk[[use_batch_size]], overlap_region)
              output_array[use_batch_size,
                           ,
                           ,
                           k + nrow(check_point_use_file)] <- polygon_to_raster_function(origin_chunk = tile_chunk[[use_batch_size]],
                                                                                         annotation_chunk = overlap_chunk,
                                                                                         tile_size = input_size,
                                                                                         zoom_level = split_output_layer[use_batch_size])
            }
          }
        }
      }
    }
  }
  
  for (use_batch_size in 1:batch_size) {
    if (flip_flop_number[use_batch_size]) {
      train_array[use_batch_size,,,] <- train_array[use_batch_size,,,] %>% 
        flip() %>% 
        rotate(angle = rotate_number[use_batch_size])
      
      output_array[use_batch_size,,,] <- output_array[use_batch_size,,,] %>% 
        flip() %>% 
        rotate(angle = rotate_number[use_batch_size])
    } else {
      train_array[use_batch_size,,,] <- train_array[use_batch_size,,,] %>% 
        rotate(angle = rotate_number[use_batch_size])
      
      output_array[use_batch_size,,,] <- output_array[use_batch_size,,,] %>% 
        rotate(angle = rotate_number[use_batch_size])
    }
  }
  
  loop_color <- 1:(dim(train_array)[4]/3)
  
  train_array[,,,(loop_color*3 - 2)] <- (train_array[,,,(loop_color*3 - 2)] + runif(1, info_option$red_augmentation_AM[1], info_option$red_augmentation_AM[2])) * runif(1, info_option$red_augmentation[1] + 1, info_option$red_augmentation[2] + 1) + runif(1, info_option$red_augmentation_AM[1], info_option$red_augmentation_AM[2])
  train_array[,,,(loop_color*3 - 1)] <- (train_array[,,,(loop_color*3 - 1)] + runif(1, info_option$green_augmentation_AM[1], info_option$green_augmentation_AM[2])) * runif(1, info_option$green_augmentation[1] + 1, info_option$green_augmentation[2] + 1) + runif(1, info_option$green_augmentation_AM[1], info_option$green_augmentation_AM[2])
  train_array[,,,loop_color*3] <- (train_array[,,,loop_color*3] + runif(1, info_option$blue_augmentation_AM[1], info_option$blue_augmentation_AM[2])) * runif(1, info_option$blue_augmentation[1] + 1, info_option$blue_augmentation[2] + 1) + runif(1, info_option$blue_augmentation_AM[1], info_option$blue_augmentation_AM[2])
  
  train_array[train_array < 0] <- 0
  train_array[train_array > 1] <- 1
  
  list(train_array, output_array)
}

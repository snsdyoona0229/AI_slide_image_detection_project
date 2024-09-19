image_training_file_generator_function <- function(loop_jpg_file,
                                                   use_point_df,
                                                   use_polygon_df,
                                                   enhancement_point_df,
                                                   enhancement_polygon_df,
                                                   only_train_point_df,
                                                   only_train_polygon_df,
                                                   Basal_train_weight_fraction,
                                                   file_location_name,
                                                   file_location_jpg) {
  
  img_array <- readImage(loop_jpg_file)
  img_dim <- dim(img_array)
  blank_label_img_all <- array(0, c(img_dim[1:2], nrow(use_point_df) + nrow(use_polygon_df)))
  blank_enhancement_img_all <- array(0, c(img_dim[1:2], nrow(enhancement_point_df) + nrow(enhancement_polygon_df)))
  blank_only_train_img_all <- array(0, c(img_dim[1:2], nrow(only_train_point_df) + nrow(only_train_polygon_df)))
  point_annotated_data <- readRDS(paste0(loop_jpg_file %>% dirname(), "/points.rds"))
  polygon_annotated_data <- readRDS(paste0(loop_jpg_file %>% dirname(), "/polygons.rds"))
  
  if (nrow(use_point_df) > 0) {
    for (k in 1:nrow(use_point_df)) {
      blank_label_img_one <- array(0, img_dim[1:2])
      point_data_each_layer <- point_annotated_data %>% 
        filter(point_primary == use_point_df$Primary_point_class[k]) %>% 
        filter(point_secondary == use_point_df$Secondary_point_class[k]) %>% 
        filter(point_tertiary == use_point_df$Tertiary_point_class[k]) %>% 
        filter(point_group == paste0("P_", use_point_df$Point_number[k]))
      point_data_each_layer <- LonLat2XY(lon_deg = point_data_each_layer$point_lon, 
                                         lat_deg = point_data_each_layer$point_lat) %>% as.matrix()
      blank_label_img_one[point_data_each_layer] <- 1
      blank_label_img_one <- blank_label_img_one %>% dilate(kern = makeBrush(size = use_point_df$Point_size[k], shape = "disc"))
      blank_label_img_all[,,k] <- blank_label_img_one
    }
  }
  
  if (nrow(enhancement_point_df) > 0) {
    for (k in 1:nrow(enhancement_point_df)) {
      blank_enhancement_img_one <- array(0, img_dim[1:2])
      point_data_each_layer <- point_annotated_data %>% 
        filter(point_primary == enhancement_point_df$Primary_point_class[k]) %>% 
        filter(point_secondary == enhancement_point_df$Secondary_point_class[k]) %>% 
        filter(point_tertiary == enhancement_point_df$Tertiary_point_class[k]) %>% 
        filter(point_group == paste0("P_", enhancement_point_df$Point_number[k]))
      point_data_each_layer <- LonLat2XY(lon_deg = point_data_each_layer$point_lon, 
                                         lat_deg = point_data_each_layer$point_lat) %>% as.matrix()
      blank_enhancement_img_one[point_data_each_layer] <- 1
      blank_enhancement_img_one <- blank_enhancement_img_one %>% dilate(kern = makeBrush(size = enhancement_point_df$Point_size[k], shape = "disc"))
      blank_enhancement_img_all[,,k] <- blank_enhancement_img_one
    }
  }
  
  if (nrow(only_train_point_df) > 0) {
    for (k in 1:nrow(only_train_point_df)) {
      blank_only_train_img_one <- array(0, img_dim[1:2])
      point_data_each_layer <- point_annotated_data %>% 
        filter(point_primary == only_train_point_df$Primary_point_class[k]) %>% 
        filter(point_secondary == only_train_point_df$Secondary_point_class[k]) %>% 
        filter(point_tertiary == only_train_point_df$Tertiary_point_class[k]) %>% 
        filter(point_group == paste0("P_", only_train_point_df$Point_number[k]))
      point_data_each_layer <- LonLat2XY(lon_deg = point_data_each_layer$point_lon, 
                                         lat_deg = point_data_each_layer$point_lat) %>% as.matrix()
      blank_only_train_img_one[point_data_each_layer] <- 1
      blank_only_train_img_one <- blank_only_train_img_one %>% dilate(kern = makeBrush(size = only_train_point_df$Point_size[k], shape = "disc"))
      blank_only_train_img_all[,,k] <- blank_only_train_img_one
    }
  }
  
  if (nrow(use_polygon_df) > 0) {
    for (k in 1:nrow(use_polygon_df)) {
      polygon_data_each_layer <- paste0(use_polygon_df$Primary_polygon_class[k],
                                        "_",
                                        use_polygon_df$Secondary_polygon_class[k],
                                        "_",
                                        use_polygon_df$Tertiary_polygon_class[k])
      polygons <- polygon_annotated_data[[polygon_data_each_layer]]
      polygon_number <- length(polygons)
      
      #print(polygon_number)
      
      if (polygon_number > 0) {
        for (j in 1:polygon_number) {
          polygon_length <- length(polygons[[j]]$geometry$coordinates[[1]])
          if (polygons[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygons[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
            polygons[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygons[[j]]$geometry$coordinates[[1]][[1]]
          }
          if ((polygons[[j]] %>% lawn_kinks())$features %>% length() != 0) {
            all_poly <- polygons[[j]] %>% lawn_coordall()
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
            polygons[[j]]$geometry$coordinates <- list(choose_poly)
          }
          polygon_length <- length(polygons[[j]]$geometry$coordinates[[1]])
          if (polygons[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygons[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
            polygons[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygons[[j]]$geometry$coordinates[[1]][[1]]
          }
          if (j == 1) {
            polygon_u <- polygons[[1]]
          } else {
            polygon_u <- lawn_union(polygon_u, polygons[[j]])
          }
        }
        
        img_arr <- array(0, img_dim[1:2])
        image_dim <- XY2LonLat(img_dim[1], img_dim[2])
        zero_dim <- XY2LonLat(0, 0)
        
        image_polygon <- lawn_polygon(list(list(
          c(zero_dim %>% as.numeric()),
          c((zero_dim %>% as.numeric())[1],(image_dim %>% as.numeric())[2]),
          c(image_dim %>% as.numeric()),
          c((image_dim %>% as.numeric())[1], (zero_dim %>% as.numeric())[2]),
          c(zero_dim %>% as.numeric())
        )))
        
        
        #polygon_u <- lawn_intersect(image_polygon, polygon_u)
        
        if (class(polygon_u) == "polygon") {
          df <- LonLat2XY(lon_deg = polygon_u$geometry$coordinates[1,,1], lat_deg = polygon_u$geometry$coordinates[1,,2])
          df$x <- replace(df$x, which(df$x <= 0), 1)
          df$x <- replace(df$x, which(df$x >= img_dim[1]), img_dim[1])
          df$y <- replace(df$y, which(df$y <= 0), 1)
          df$y <- replace(df$y, which(df$y >= img_dim[2]), img_dim[2])
          rdf <- list()
          if (dim(polygon_u$geometry$coordinates)[1] != 1) {
            for (i in 2:dim(polygon_u$geometry$coordinates)[1]) {
              rdf[[i-1]] <- LonLat2XY(lon_deg = x[i,,1], lat_deg = x[i,,2])
              rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
              rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= img_dim[1]), img_dim[1])
              rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
              rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= img_dim[2]), img_dim[2])
            }
          }
          matrix_polygon <- list(list(df, rdf))
        } else {
          matrix_polygon <- lapply(polygon_u$geometry$coordinates, function(x){
            if (class(x) == "list") {
              df <- LonLat2XY(lon_deg = x[[1]][,1], lat_deg = x[[1]][,2])
              df$x <- replace(df$x, which(df$x <= 0), 1)
              df$x <- replace(df$x, which(df$x >= img_dim[1]), img_dim[1])
              df$y <- replace(df$y, which(df$y <= 0), 1)
              df$y <- replace(df$y, which(df$y >= img_dim[2]), img_dim[2])
              rdf <- list()
              for (i in 2:length(x)) {
                rdf[[i-1]] <- LonLat2XY(lon_deg = x[[i]][,1], lat_deg = x[[i]][,2])
                rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
                rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= img_dim[1]), img_dim[1])
                rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
                rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= img_dim[2]), img_dim[2])
              }
              
            } else if (class(x) == "array") {
              df <- LonLat2XY(lon_deg = x[1,,1], lat_deg = x[1,,2])
              df$x <- replace(df$x, which(df$x <= 0), 1)
              df$x <- replace(df$x, which(df$x >= img_dim[1]), img_dim[1])
              df$y <- replace(df$y, which(df$y <= 0), 1)
              df$y <- replace(df$y, which(df$y >= img_dim[2]), img_dim[2])
              rdf <- list()
              if (dim(x)[1] != 1) {
                for (i in 2:dim(x)[1]) {
                  rdf[[i-1]] <- LonLat2XY(lon_deg = x[i,,1], lat_deg = x[i,,2])
                  rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
                  rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= img_dim[1]), img_dim[1])
                  rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
                  rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= img_dim[2]), img_dim[2])
                }
              }
            }
            list(df, rdf)
          })
        }
        
        for (kk in 1:length(matrix_polygon)) {
          for (ii in 2:nrow(matrix_polygon[[kk]][[1]])) {
            point_draw <- point_line_function(c(matrix_polygon[[kk]][[1]][ii-1,1], matrix_polygon[[kk]][[1]][ii-1,2]),
                                              c(matrix_polygon[[kk]][[1]][ii,1], matrix_polygon[[kk]][[1]][ii,2]))
            img_arr[point_draw %>% as.matrix()] <- 1
          }
          if (length(matrix_polygon[[kk]][[2]]) > 0) {
            for (jj in 1:length(matrix_polygon[[kk]][[2]])) {
              for (ii in 2:nrow(matrix_polygon[[kk]][[2]][[jj]])) {
                point_draw <- point_line_function(c(matrix_polygon[[kk]][[2]][[jj]][ii-1,1], matrix_polygon[[kk]][[2]][[jj]][ii-1,2]),
                                                  c(matrix_polygon[[kk]][[2]][[jj]][ii,1], matrix_polygon[[kk]][[2]][[jj]][ii,2]))
                img_arr[point_draw %>% as.matrix()] <- -1
              }
            }
          }
        }
        
        chunk_img <- img_arr %>% replace(which(img_arr == -1), 0) %>% fillHull()
        rm_chunk_img <- img_arr*-1
        rm_chunk_img <- rm_chunk_img %>% replace(which(rm_chunk_img == -1), 0) %>% fillHull()
        img_arr <- chunk_img - rm_chunk_img
        
        blank_label_img_all[,,nrow(use_point_df) + k] <- img_arr
      }
    }
  }
  
  if (nrow(enhancement_polygon_df) > 0) {
    for (k in 1:nrow(enhancement_polygon_df)) {
      polygon_data_each_layer <- paste0(enhancement_polygon_df$Primary_polygon_class[k],
                                        "_",
                                        enhancement_polygon_df$Secondary_polygon_class[k],
                                        "_",
                                        enhancement_polygon_df$Tertiary_polygon_class[k])
      polygons <- polygon_annotated_data[[polygon_data_each_layer]]
      polygon_number <- length(polygons)
      
      if (polygon_number > 0) {
        for (j in 1:polygon_number) {
          polygon_length <- length(polygons[[j]]$geometry$coordinates[[1]])
          if (polygons[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygons[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
            polygons[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygons[[j]]$geometry$coordinates[[1]][[1]]
          }
          if ((polygons[[j]] %>% lawn_kinks())$features %>% length() != 0) {
            all_poly <- polygons[[j]] %>% lawn_coordall()
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
            polygons[[j]]$geometry$coordinates <- list(choose_poly)
          }
          polygon_length <- length(polygons[[j]]$geometry$coordinates[[1]])
          if (polygons[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygons[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
            polygons[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygons[[j]]$geometry$coordinates[[1]][[1]]
          }
          if (j == 1) {
            polygon_u <- polygons[[1]]
          } else {
            polygon_u <- lawn_union(polygon_u, polygons[[j]])
          }
        }
        
        img_arr <- array(0, img_dim[1:2])
        image_dim <- XY2LonLat(img_dim[1], img_dim[2])
        zero_dim <- XY2LonLat(0, 0)
        
        image_polygon <- lawn_polygon(list(list(
          c(zero_dim %>% as.numeric()),
          c((zero_dim %>% as.numeric())[1],(image_dim %>% as.numeric())[2]),
          c(image_dim %>% as.numeric()),
          c((image_dim %>% as.numeric())[1], (zero_dim %>% as.numeric())[2]),
          c(zero_dim %>% as.numeric())
        )))
        
        #polygon_u <- lawn_intersect(image_polygon, polygon_u)
        
        if (class(polygon_u) == "polygon") {
          df <- LonLat2XY(lon_deg = polygon_u$geometry$coordinates[1,,1], lat_deg = polygon_u$geometry$coordinates[1,,2])
          df$x <- replace(df$x, which(df$x <= 0), 1)
          df$x <- replace(df$x, which(df$x >= img_dim[1]), img_dim[1])
          df$y <- replace(df$y, which(df$y <= 0), 1)
          df$y <- replace(df$y, which(df$y >= img_dim[2]), img_dim[2])
          rdf <- list()
          if (dim(polygon_u$geometry$coordinates)[1] != 1) {
            for (i in 2:dim(polygon_u$geometry$coordinates)[1]) {
              rdf[[i-1]] <- LonLat2XY(lon_deg = x[i,,1], lat_deg = x[i,,2])
              rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
              rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= img_dim[1]), img_dim[1])
              rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
              rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= img_dim[2]), img_dim[2])
            }
          }
          matrix_polygon <- list(list(df, rdf))
        } else {
          matrix_polygon <- lapply(polygon_u$geometry$coordinates, function(x){
            if (class(x) == "list") {
              df <- LonLat2XY(lon_deg = x[[1]][,1], lat_deg = x[[1]][,2])
              df$x <- replace(df$x, which(df$x <= 0), 1)
              df$x <- replace(df$x, which(df$x >= img_dim[1]), img_dim[1])
              df$y <- replace(df$y, which(df$y <= 0), 1)
              df$y <- replace(df$y, which(df$y >= img_dim[2]), img_dim[2])
              rdf <- list()
              for (i in 2:length(x)) {
                rdf[[i-1]] <- LonLat2XY(lon_deg = x[[i]][,1], lat_deg = x[[i]][,2])
                rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
                rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= img_dim[1]), img_dim[1])
                rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
                rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= img_dim[2]), img_dim[2])
              }
              
            } else if (class(x) == "array") {
              df <- LonLat2XY(lon_deg = x[1,,1], lat_deg = x[1,,2])
              df$x <- replace(df$x, which(df$x <= 0), 1)
              df$x <- replace(df$x, which(df$x >= img_dim[1]), img_dim[1])
              df$y <- replace(df$y, which(df$y <= 0), 1)
              df$y <- replace(df$y, which(df$y >= img_dim[2]), img_dim[2])
              rdf <- list()
              if (dim(x)[1] != 1) {
                for (i in 2:dim(x)[1]) {
                  rdf[[i-1]] <- LonLat2XY(lon_deg = x[i,,1], lat_deg = x[i,,2])
                  rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
                  rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= img_dim[1]), img_dim[1])
                  rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
                  rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= img_dim[2]), img_dim[2])
                }
              }
            }
            list(df, rdf)
          })
        }
        
        #saveRDS(matrix_polygon, "test.rds")
        
        
        for (kk in 1:length(matrix_polygon)) {
          for (ii in 2:nrow(matrix_polygon[[kk]][[1]])) {
            point_draw <- point_line_function(c(matrix_polygon[[kk]][[1]][ii-1,1], matrix_polygon[[kk]][[1]][ii-1,2]),
                                              c(matrix_polygon[[kk]][[1]][ii,1], matrix_polygon[[kk]][[1]][ii,2]))
            img_arr[point_draw %>% as.matrix()] <- 1
          }
          if (length(matrix_polygon[[kk]][[2]]) > 0) {
            for (jj in 1:length(matrix_polygon[[kk]][[2]])) {
              for (ii in 2:nrow(matrix_polygon[[kk]][[2]][[jj]])) {
                point_draw <- point_line_function(c(matrix_polygon[[kk]][[2]][[jj]][ii-1,1], matrix_polygon[[kk]][[2]][[jj]][ii-1,2]),
                                                  c(matrix_polygon[[kk]][[2]][[jj]][ii,1], matrix_polygon[[kk]][[2]][[jj]][ii,2]))
                img_arr[point_draw %>% as.matrix()] <- -1
              }
            }
          }
        }
        
        chunk_img <- img_arr %>% replace(which(img_arr == -1), 0) %>% fillHull()
        rm_chunk_img <- img_arr*-1
        rm_chunk_img <- rm_chunk_img %>% replace(which(rm_chunk_img == -1), 0) %>% fillHull()
        img_arr <- chunk_img - rm_chunk_img
        
        blank_enhancement_img_all[,,nrow(enhancement_point_df) + k] <- img_arr
      }
    }
  }
  
  if (nrow(only_train_polygon_df) > 0) {
    for (k in 1:nrow(only_train_polygon_df)) {
      polygon_data_each_layer <- paste0(only_train_polygon_df$Primary_polygon_class[k],
                                        "_",
                                        only_train_polygon_df$Secondary_polygon_class[k],
                                        "_",
                                        only_train_polygon_df$Tertiary_polygon_class[k])
      polygons <- polygon_annotated_data[[polygon_data_each_layer]]
      polygon_number <- length(polygons)
      
      if (polygon_number > 0) {
        for (j in 1:polygon_number) {
          polygon_length <- length(polygons[[j]]$geometry$coordinates[[1]])
          if (polygons[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygons[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
            polygons[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygons[[j]]$geometry$coordinates[[1]][[1]]
          }
          if ((polygons[[j]] %>% lawn_kinks())$features %>% length() != 0) {
            all_poly <- polygons[[j]] %>% lawn_coordall()
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
            polygons[[j]]$geometry$coordinates <- list(choose_poly)
          }
          polygon_length <- length(polygons[[j]]$geometry$coordinates[[1]])
          if (polygons[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygons[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
            polygons[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygons[[j]]$geometry$coordinates[[1]][[1]]
          }
          if (j == 1) {
            polygon_u <- polygons[[1]]
          } else {
            polygon_u <- lawn_union(polygon_u, polygons[[j]])
          }
        }
        
        img_arr <- array(0, img_dim[1:2])
        image_dim <- XY2LonLat(img_dim[1], img_dim[2])
        zero_dim <- XY2LonLat(0, 0)
        
        image_polygon <- lawn_polygon(list(list(
          c(zero_dim %>% as.numeric()),
          c((zero_dim %>% as.numeric())[1],(image_dim %>% as.numeric())[2]),
          c(image_dim %>% as.numeric()),
          c((image_dim %>% as.numeric())[1], (zero_dim %>% as.numeric())[2]),
          c(zero_dim %>% as.numeric())
        )))
        
        #polygon_u <- lawn_intersect(image_polygon, polygon_u)
        
        if (class(polygon_u) == "polygon") {
          df <- LonLat2XY(lon_deg = polygon_u$geometry$coordinates[1,,1], lat_deg = polygon_u$geometry$coordinates[1,,2])
          df$x <- replace(df$x, which(df$x <= 0), 1)
          df$x <- replace(df$x, which(df$x >= img_dim[1]), img_dim[1])
          df$y <- replace(df$y, which(df$y <= 0), 1)
          df$y <- replace(df$y, which(df$y >= img_dim[2]), img_dim[2])
          rdf <- list()
          if (dim(polygon_u$geometry$coordinates)[1] != 1) {
            for (i in 2:dim(polygon_u$geometry$coordinates)[1]) {
              rdf[[i-1]] <- LonLat2XY(lon_deg = x[i,,1], lat_deg = x[i,,2])
              rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
              rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= img_dim[1]), img_dim[1])
              rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
              rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= img_dim[2]), img_dim[2])
            }
          }
          matrix_polygon <- list(list(df, rdf))
        } else {
          matrix_polygon <- lapply(polygon_u$geometry$coordinates, function(x){
            if (class(x) == "list") {
              df <- LonLat2XY(lon_deg = x[[1]][,1], lat_deg = x[[1]][,2])
              df$x <- replace(df$x, which(df$x <= 0), 1)
              df$x <- replace(df$x, which(df$x >= img_dim[1]), img_dim[1])
              df$y <- replace(df$y, which(df$y <= 0), 1)
              df$y <- replace(df$y, which(df$y >= img_dim[2]), img_dim[2])
              rdf <- list()
              for (i in 2:length(x)) {
                rdf[[i-1]] <- LonLat2XY(lon_deg = x[[i]][,1], lat_deg = x[[i]][,2])
                rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
                rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= img_dim[1]), img_dim[1])
                rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
                rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= img_dim[2]), img_dim[2])
              }
              
            } else if (class(x) == "array") {
              df <- LonLat2XY(lon_deg = x[1,,1], lat_deg = x[1,,2])
              df$x <- replace(df$x, which(df$x <= 0), 1)
              df$x <- replace(df$x, which(df$x >= img_dim[1]), img_dim[1])
              df$y <- replace(df$y, which(df$y <= 0), 1)
              df$y <- replace(df$y, which(df$y >= img_dim[2]), img_dim[2])
              rdf <- list()
              if (dim(x)[1] != 1) {
                for (i in 2:dim(x)[1]) {
                  rdf[[i-1]] <- LonLat2XY(lon_deg = x[i,,1], lat_deg = x[i,,2])
                  rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
                  rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= img_dim[1]), img_dim[1])
                  rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
                  rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= img_dim[2]), img_dim[2])
                }
              }
            }
            list(df, rdf)
          })
        }
        
        for (kk in 1:length(matrix_polygon)) {
          for (ii in 2:nrow(matrix_polygon[[kk]][[1]])) {
            point_draw <- point_line_function(c(matrix_polygon[[kk]][[1]][ii-1,1], matrix_polygon[[kk]][[1]][ii-1,2]),
                                              c(matrix_polygon[[kk]][[1]][ii,1], matrix_polygon[[kk]][[1]][ii,2]))
            img_arr[point_draw %>% as.matrix()] <- 1
          }
          if (length(matrix_polygon[[kk]][[2]]) > 0) {
            for (jj in 1:length(matrix_polygon[[kk]][[2]])) {
              for (ii in 2:nrow(matrix_polygon[[kk]][[2]][[jj]])) {
                point_draw <- point_line_function(c(matrix_polygon[[kk]][[2]][[jj]][ii-1,1], matrix_polygon[[kk]][[2]][[jj]][ii-1,2]),
                                                  c(matrix_polygon[[kk]][[2]][[jj]][ii,1], matrix_polygon[[kk]][[2]][[jj]][ii,2]))
                img_arr[point_draw %>% as.matrix()] <- -1
              }
            }
          }
        }
        
        chunk_img <- img_arr %>% replace(which(img_arr == -1), 0) %>% fillHull()
        rm_chunk_img <- img_arr*-1
        rm_chunk_img <- rm_chunk_img %>% replace(which(rm_chunk_img == -1), 0) %>% fillHull()
        img_arr <- chunk_img - rm_chunk_img
        
        blank_only_train_img_all[,,nrow(only_train_point_df) + k] <- img_arr
      }
    }
  }
  
  weight_map <- array(1, dim = dim(blank_label_img_all))
  fraction_vector <- c(use_point_df$Select_to_train_weight_fraction,
                       use_polygon_df$Select_to_train_weight_fraction,
                       enhancement_point_df$Select_to_train_weight_fraction,
                       enhancement_polygon_df$Select_to_train_weight_fraction,
                       only_train_point_df$Select_to_train_weight_fraction,
                       only_train_polygon_df$Select_to_train_weight_fraction,
                       Basal_train_weight_fraction
  ) %>% as.numeric()
  
  if (nrow(only_train_point_df) > 0 | nrow(only_train_polygon_df) > 0) {
    only_train_map <- rowSums(blank_only_train_img_all, dims = 2)
    only_train_map[which(only_train_map > 1, arr.ind = TRUE)] <- 1
    
    blank_label_img_all_s <- lapply(1:dim(blank_label_img_all)[3], function(x){blank_label_img_all[,,x] * only_train_map}) %>% 
      abind(along = 3) %>% 
      unname()
    blank_enhancement_img_all <- lapply(1:dim(blank_enhancement_img_all)[3], function(x){blank_enhancement_img_all[,,x] * only_train_map}) %>% 
      abind(along = 3) %>% 
      unname()
    blank_background_img_all <- only_train_map
  } else {
    blank_label_img_all_s <- blank_label_img_all
    blank_background_img_all <- array(1, dim = c(dim(blank_label_img_all)[1:2], 1))
  }
  
  all_train_loop_map <- abind::abind(blank_label_img_all_s, 
                                     blank_enhancement_img_all, 
                                     blank_only_train_img_all, 
                                     blank_background_img_all, along = 3) %>% unname()
  
  all_train_loop_array_data <- lapply(1:dim(all_train_loop_map)[3], function(x) {
    which(all_train_loop_map[,,x] == 1, arr.ind = TRUE)
  })
  
  check_loop_array_zero <- sapply(all_train_loop_array_data, function(x){
    nrow(x)
  })
  
  fraction_vector[check_loop_array_zero == 0] <- 0
  
  saveRDS(list(img_array, 
               blank_label_img_all, 
               img_dim[1], 
               img_dim[2], 
               weight_map,
               #blank_enhancement_img_all,
               #blank_only_train_img_all,
               fraction_vector,
               all_train_loop_array_data), 
          paste0(file_location_name, "/", img_dim[1] * img_dim[2], "_", file_location_jpg %>% str_replace_all(".jpg", ""), ".rds"))
  
}

thumb_restricted_region_function <- function(slide_file,
                                             polygon_layer_select) {
  polygon_annotated_data <- readRDS(paste0(slide_file %>% dirname(), "/polygons.rds"))
  polygons <- polygon_annotated_data[[polygon_layer_select]]
  polygon_number <- length(polygons)
  
  if (polygon_number > 0) {
    for (j in 1:polygon_number) {
      polygon_length <- length(polygons[[j]]$geometry$coordinates[[1]])
      if (polygons[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygons[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
        polygons[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygons[[j]]$geometry$coordinates[[1]][[1]]
      }
      if ((polygons[[j]] %>% lawn_kinks())$features %>% length() != 0) {
        all_poly <- polygons[[j]] %>% lawn_coordall()
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
        polygons[[j]]$geometry$coordinates <- list(choose_poly)
      }
      polygon_length <- length(polygons[[j]]$geometry$coordinates[[1]])
      if (polygons[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygons[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
        polygons[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygons[[j]]$geometry$coordinates[[1]][[1]]
      }
      if (j == 1) {
        polygon_u <- polygons[[1]]
      } else {
        polygon_u <- lawn_union(polygon_u, polygons[[j]])
      }
    }
    
    img_arr <- array(0, c(2048,2048))
    image_dim <- XY2LonLat(2048, 2048, zoom = 6)
    zero_dim <- XY2LonLat(0, 0, zoom = 6)
    
    image_polygon <- lawn_polygon(list(list(
      c(zero_dim %>% as.numeric()),
      c((zero_dim %>% as.numeric())[1],(image_dim %>% as.numeric())[2]),
      c(image_dim %>% as.numeric()),
      c((image_dim %>% as.numeric())[1], (zero_dim %>% as.numeric())[2]),
      c(zero_dim %>% as.numeric())
    )))
    
    if (class(polygon_u) == "polygon") {
      df <- LonLat2XY(lon_deg = polygon_u$geometry$coordinates[1,,1], lat_deg = polygon_u$geometry$coordinates[1,,2], zoom = 6)
      df$x <- replace(df$x, which(df$x <= 0), 1)
      df$x <- replace(df$x, which(df$x >= 2048), 2048)
      df$y <- replace(df$y, which(df$y <= 0), 1)
      df$y <- replace(df$y, which(df$y >= 2048), 2048)
      rdf <- list()
      if (dim(polygon_u$geometry$coordinates)[1] != 1) {
        for (i in 2:dim(polygon_u$geometry$coordinates)[1]) {
          rdf[[i-1]] <- LonLat2XY(lon_deg = x[i,,1], lat_deg = x[i,,2], zoom = 6)
          rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
          rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= 2048), 2048)
          rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
          rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= 2048), 2048)
        }
      }
      matrix_polygon <- list(list(df, rdf))
    } else {
      matrix_polygon <- lapply(polygon_u$geometry$coordinates, function(x){
        if (class(x) == "list") {
          df <- LonLat2XY(lon_deg = x[[1]][,1], lat_deg = x[[1]][,2], zoom = 6)
          df$x <- replace(df$x, which(df$x <= 0), 1)
          df$x <- replace(df$x, which(df$x >= 2048), 2048)
          df$y <- replace(df$y, which(df$y <= 0), 1)
          df$y <- replace(df$y, which(df$y >= 2048), 2048)
          rdf <- list()
          for (i in 2:length(x)) {
            rdf[[i-1]] <- LonLat2XY(lon_deg = x[[i]][,1], lat_deg = x[[i]][,2], zoom = 6)
            rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
            rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= 2048), 2048)
            rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
            rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= 2048), 2048)
          }
          
        } else if (class(x) == "array") {
          df <- LonLat2XY(lon_deg = x[1,,1], lat_deg = x[1,,2], zoom = 6)
          df$x <- replace(df$x, which(df$x <= 0), 1)
          df$x <- replace(df$x, which(df$x >= 2048), 2048)
          df$y <- replace(df$y, which(df$y <= 0), 1)
          df$y <- replace(df$y, which(df$y >= 2048), 2048)
          rdf <- list()
          if (dim(x)[1] != 1) {
            for (i in 2:dim(x)[1]) {
              rdf[[i-1]] <- LonLat2XY(lon_deg = x[i,,1], lat_deg = x[i,,2], zoom = 6)
              rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
              rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= 2048), 2048)
              rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
              rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= 2048), 2048)
            }
          }
        }
        list(df, rdf)
      })
    }
    
    for (kk in 1:length(matrix_polygon)) {
      for (ii in 2:nrow(matrix_polygon[[kk]][[1]])) {
        point_draw <- point_line_function(c(matrix_polygon[[kk]][[1]][ii-1,1], matrix_polygon[[kk]][[1]][ii-1,2]),
                                          c(matrix_polygon[[kk]][[1]][ii,1], matrix_polygon[[kk]][[1]][ii,2]))
        img_arr[point_draw %>% as.matrix()] <- 1
      }
      if (length(matrix_polygon[[kk]][[2]]) > 0) {
        for (jj in 1:length(matrix_polygon[[kk]][[2]])) {
          for (ii in 2:nrow(matrix_polygon[[kk]][[2]][[jj]])) {
            point_draw <- point_line_function(c(matrix_polygon[[kk]][[2]][[jj]][ii-1,1], matrix_polygon[[kk]][[2]][[jj]][ii-1,2]),
                                              c(matrix_polygon[[kk]][[2]][[jj]][ii,1], matrix_polygon[[kk]][[2]][[jj]][ii,2]))
            img_arr[point_draw %>% as.matrix()] <- -1
          }
        }
      }
    }
    
    chunk_img <- img_arr %>% replace(which(img_arr == -1), 0) %>% fillHull()
    rm_chunk_img <- img_arr*-1
    rm_chunk_img <- rm_chunk_img %>% replace(which(rm_chunk_img == -1), 0) %>% fillHull()
    img_arr <- chunk_img - rm_chunk_img
    #blank_label_img_all[,,nrow(use_point_df) + k] <- img_arr
    return(img_arr)
  }
}
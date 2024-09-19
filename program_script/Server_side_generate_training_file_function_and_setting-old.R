observeEvent(input$Training_Slide_or_image, {
  updateSelectInput(session = session, inputId = "Training_Annotation_group", choices = list.files(paste0("www/", input$Training_Slide_or_image, "/")) %>% str_sort(numeric = TRUE))
})

observeEvent(input$Choose_file_training_table, {
  updateSelectInput(session = session, inputId = "Preview_point_image", choices = input$Choose_file_training_table)
  updateSelectInput(session = session, inputId = "Preview_polygon_image", choices = input$Choose_file_training_table)
  generate_file_fraction_df$df <- data.frame(
    row.names = input$Choose_file_training_table,
    sample_number = rep(1000,length(input$Choose_file_training_table)),
    custom_fraction = rep(1,length(input$Choose_file_training_table)),
    slide_size_fraction = rep(NA,length(input$Choose_file_training_table)),
    tissue_size_fraction = rep(NA,length(input$Choose_file_training_table))
  )

  #print(input$Choose_file_training_table)
})


observeEvent(input$Preview_polygon_weighted_map, {
  if (length(input$Preview_polygon_image) > 0) {
    file_location <- paste0("www/Image/", input$Training_Annotation_group, "/", input$Preview_point_image)
    file_location_jpg <- sapply(file_location, function(x){list.files(x, pattern = ".jpg")})
    loop_jpg_file <- paste0(file_location, "/", file_location_jpg)
    
    use_polygon_df <- as.data.frame(hot_to_r(input$polygon_label_on_train_table)) %>% filter(Use_for_train)
    img_array <- readImage(loop_jpg_file)
    img_dim <- dim(img_array)
    blank_label_img_all <- array(0, c(img_dim[1:2], nrow(use_polygon_df)))
    polygon_annotated_data <- readRDS(paste0(loop_jpg_file %>% dirname(), "/polygons.rds"))
    
    if (nrow(use_polygon_df) > 0) {
      for (k in 1:nrow(use_polygon_df)) {
        polygon_data_each_layer <- paste0(use_polygon_df$Primary_polygon_class[k],
                                          "_",
                                          use_polygon_df$Secondary_polygon_class[k],
                                          "_",
                                          use_polygon_df$Tertiary_polygon_class[k])
        polygons <- polygon_annotated_data[[polygon_data_each_layer]]
        polygon_number <- length(polygons)
        
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
        
        blank_label_img_all[,,k] <- img_arr
        
      }
      
      b_run_tensor <- tf$constant(value = 0, 
                                  shape = list(1 %>% as.integer(), 
                                               dim(blank_label_img_all)[1] %>% as.integer(), 
                                               dim(blank_label_img_all)[2] %>% as.integer(), 
                                               dim(blank_label_img_all)[3] %>% as.integer()), 
                                  dtype = "float32")
      
      max_data <- b_run_tensor %>% 
        tf$shape() %>% 
        tf$slice(begin = tf$constant(array(data = 3), 
                                     dtype = "int32"), 
                 size = tf$constant(array(data = 1), 
                                    dtype = "int32"))
      
      plus_data <- tf$constant(array(data = 1), 
                               dtype = "int32")
      
      pre_begin_tensor <- tf$zeros(shape = list(3 %>% as.integer()), 
                                   dtype = "int32")
      
      outer_use <- list()
      outer_cent_use <- list()
      outer_size <- list()
      outer_basal_weight <- list()
      outer_dot <- list()
      outer_bias <- list()
      inner_use <- list()
      inner_cent_use <- list()
      inner_size <- list()
      inner_basal_weight <- list()
      inner_dot <- list()
      inner_bias <- list()
      
      
      for (i in 1:nrow(use_polygon_df)) {
        if (use_polygon_df$Outer_weight_train[i] == "N") {
          outer_use[[i]] <- 0
          outer_cent_use[[i]] <- 0
          outer_size[[i]] <- 10
          outer_basal_weight[[i]] <- use_polygon_df$Outer_non_labeled_object_basal_weight[i]
          outer_dot[[i]] <- 1
          outer_bias[[i]] <- 0
        } else if (use_polygon_df$Outer_weight_train[i] == "C") {
          outer_use[[i]] <- 1
          outer_cent_use[[i]] <- 1
          outer_size[[i]] <- use_polygon_df$Outer_central_size[i]
          outer_basal_weight[[i]] <- use_polygon_df$Outer_non_labeled_object_basal_weight[i]
          outer_dot[[i]] <- use_polygon_df$Outer_central_dot[i]
          outer_bias[[i]] <- use_polygon_df$Outer_central_bias[i]
        } else if (use_polygon_df$Outer_weight_train[i] == "B") {
          outer_use[[i]] <- 1
          outer_cent_use[[i]] <- 0
          outer_size[[i]] <- use_polygon_df$Outer_boundary_size[i]
          outer_basal_weight[[i]] <- use_polygon_df$Outer_non_labeled_object_basal_weight[i]
          outer_dot[[i]] <- use_polygon_df$Outer_boundary_dot[i]
          outer_bias[[i]] <- use_polygon_df$Outer_boundary_bias[i]
        }
        
        if (use_polygon_df$Inner_weight_train[i] == "N") {
          inner_use[[i]] <- 0
          inner_cent_use[[i]] <- 0
          inner_size[[i]] <- 10
          inner_basal_weight[[i]] <- use_polygon_df$Inner_labeled_object_basal_weight[i]
          inner_dot[[i]] <- 1
          inner_bias[[i]] <- 0
        } else if (use_polygon_df$Inner_weight_train[i] == "C") {
          inner_use[[i]] <- 1
          inner_cent_use[[i]] <- 1
          inner_size[[i]] <- use_polygon_df$Inner_central_size[i]
          inner_basal_weight[[i]] <- use_polygon_df$Inner_labeled_object_basal_weight[i]
          inner_dot[[i]] <- use_polygon_df$Inner_central_dot[i]
          inner_bias[[i]] <- use_polygon_df$Inner_central_bias[i]
        } else if (use_polygon_df$Inner_weight_train[i] == "B") {
          inner_use[[i]] <- 1
          inner_cent_use[[i]] <- 0
          inner_size[[i]] <- use_polygon_df$Inner_boundary_size[i]
          inner_basal_weight[[i]] <- use_polygon_df$Inner_labeled_object_basal_weight[i]
          inner_dot[[i]] <- use_polygon_df$Inner_boundary_dot[i]
          inner_bias[[i]] <- use_polygon_df$Inner_boundary_bias[i]
        }
      }
      
      outer_use <- tf$cast(outer_use, dtype = "float32")
      outer_cent_use <- tf$cast(outer_cent_use, dtype = "float32")
      outer_size <- tf$cast(outer_size, dtype = "int32")
      outer_basal_weight <- tf$cast(outer_basal_weight, dtype = "float32")
      outer_dot <- tf$cast(outer_dot, dtype = "float32")
      outer_bias <- tf$cast(outer_bias, dtype = "float32")
      
      inner_use <- tf$cast(inner_use, dtype = "float32")
      inner_cent_use <- tf$cast(inner_cent_use, dtype = "float32")
      inner_size <- tf$cast(inner_size, dtype = "int32")
      inner_basal_weight <- tf$cast(inner_basal_weight, dtype = "float32")
      inner_dot <- tf$cast(inner_dot, dtype = "float32")
      inner_bias <- tf$cast(inner_bias, dtype = "float32")
      
      i <- tf$constant(array(data = 0), dtype = "int32")
      c <- function(i, run_tensor, outer_dot, outer_bias, inner_dot, inner_bias, outer_use, outer_cent_use, outer_size, outer_basal_weight, inner_use, inner_cent_use, inner_size, inner_basal_weight) {tf$less(i, max_data)}
      b <- function(i, run_tensor, outer_dot, outer_bias, inner_dot, inner_bias, outer_use, outer_cent_use, outer_size, outer_basal_weight, inner_use, inner_cent_use, inner_size, inner_basal_weight) {
        check_tensor <- tf$concat(list(run_tensor %>% 
                                         tf$shape() %>% 
                                         tf$slice(begin = tf$constant(array(data = 0), 
                                                                      dtype = "int32"), 
                                                  size = tf$constant(array(data = 3), 
                                                                     dtype = "int32")),
                                       tf$ones(shape = 1 %>% as.integer(), 
                                               dtype = "int32")), 
                                  axis = as.integer(0))
        
        begin_value <- tf$concat(list(pre_begin_tensor, i), axis = as.integer(0))
        
        slide_1 <- tf$cast(list(1), dtype = "int32")
        
        begin_i <- i %>% 
          tf$reshape(shape = slide_1)
        
        inner_array <- tf$slice(run_tensor, 
                                begin = begin_value, 
                                size = check_tensor)
        
        outter_array <- 1 - inner_array
        
        outter_kernel_value <- tf$concat(list(tf$slice(outer_size, 
                                                       begin = begin_i, 
                                                       size = slide_1),
                                              tf$slice(outer_size, 
                                                       begin = begin_i, 
                                                       size = slide_1),
                                              tf$ones(shape = list(2 %>% as.integer()), 
                                                      dtype = "int32")), 
                                         axis = as.integer(0))
        
        inner_kernel_value <- tf$concat(list(tf$slice(inner_size, 
                                                      begin = begin_i, 
                                                      size = slide_1),
                                             tf$slice(inner_size, 
                                                      begin = begin_i, 
                                                      size = slide_1),
                                             tf$ones(shape = list(2 %>% as.integer()), 
                                                     dtype = "int32")), 
                                        axis = as.integer(0))
        
        outer_center <- tf$sqrt(k_conv2d(outter_array, 
                                         kernel = tf$ones(shape = outter_kernel_value, 
                                                          dtype = "float32"), 
                                         padding = "same", 
                                         strides = 1, 
                                         dilation_rate = 1)) * tf$slice(outer_dot, 
                                                                        begin = begin_i, 
                                                                        size = slide_1) + tf$slice(outer_bias, 
                                                                                                   begin = begin_i, 
                                                                                                   size = slide_1)
        
        outer_center <- outer_center * outter_array * tf$slice(outer_cent_use, 
                                                               begin = begin_i, 
                                                               size = slide_1) * tf$slice(outer_use, 
                                                                                          begin = begin_i, 
                                                                                          size = slide_1) 
        
        outer_border <- tf$sqrt(k_conv2d(inner_array, 
                                         kernel = tf$ones(shape = outter_kernel_value, 
                                                          dtype = "float32"), 
                                         padding = "same", 
                                         strides = 1, 
                                         dilation_rate = 1)) * tf$slice(outer_dot, 
                                                                        begin = begin_i, 
                                                                        size = slide_1)
        
        outer_border <- outer_border + outer_border %>% tf$clip_by_value(clip_value_min = 0, 
                                                                         clip_value_max = 1) * tf$slice(outer_bias, 
                                                                                                        begin = begin_i, 
                                                                                                        size = slide_1)
        
        outer_border <- outer_border * outter_array * tf$slice(1 - outer_cent_use, 
                                                               begin = begin_i, 
                                                               size = slide_1) * tf$slice(outer_use, 
                                                                                          begin = begin_i, 
                                                                                          size = slide_1)
        
        inner_center <- tf$sqrt(k_conv2d(inner_array, 
                                         kernel = tf$ones(shape = inner_kernel_value, 
                                                          dtype = "float32"), 
                                         padding = "same", 
                                         strides = 1, 
                                         dilation_rate = 1)) * tf$slice(inner_dot, 
                                                                        begin = begin_i, 
                                                                        size = slide_1) + tf$slice(inner_bias, 
                                                                                                   begin = begin_i, 
                                                                                                   size = slide_1)
        
        inner_center <- inner_center * inner_array * tf$slice(inner_cent_use, 
                                                              begin = begin_i, 
                                                              size = slide_1) * tf$slice(inner_use, 
                                                                                         begin = begin_i, 
                                                                                         size = slide_1)
        
        inner_border <- tf$sqrt(k_conv2d(outter_array, 
                                         kernel = tf$ones(shape = inner_kernel_value, 
                                                          dtype = "float32"), 
                                         padding = "same", 
                                         strides = 1, 
                                         dilation_rate = 1)) * tf$slice(inner_dot, 
                                                                        begin = begin_i, 
                                                                        size = slide_1)
        
        inner_border <- inner_border + inner_border %>% tf$clip_by_value(clip_value_min = 0, 
                                                                         clip_value_max = 1) * tf$slice(inner_bias, 
                                                                                                        begin = begin_i, 
                                                                                                        size = slide_1)
        
        inner_border <- inner_border * inner_array * tf$slice(1 - inner_cent_use, 
                                                              begin = begin_i, 
                                                              size = slide_1) * tf$slice(inner_use, 
                                                                                         begin = begin_i, 
                                                                                         size = slide_1)
        
        label_weight <- tf$slice(inner_basal_weight, 
                                 begin = begin_i, 
                                 size = slide_1)
        
        non_label_weight <- tf$slice(outer_basal_weight, 
                                     begin = begin_i, 
                                     size = slide_1)
        
        replace_array <- outer_center + outer_border + inner_center + inner_border + inner_array * label_weight + outter_array * non_label_weight
        
        replace_array <- tf$transpose(replace_array, 
                                      tf$cast(list(3,0,1,2), 
                                              dtype = "int32"))
        
        transform_run_tensor <- tf$transpose(run_tensor, 
                                             tf$cast(list(3,0,1,2), 
                                                     dtype = "int32"))
        
        
        transform_run_tensor <- tf$tensor_scatter_nd_update(transform_run_tensor, 
                                                            tf$reshape(i, 
                                                                       shape = tf$cast(list(1,1), 
                                                                                       dtype = "int32")), 
                                                            replace_array)
        
        run_tensor <- tf$transpose(transform_run_tensor, 
                                   tf$cast(list(1,2,3,0), 
                                           dtype = "int32"))
        
        list(tf$add(i, plus_data), run_tensor, outer_dot, outer_bias, inner_dot, inner_bias, outer_use, outer_cent_use, outer_size, outer_basal_weight, inner_use, inner_cent_use, inner_size, inner_basal_weight)
      }
      
      y_true <- blank_label_img_all %>% array_reshape(dim = base::c(1,dim(blank_label_img_all))) %>% tf$cast(dtype = "float32")
      
      weight <- tf$while_loop(c, b, list(i, y_true, outer_dot, outer_bias, inner_dot, inner_bias, outer_use, outer_cent_use, outer_size, outer_basal_weight, inner_use, inner_cent_use, inner_size, inner_basal_weight))[[2]]
      weight <- weight %>% as.array()
      max_weight <- max(weight)
      weight <- (weight %>% array_reshape(dim = dim(weight)[2:4]))/max_weight
      
      jet.colors = colorRampPalette(base::c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
      
      showModal(modalDialog(
        title = "Polygon weighted map preview!",
        size = "l",
        column(6,
               output$display_polygon_original_map_preview <- renderDisplay({
                 display(img_array)
               })
        ),
        column(6,
               output$display_polygon_weighted_map_preview <- renderDisplay({
                 display(weight %>% colormap(jet.colors(256)))
               })
        ),
        output$display_polygon_original_gradient <- renderPlot({
          image(matrix(1:256, 256), col = jet.colors(256),  axes = FALSE)
          axis(1, at = 0:as.integer(max_weight) * (1/as.integer(max_weight)), labels = 0:as.integer(max_weight))
        }, height = 200)
      ))
    }
    #print(use_polygon_df)
  }
})

observeEvent(input$Preview_point_weighted_map, {
  if (length(input$Preview_point_image) > 0) {
    file_location <- paste0("www/Image/", input$Training_Annotation_group, "/", input$Preview_point_image)
    file_location_jpg <- sapply(file_location, function(x){list.files(x, pattern = ".jpg")})
    loop_jpg_file <- paste0(file_location, "/", file_location_jpg)
    #print(loop_jpg_file)
    
    use_point_df <- as.data.frame(hot_to_r(input$point_label_on_train_table)) %>% filter(Use_for_train)
    img_array <- readImage(loop_jpg_file)
    img_dim <- dim(img_array)
    blank_label_img_all <- array(0, c(img_dim[1:2], nrow(use_point_df)))
    point_annotated_data <- readRDS(paste0(loop_jpg_file %>% dirname(), "/points.rds"))
    
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
      
      b_run_tensor <- tf$constant(value = 0, 
                                  shape = list(1 %>% as.integer(), 
                                               dim(blank_label_img_all)[1] %>% as.integer(), 
                                               dim(blank_label_img_all)[2] %>% as.integer(), 
                                               dim(blank_label_img_all)[3] %>% as.integer()), 
                                  dtype = "float32")
      
      max_data <- b_run_tensor %>% 
        tf$shape() %>% 
        tf$slice(begin = tf$constant(array(data = 3), 
                                     dtype = "int32"), 
                 size = tf$constant(array(data = 1), 
                                    dtype = "int32"))
      
      plus_data <- tf$constant(array(data = 1), 
                               dtype = "int32")
      
      pre_begin_tensor <- tf$zeros(shape = list(3 %>% as.integer()), 
                                   dtype = "int32")
      
      outer_use <- list()
      outer_cent_use <- list()
      outer_size <- list()
      outer_basal_weight <- list()
      outer_dot <- list()
      outer_bias <- list()
      inner_use <- list()
      inner_cent_use <- list()
      inner_size <- list()
      inner_basal_weight <- list()
      inner_dot <- list()
      inner_bias <- list()
      
      
      for (i in 1:nrow(use_point_df)) {
        if (use_point_df$Outer_weight_train[i] == "N") {
          outer_use[[i]] <- 0
          outer_cent_use[[i]] <- 0
          outer_size[[i]] <- 10
          outer_basal_weight[[i]] <- use_point_df$Outer_non_labeled_object_basal_weight[i]
          outer_dot[[i]] <- 1
          outer_bias[[i]] <- 0
        } else if (use_point_df$Outer_weight_train[i] == "C") {
          outer_use[[i]] <- 1
          outer_cent_use[[i]] <- 1
          outer_size[[i]] <- use_point_df$Outer_central_size[i]
          outer_basal_weight[[i]] <- use_point_df$Outer_non_labeled_object_basal_weight[i]
          outer_dot[[i]] <- use_point_df$Outer_central_dot[i]
          outer_bias[[i]] <- use_point_df$Outer_central_bias[i]
        } else if (use_point_df$Outer_weight_train[i] == "B") {
          outer_use[[i]] <- 1
          outer_cent_use[[i]] <- 0
          outer_size[[i]] <- use_point_df$Outer_boundary_size[i]
          outer_basal_weight[[i]] <- use_point_df$Outer_non_labeled_object_basal_weight[i]
          outer_dot[[i]] <- use_point_df$Outer_boundary_dot[i]
          outer_bias[[i]] <- use_point_df$Outer_boundary_bias[i]
        }
        
        if (use_point_df$Inner_weight_train[i] == "N") {
          inner_use[[i]] <- 0
          inner_cent_use[[i]] <- 0
          inner_size[[i]] <- 10
          inner_basal_weight[[i]] <- use_point_df$Inner_labeled_object_basal_weight[i]
          inner_dot[[i]] <- 1
          inner_bias[[i]] <- 0
        } else if (use_point_df$Inner_weight_train[i] == "C") {
          inner_use[[i]] <- 1
          inner_cent_use[[i]] <- 1
          inner_size[[i]] <- use_point_df$Inner_central_size[i]
          inner_basal_weight[[i]] <- use_point_df$Inner_labeled_object_basal_weight[i]
          inner_dot[[i]] <- use_point_df$Inner_central_dot[i]
          inner_bias[[i]] <- use_point_df$Inner_central_bias[i]
        } else if (use_point_df$Inner_weight_train[i] == "B") {
          inner_use[[i]] <- 1
          inner_cent_use[[i]] <- 0
          inner_size[[i]] <- use_point_df$Inner_boundary_size[i]
          inner_basal_weight[[i]] <- use_point_df$Inner_labeled_object_basal_weight[i]
          inner_dot[[i]] <- use_point_df$Inner_boundary_dot[i]
          inner_bias[[i]] <- use_point_df$Inner_boundary_bias[i]
        }
      }
      
      outer_use <- tf$cast(outer_use, dtype = "float32")
      outer_cent_use <- tf$cast(outer_cent_use, dtype = "float32")
      outer_size <- tf$cast(outer_size, dtype = "int32")
      outer_basal_weight <- tf$cast(outer_basal_weight, dtype = "float32")
      outer_dot <- tf$cast(outer_dot, dtype = "float32")
      outer_bias <- tf$cast(outer_bias, dtype = "float32")
      
      inner_use <- tf$cast(inner_use, dtype = "float32")
      inner_cent_use <- tf$cast(inner_cent_use, dtype = "float32")
      inner_size <- tf$cast(inner_size, dtype = "int32")
      inner_basal_weight <- tf$cast(inner_basal_weight, dtype = "float32")
      inner_dot <- tf$cast(inner_dot, dtype = "float32")
      inner_bias <- tf$cast(inner_bias, dtype = "float32")
      
      i <- tf$constant(array(data = 0), dtype = "int32")
      c <- function(i, run_tensor, outer_dot, outer_bias, inner_dot, inner_bias, outer_use, outer_cent_use, outer_size, outer_basal_weight, inner_use, inner_cent_use, inner_size, inner_basal_weight) {tf$less(i, max_data)}
      b <- function(i, run_tensor, outer_dot, outer_bias, inner_dot, inner_bias, outer_use, outer_cent_use, outer_size, outer_basal_weight, inner_use, inner_cent_use, inner_size, inner_basal_weight) {
        check_tensor <- tf$concat(list(run_tensor %>% 
                                         tf$shape() %>% 
                                         tf$slice(begin = tf$constant(array(data = 0), 
                                                                      dtype = "int32"), 
                                                  size = tf$constant(array(data = 3), 
                                                                     dtype = "int32")),
                                       tf$ones(shape = 1 %>% as.integer(), 
                                               dtype = "int32")), 
                                  axis = as.integer(0))
        
        begin_value <- tf$concat(list(pre_begin_tensor, i), axis = as.integer(0))
        
        slide_1 <- tf$cast(list(1), dtype = "int32")
        
        begin_i <- i %>% 
          tf$reshape(shape = slide_1)
        
        inner_array <- tf$slice(run_tensor, 
                                begin = begin_value, 
                                size = check_tensor)
        
        outter_array <- 1 - inner_array
        
        outter_kernel_value <- tf$concat(list(tf$slice(outer_size, 
                                                       begin = begin_i, 
                                                       size = slide_1),
                                              tf$slice(outer_size, 
                                                       begin = begin_i, 
                                                       size = slide_1),
                                              tf$ones(shape = list(2 %>% as.integer()), 
                                                      dtype = "int32")), 
                                         axis = as.integer(0))
        
        inner_kernel_value <- tf$concat(list(tf$slice(inner_size, 
                                                      begin = begin_i, 
                                                      size = slide_1),
                                             tf$slice(inner_size, 
                                                      begin = begin_i, 
                                                      size = slide_1),
                                             tf$ones(shape = list(2 %>% as.integer()), 
                                                     dtype = "int32")), 
                                        axis = as.integer(0))
        
        outer_center <- tf$sqrt(k_conv2d(outter_array, 
                                         kernel = tf$ones(shape = outter_kernel_value, 
                                                          dtype = "float32"), 
                                         padding = "same", 
                                         strides = 1, 
                                         dilation_rate = 1)) * tf$slice(outer_dot, 
                                                                        begin = begin_i, 
                                                                        size = slide_1) + tf$slice(outer_bias, 
                                                                                                   begin = begin_i, 
                                                                                                   size = slide_1)
        
        outer_center <- outer_center * outter_array * tf$slice(outer_cent_use, 
                                                               begin = begin_i, 
                                                               size = slide_1) * tf$slice(outer_use, 
                                                                                          begin = begin_i, 
                                                                                          size = slide_1) 
        
        outer_border <- tf$sqrt(k_conv2d(inner_array, 
                                         kernel = tf$ones(shape = outter_kernel_value, 
                                                          dtype = "float32"), 
                                         padding = "same", 
                                         strides = 1, 
                                         dilation_rate = 1)) * tf$slice(outer_dot, 
                                                                        begin = begin_i, 
                                                                        size = slide_1)
        
        outer_border <- outer_border + outer_border %>% tf$clip_by_value(clip_value_min = 0, 
                                                                         clip_value_max = 1) * tf$slice(outer_bias, 
                                                                                                        begin = begin_i, 
                                                                                                        size = slide_1)
        
        outer_border <- outer_border * outter_array * tf$slice(1 - outer_cent_use, 
                                                               begin = begin_i, 
                                                               size = slide_1) * tf$slice(outer_use, 
                                                                                          begin = begin_i, 
                                                                                          size = slide_1)
        
        inner_center <- tf$sqrt(k_conv2d(inner_array, 
                                         kernel = tf$ones(shape = inner_kernel_value, 
                                                          dtype = "float32"), 
                                         padding = "same", 
                                         strides = 1, 
                                         dilation_rate = 1)) * tf$slice(inner_dot, 
                                                                        begin = begin_i, 
                                                                        size = slide_1) + tf$slice(inner_bias, 
                                                                                                   begin = begin_i, 
                                                                                                   size = slide_1)
        
        inner_center <- inner_center * inner_array * tf$slice(inner_cent_use, 
                                                              begin = begin_i, 
                                                              size = slide_1) * tf$slice(inner_use, 
                                                                                         begin = begin_i, 
                                                                                         size = slide_1)
        
        inner_border <- tf$sqrt(k_conv2d(outter_array, 
                                         kernel = tf$ones(shape = inner_kernel_value, 
                                                          dtype = "float32"), 
                                         padding = "same", 
                                         strides = 1, 
                                         dilation_rate = 1)) * tf$slice(inner_dot, 
                                                                        begin = begin_i, 
                                                                        size = slide_1)
        
        inner_border <- inner_border + inner_border %>% tf$clip_by_value(clip_value_min = 0, 
                                                                         clip_value_max = 1) * tf$slice(inner_bias, 
                                                                                                        begin = begin_i, 
                                                                                                        size = slide_1)
        
        inner_border <- inner_border * inner_array * tf$slice(1 - inner_cent_use, 
                                                              begin = begin_i, 
                                                              size = slide_1) * tf$slice(inner_use, 
                                                                                         begin = begin_i, 
                                                                                         size = slide_1)
        
        label_weight <- tf$slice(inner_basal_weight, 
                                 begin = begin_i, 
                                 size = slide_1)
        
        non_label_weight <- tf$slice(outer_basal_weight, 
                                     begin = begin_i, 
                                     size = slide_1)
        
        replace_array <- outer_center + outer_border + inner_center + inner_border + inner_array * label_weight + outter_array * non_label_weight
        
        replace_array <- tf$transpose(replace_array, 
                                      tf$cast(list(3,0,1,2), 
                                              dtype = "int32"))
        
        transform_run_tensor <- tf$transpose(run_tensor, 
                                             tf$cast(list(3,0,1,2), 
                                                     dtype = "int32"))
        
        
        transform_run_tensor <- tf$tensor_scatter_nd_update(transform_run_tensor, 
                                                            tf$reshape(i, 
                                                                       shape = tf$cast(list(1,1), 
                                                                                       dtype = "int32")), 
                                                            replace_array)
        
        run_tensor <- tf$transpose(transform_run_tensor, 
                                   tf$cast(list(1,2,3,0), 
                                           dtype = "int32"))
        
        list(tf$add(i, plus_data), run_tensor, outer_dot, outer_bias, inner_dot, inner_bias, outer_use, outer_cent_use, outer_size, outer_basal_weight, inner_use, inner_cent_use, inner_size, inner_basal_weight)
      }
      
      y_true <- blank_label_img_all %>% array_reshape(dim = base::c(1,dim(blank_label_img_all))) %>% tf$cast(dtype = "float32")
      
      weight <- tf$while_loop(c, b, list(i, y_true, outer_dot, outer_bias, inner_dot, inner_bias, outer_use, outer_cent_use, outer_size, outer_basal_weight, inner_use, inner_cent_use, inner_size, inner_basal_weight))[[2]]
      weight <- weight %>% as.array()
      max_weight <- max(weight)
      weight <- (weight %>% array_reshape(dim = dim(weight)[2:4]))/max_weight
      
      jet.colors = colorRampPalette(base::c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
      
      showModal(modalDialog(
        title = "Point weighted map preview!",
        size = "l",
        column(6,
               output$display_point_original_map_preview <- renderDisplay({
                 display(img_array)
               })
        ),
        column(6,
               output$display_point_weighted_map_preview <- renderDisplay({
                 display(weight %>% colormap(jet.colors(256)))
               })
        ),
        output$display_point_original_gradient <- renderPlot({
          image(matrix(1:256, 256), col = jet.colors(256),  axes = FALSE)
          axis(1, at = 0:as.integer(max_weight) * (1/as.integer(max_weight)), labels = 0:as.integer(max_weight))
        }, height = 200)
      ))
    }
  }

  
  
  
  
})

observeEvent(input$Start_generate_training_file, {
  if (length(input$Choose_file_training_table) > 0) {
    # & is.null(input$point_label_on_train_table) == FALSE
    if (input$Training_Slide_or_image == "Image") {
      
      info_list <- list()
      
      file_location <- paste0("www/Image/", input$Training_Annotation_group, "/", input$Choose_file_training_table)
      file_location_jpg <- sapply(file_location, function(x){list.files(x, pattern = ".jpg")})
      loop_jpg_file <- paste0(file_location, "/", file_location_jpg)
      if (input$turn_on_point_label_on_train | input$turn_on_polygon_label_on_train) {
        if (is.null(input$point_label_on_train_table)) {
          use_point_df <- data.frame()
          enhancement_point_df <- data.frame()
          only_train_point_df <- data.frame()
          point_meaning <- c()
          point_inner_boundary_size <- c()
          point_inner_boundary_dot <- c()
          point_inner_boundary_bias <- c()
          point_outer_boundary_size <- c()
          point_outer_boundary_dot <- c()
          point_outer_boundary_bias <- c()
          point_inner_central_size <- c()
          point_inner_central_dot <- c()
          point_inner_central_bias <- c()
          point_outer_central_size <- c()
          point_outer_central_dot <- c()
          point_outer_central_bias <- c()
          point_inner_labeled_object_basal_weight <- c()
          point_outer_non_labeled_object_basal_weight <- c()
          point_inner_weight_train <- c()
          point_outer_weight_train <- c()
        } else {
          use_point_df <- as.data.frame(hot_to_r(input$point_label_on_train_table)) %>% filter(Use_for_train)
          enhancement_point_df <- as.data.frame(hot_to_r(input$point_label_on_train_table)) %>% filter(Use_for_Enhancement)
          only_train_point_df <- as.data.frame(hot_to_r(input$point_label_on_train_table)) %>% filter(Use_for_Only_train_area)
          point_meaning <- use_point_df$Meanings
          point_inner_boundary_size <- use_point_df$Inner_boundary_size
          point_inner_boundary_dot <- use_point_df$Inner_boundary_dot
          point_inner_boundary_bias <- use_point_df$Inner_boundary_bias
          point_outer_boundary_size <- use_point_df$Outer_boundary_size
          point_outer_boundary_dot <- use_point_df$Outer_boundary_dot
          point_outer_boundary_bias <- use_point_df$Outer_boundary_bias
          point_inner_central_size <- use_point_df$Inner_central_size
          point_inner_central_dot <- use_point_df$Inner_central_dot
          point_inner_central_bias <- use_point_df$Inner_central_bias
          point_outer_central_size <- use_point_df$Outer_central_size
          point_outer_central_dot <- use_point_df$Outer_central_dot
          point_outer_central_bias <- use_point_df$Outer_central_bias
          point_inner_labeled_object_basal_weight <- use_point_df$Inner_labeled_object_basal_weight
          point_outer_non_labeled_object_basal_weight <- use_point_df$Outer_non_labeled_object_basal_weight
          point_inner_weight_train <- use_point_df$Inner_weight_train
          point_outer_weight_train <- use_point_df$Outer_weight_train
        }
        if (is.null(input$polygon_label_on_train_table)) {
          use_polygon_df <- data.frame()
          enhancement_polygon_df <- data.frame()
          only_train_polygon_df <- data.frame()
          polygon_meaning <- c()
          polygon_inner_boundary_size <- c()
          polygon_inner_boundary_dot <- c()
          polygon_inner_boundary_bias <- c()
          polygon_outer_boundary_size <- c()
          polygon_outer_boundary_dot <- c()
          polygon_outer_boundary_bias <- c()
          polygon_inner_central_size <- c()
          polygon_inner_central_dot <- c()
          polygon_inner_central_bias <- c()
          polygon_outer_central_size <- c()
          polygon_outer_central_dot <- c()
          polygon_outer_central_bias <- c()
          polygon_inner_labeled_object_basal_weight <- c()
          polygon_outer_non_labeled_object_basal_weight <- c()
          polygon_inner_weight_train <- c()
          polygon_outer_weight_train <- c()
        } else {
          use_polygon_df <- as.data.frame(hot_to_r(input$polygon_label_on_train_table)) %>% filter(Use_for_train)
          enhancement_polygon_df <- as.data.frame(hot_to_r(input$polygon_label_on_train_table)) %>% filter(Use_for_Enhancement)
          only_train_polygon_df <- as.data.frame(hot_to_r(input$polygon_label_on_train_table)) %>% filter(Use_for_Only_train_area)
          polygon_meaning <- use_polygon_df$Meanings
          polygon_inner_boundary_size <- use_polygon_df$Inner_boundary_size
          polygon_inner_boundary_dot <- use_polygon_df$Inner_boundary_dot
          polygon_inner_boundary_bias <- use_polygon_df$Inner_boundary_bias
          polygon_outer_boundary_size <- use_polygon_df$Outer_boundary_size
          polygon_outer_boundary_dot <- use_polygon_df$Outer_boundary_dot
          polygon_outer_boundary_bias <- use_polygon_df$Outer_boundary_bias
          polygon_inner_central_size <- use_polygon_df$Inner_central_size
          polygon_inner_central_dot <- use_polygon_df$Inner_central_dot
          polygon_inner_central_bias <- use_polygon_df$Inner_central_bias
          polygon_outer_central_size <- use_polygon_df$Outer_central_size
          polygon_outer_central_dot <- use_polygon_df$Outer_central_dot
          polygon_outer_central_bias <- use_polygon_df$Outer_central_bias
          polygon_inner_labeled_object_basal_weight <- use_polygon_df$Inner_labeled_object_basal_weight
          polygon_outer_non_labeled_object_basal_weight <- use_polygon_df$Outer_non_labeled_object_basal_weight
          polygon_inner_weight_train <- use_polygon_df$Inner_weight_train
          polygon_outer_weight_train <- use_polygon_df$Outer_weight_train
        }
        file_location_name <- paste0("training_file/", input$Training_Slide_or_image, "_", input$Training_Annotation_group, "_", Sys.time() %>% as.character() %>% str_replace_all(" ", "_") %>% str_replace_all(":", "-"))
        dir.create(file_location_name)
        
        progressSweetAlert(
          session = session, 
          id = "generate_training_file_myprogress",
          title = "Generate in progress",
          display_pct = TRUE, value = 0
        )
        
        if (nrow(use_point_df) > 0) {
          for (k in 1:nrow(use_point_df)) {
            info_list[["layer_class_id_P"]][k] <- paste0(use_point_df$Primary_point_class[k], 
                                                         "_",
                                                         use_point_df$Secondary_point_class[k],
                                                         "_",
                                                         use_point_df$Tertiary_point_class[k],
                                                         "_",
                                                         paste0("P_", use_point_df$Point_number[k]))
          }
        }
        
        if (nrow(use_polygon_df) > 0) {
          for (k in 1:nrow(use_polygon_df)) {
            polygon_data_each_layer <- paste0(use_polygon_df$Primary_polygon_class[k],
                                              "_",
                                              use_polygon_df$Secondary_polygon_class[k],
                                              "_",
                                              use_polygon_df$Tertiary_polygon_class[k])
            info_list[["layer_class_id_G"]][k] <- polygon_data_each_layer
          }
        }
        
        Basal_train_weight_fraction <- input$Basal_train_weight_fraction
        core_use <- input$Start_generate_training_file_cpu_number %>% as.integer()
        
        core_run_list <- list()
        
        for (i in 1:length(loop_jpg_file)) {
          
          remainder <- (i - 1) %% core_use + 1
          quotient <- (i - 1) %/% core_use
          
          img_array <- readImage(loop_jpg_file[i])
          img_dim <- dim(img_array)
          
          info_list[["image_area"]][i] <- img_dim[1] * img_dim[2]
          info_list[["image_width"]][i] <- img_dim[1]
          info_list[["image_hight"]][i] <- img_dim[2]
          info_list[["training_file_location"]][i] <- paste0(getwd(), "/", file_location_name, "/", img_dim[1] * img_dim[2], "_", file_location_jpg[i] %>% str_replace_all(".jpg", ""), ".rds")
          
          if (quotient > 0) {
            resolve_done <- resolved(core_run_list[[remainder]])
          }
          
          core_run_list[[remainder]] <- future({
            image_training_file_generator_function(loop_jpg_file = loop_jpg_file[i],
                                                   use_point_df = use_point_df,
                                                   use_polygon_df = use_polygon_df,
                                                   enhancement_point_df = enhancement_point_df,
                                                   enhancement_polygon_df = enhancement_polygon_df,
                                                   only_train_point_df = only_train_point_df,
                                                   only_train_polygon_df = only_train_polygon_df,
                                                   Basal_train_weight_fraction = Basal_train_weight_fraction,
                                                   file_location_name = file_location_name,
                                                   file_location_jpg = file_location_jpg[i])
          }, seed = FALSE)
          
          updateProgressBar(
            session = session,
            id = "generate_training_file_myprogress",
            title = paste0("Generating in progress...(", i, "/", length(loop_jpg_file), ")"),
            value = i * 1/length(loop_jpg_file) * 100
          )
          
        }
        
        resolve(core_run_list)
        
        info_list[["image_or_slide"]] <- input$Training_Slide_or_image
        info_list[["layer_meaning"]] <- c(point_meaning, polygon_meaning)
        info_list[["Inner_boundary_size"]] <- c(point_inner_boundary_size, polygon_inner_boundary_size)
        info_list[["Inner_boundary_dot"]] <- c(point_inner_boundary_dot, polygon_inner_boundary_dot)
        info_list[["Inner_boundary_bias"]] <- c(point_inner_boundary_bias, polygon_inner_boundary_bias)
        info_list[["Outer_boundary_size"]] <- c(point_outer_boundary_size, polygon_outer_boundary_size)
        info_list[["Outer_boundary_dot"]] <- c(point_outer_boundary_dot, polygon_outer_boundary_dot)
        info_list[["Outer_boundary_bias"]] <- c(point_outer_boundary_bias, polygon_outer_boundary_bias)
        info_list[["Inner_central_size"]] <- c(point_inner_central_size, polygon_inner_central_size)
        info_list[["Inner_central_dot"]] <- c(point_inner_central_dot, polygon_inner_central_dot)
        info_list[["Inner_central_bias"]] <- c(point_inner_central_bias, polygon_inner_central_bias)
        info_list[["Outer_central_size"]] <- c(point_outer_central_size, polygon_outer_central_size)
        info_list[["Outer_central_dot"]] <- c(point_outer_central_dot, polygon_outer_central_dot)
        info_list[["Outer_central_bias"]] <- c(point_outer_central_bias, polygon_outer_central_bias)
        info_list[["Inner_labeled_object_basal_weight"]] <- c(point_inner_labeled_object_basal_weight, polygon_inner_labeled_object_basal_weight)
        info_list[["Outer_non_labeled_object_basal_weight"]] <- c(point_outer_non_labeled_object_basal_weight, polygon_outer_non_labeled_object_basal_weight)
        info_list[["Inner_weight_train"]] <- c(point_inner_weight_train, polygon_inner_weight_train)
        info_list[["Outer_weight_train"]] <- c(point_outer_weight_train, polygon_outer_weight_train)
        info_list[["layer_option"]] <- c(rep("P", length(point_meaning)), rep("G", length(polygon_meaning)))
        info_list[["layer_structure"]] <- input$training_network_type
        info_list[["loss_function"]] <- input$training_loss_function_type
        info_list[["max_train_size"]] <- input$training_tile_size %>% as.numeric()
        info_list[["epoch_number"]] <- input$training_epoch_number %>% as.numeric()
        info_list[["monitor"]] <- input$training_monitor_type
        info_list[["batch_size"]] <- input$training_batch_size %>% as.numeric()
        info_list[["training_model_group"]] <- input$training_model_group
        info_list[["training_model_name"]] <- input$training_model_name
        info_list[["training_patient"]] <- input$training_early_stop_patient
        info_list[["training_fraction"]] <- input$training_validation_fraction
        info_list[["optimizer"]] <- input$training_optimizer_selection
        info_list[["learning_rate"]] <- input$training_learning_rate_selection %>% as.numeric()
        info_list[["red_augmentation"]] <- input$training_red_augmentation %>% as.numeric()
        info_list[["green_augmentation"]] <- input$training_green_augmentation %>% as.numeric()
        info_list[["blue_augmentation"]] <- input$training_blue_augmentation %>% as.numeric()
        info_list[["red_augmentation_AM"]] <- input$training_red_augmentation_AM %>% as.numeric()
        info_list[["green_augmentation_AM"]] <- input$training_green_augmentation_AM %>% as.numeric()
        info_list[["blue_augmentation_AM"]] <- input$training_blue_augmentation_AM %>% as.numeric()
        info_list[["image_rotate"]] <- input$training_image_rotate %>% as.numeric()
        info_list[["image_flip_flop"]] <- input$training_image_flip_flop %>% as.numeric()
        info_list[["basal_train_weight_fraction"]] <- input$Basal_train_weight_fraction
        info_list[["train_on_random_or_sequential"]] <- input$train_on_random_or_sequential
        
        saveRDS(
          info_list,
          paste0(file_location_name, "/detail.rds")
        )
        
        file.copy(paste0("network_database/",input$training_network_class,"/run_me.R"), 
                  paste0(file_location_name, "/Run_me_start_training.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_loss/", input$training_loss_function_type, "/train_loss.R"), 
                  paste0(file_location_name, "/train_loss.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_monitor/", input$training_monitor_type, "/monitor.R"), 
                  paste0(file_location_name, "/monitor.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_network/", input$training_network_type, "/train_use.R"), 
                  paste0(file_location_name, "/train_use.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_network/", input$training_network_type, "/run_use.R"), 
                  paste0(file_location_name, "/run_use.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_optimizer/", input$training_optimizer_selection, "/train_optimizer.R"), 
                  paste0(file_location_name, "/train_optimizer.R"))
        
        closeSweetAlert(session = session)
        sendSweetAlert(
          session = session,
          title = "Generate completed !",
          type = "success"
        )
        #print(point_annotated_data)
      }
    } else {
      
      info_list <- list()
      
      file_location <- paste0("www/Slide/", input$Training_Annotation_group, "/", input$Choose_file_training_table)
      file_location_ndpi <- sapply(file_location, function(x){list.files(x, pattern = ".ndpi")})
      loop_ndpi_file <- paste0(file_location, "/", file_location_ndpi)
      if (input$turn_on_point_label_on_train | input$turn_on_polygon_label_on_train) {
        if (is.null(input$point_label_on_train_table)) {
          use_point_df <- data.frame()
          enhancement_point_df <- data.frame()
          only_train_point_df <- data.frame()
          point_meaning <- c()
          point_inner_boundary_size <- c()
          point_inner_boundary_dot <- c()
          point_inner_boundary_bias <- c()
          point_outer_boundary_size <- c()
          point_outer_boundary_dot <- c()
          point_outer_boundary_bias <- c()
          point_inner_central_size <- c()
          point_inner_central_dot <- c()
          point_inner_central_bias <- c()
          point_outer_central_size <- c()
          point_outer_central_dot <- c()
          point_outer_central_bias <- c()
          point_inner_labeled_object_basal_weight <- c()
          point_outer_non_labeled_object_basal_weight <- c()
          point_inner_weight_train <- c()
          point_outer_weight_train <- c()
        } else {
          use_point_df <- as.data.frame(hot_to_r(input$point_label_on_train_table)) %>% filter(Use_for_train)
          enhancement_point_df <- as.data.frame(hot_to_r(input$point_label_on_train_table)) %>% filter(Use_for_Enhancement)
          only_train_point_df <- as.data.frame(hot_to_r(input$point_label_on_train_table)) %>% filter(Use_for_Only_train_area)
          point_meaning <- use_point_df$Meanings
          point_inner_boundary_size <- use_point_df$Inner_boundary_size
          point_inner_boundary_dot <- use_point_df$Inner_boundary_dot
          point_inner_boundary_bias <- use_point_df$Inner_boundary_bias
          point_outer_boundary_size <- use_point_df$Outer_boundary_size
          point_outer_boundary_dot <- use_point_df$Outer_boundary_dot
          point_outer_boundary_bias <- use_point_df$Outer_boundary_bias
          point_inner_central_size <- use_point_df$Inner_central_size
          point_inner_central_dot <- use_point_df$Inner_central_dot
          point_inner_central_bias <- use_point_df$Inner_central_bias
          point_outer_central_size <- use_point_df$Outer_central_size
          point_outer_central_dot <- use_point_df$Outer_central_dot
          point_outer_central_bias <- use_point_df$Outer_central_bias
          point_inner_labeled_object_basal_weight <- use_point_df$Inner_labeled_object_basal_weight
          point_outer_non_labeled_object_basal_weight <- use_point_df$Outer_non_labeled_object_basal_weight
          point_inner_weight_train <- use_point_df$Inner_weight_train
          point_outer_weight_train <- use_point_df$Outer_weight_train
        }
        if (is.null(input$polygon_label_on_train_table)) {
          use_polygon_df <- data.frame()
          enhancement_polygon_df <- data.frame()
          only_train_polygon_df <- data.frame()
          polygon_meaning <- c()
          polygon_inner_boundary_size <- c()
          polygon_inner_boundary_dot <- c()
          polygon_inner_boundary_bias <- c()
          polygon_outer_boundary_size <- c()
          polygon_outer_boundary_dot <- c()
          polygon_outer_boundary_bias <- c()
          polygon_inner_central_size <- c()
          polygon_inner_central_dot <- c()
          polygon_inner_central_bias <- c()
          polygon_outer_central_size <- c()
          polygon_outer_central_dot <- c()
          polygon_outer_central_bias <- c()
          polygon_inner_labeled_object_basal_weight <- c()
          polygon_outer_non_labeled_object_basal_weight <- c()
          polygon_inner_weight_train <- c()
          polygon_outer_weight_train <- c()
          polygon_Boundary_train_fraction <- c()
        } else {
          use_polygon_df <- as.data.frame(hot_to_r(input$polygon_label_on_train_table)) %>% filter(Use_for_train)
          enhancement_polygon_df <- as.data.frame(hot_to_r(input$polygon_label_on_train_table)) %>% filter(Use_for_Enhancement)
          only_train_polygon_df <- as.data.frame(hot_to_r(input$polygon_label_on_train_table)) %>% filter(Use_for_Only_train_area)
          polygon_meaning <- use_polygon_df$Meanings
          polygon_inner_boundary_size <- use_polygon_df$Inner_boundary_size
          polygon_inner_boundary_dot <- use_polygon_df$Inner_boundary_dot
          polygon_inner_boundary_bias <- use_polygon_df$Inner_boundary_bias
          polygon_outer_boundary_size <- use_polygon_df$Outer_boundary_size
          polygon_outer_boundary_dot <- use_polygon_df$Outer_boundary_dot
          polygon_outer_boundary_bias <- use_polygon_df$Outer_boundary_bias
          polygon_inner_central_size <- use_polygon_df$Inner_central_size
          polygon_inner_central_dot <- use_polygon_df$Inner_central_dot
          polygon_inner_central_bias <- use_polygon_df$Inner_central_bias
          polygon_outer_central_size <- use_polygon_df$Outer_central_size
          polygon_outer_central_dot <- use_polygon_df$Outer_central_dot
          polygon_outer_central_bias <- use_polygon_df$Outer_central_bias
          polygon_inner_labeled_object_basal_weight <- use_polygon_df$Inner_labeled_object_basal_weight
          polygon_outer_non_labeled_object_basal_weight <- use_polygon_df$Outer_non_labeled_object_basal_weight
          polygon_inner_weight_train <- use_polygon_df$Inner_weight_train
          polygon_outer_weight_train <- use_polygon_df$Outer_weight_train
          polygon_Boundary_train_fraction <- use_polygon_df$Boundary_train_fraction
        }
        file_location_name <- paste0("training_file/", input$Training_Slide_or_image, "_", input$Training_Annotation_group, "_", Sys.time() %>% 
                                       as.character() %>% str_replace_all(" ", "_") %>% 
                                       str_replace_all(":", "-"))
        dir.create(file_location_name)
        
        progressSweetAlert(
          session = session, id = "generate_training_file_myprogress",
          title = "Generate in progress",
          display_pct = TRUE, value = 0
        )
        
        if (nrow(use_point_df) > 0) {
          for (k in 1:nrow(use_point_df)) {
            info_list[["layer_class_id_P"]][k] <- paste0(use_point_df$Primary_point_class[k], 
                                                         "_",
                                                         use_point_df$Secondary_point_class[k],
                                                         "_",
                                                         use_point_df$Tertiary_point_class[k],
                                                         "_",
                                                         paste0("P_", use_point_df$Point_number[k]))
          }
        }
        
        if (nrow(use_polygon_df) > 0) {
          for (k in 1:nrow(use_polygon_df)) {
            info_list[["layer_class_id_G"]][k] <- paste0(use_polygon_df$Primary_polygon_class[k],
                                                         "_",
                                                         use_polygon_df$Secondary_polygon_class[k],
                                                         "_",
                                                         use_polygon_df$Tertiary_polygon_class[k])
          }
        }
        
        file_pointer_df <- data.frame(
          slide_location = NA,
          slide_process_location = NA,
          point_label_location = NA,
          polygon_label_location = NA,
          slide_width = NA,
          slide_height = NA,
          slide_area = NA
        )
        tile_size_use <- input$training_tile_size %>% as.numeric()
        Basal_train_weight_fraction <- input$Basal_train_weight_fraction
        core_use <- input$Start_generate_training_file_cpu_number %>% as.integer()
        
        core_run_list <- list()
        
        for (i in 1:length(loop_ndpi_file)) {
          
          slide_info <- NDPI_info(loop_ndpi_file[i])
          
          remainder <- (i - 1) %% core_use + 1
          quotient <- (i - 1) %/% core_use
          
          if (quotient > 0) {
            resolve_done <- resolved(core_run_list[[remainder]])
          }
          
          core_run_list[[remainder]] <- future({
            slide_training_file_generator_function(loop_jpg_file = loop_ndpi_file[i],
                                                   use_point_df = use_point_df,
                                                   use_polygon_df = use_polygon_df,
                                                   enhancement_point_df = enhancement_point_df,
                                                   enhancement_polygon_df = enhancement_polygon_df,
                                                   only_train_point_df = only_train_point_df,
                                                   only_train_polygon_df = only_train_polygon_df,
                                                   Basal_train_weight_fraction = Basal_train_weight_fraction,
                                                   tile_size = tile_size_use,
                                                   file_location_name = file_location_name,
                                                   file_location_jpg = file_location_ndpi[i])
          }, seed = FALSE)
          
          
          
          file_pointer_df[i,"slide_location"] <- paste0(getwd(), "/", loop_ndpi_file[i])
          file_pointer_df[i,"slide_process_location"] <- paste0(getwd(), 
                                                                "/", 
                                                                file_location_name, 
                                                                "/", 
                                                                file_location_ndpi[i] %>% str_replace_all(".ndpi", ""), 
                                                                ".rds")
          file_pointer_df[i,"point_label_location"] <- paste0(getwd(), "/", loop_ndpi_file[i] %>% dirname(), "/points.rds")
          file_pointer_df[i,"polygon_label_location"] <- paste0(getwd(), "/", loop_ndpi_file[i] %>% dirname(), "/polygons.rds")
          file_pointer_df[i,"slide_width"] <- slide_info$Width_pixel
          file_pointer_df[i,"slide_height"] <- slide_info$Height_pixel
          file_pointer_df[i,"slide_area"] <- as.numeric(slide_info$Width_pixel) * as.numeric(slide_info$Height_pixel)
          
          
          info_list[["image_width"]][i] <- slide_info$Width_pixel
          info_list[["image_hight"]][i] <- slide_info$Height_pixel
          info_list[["image_area"]][i] <- as.numeric(slide_info$Width_pixel) * as.numeric(slide_info$Height_pixel)
          
          updateProgressBar(
            session = session,
            id = "generate_training_file_myprogress",
            value = i * 1/length(loop_ndpi_file) * 100
          )
        }
        
        resolve(core_run_list)
        
        info_list[["image_or_slide"]] <- input$Training_Slide_or_image
        info_list[["slide_input_type"]] <- input$train_on_multiple_layer_once_or_different_zoom_level
        info_list[["slide_input_layer"]] <- input$Slide_training_use_layer
        info_list[["slide_output_layer"]] <- input$Slide_predicting_use_layer
        info_list[["layer_meaning"]] <- c(point_meaning, polygon_meaning)
        info_list[["Inner_boundary_size"]] <- c(point_inner_boundary_size, polygon_inner_boundary_size)
        info_list[["Inner_boundary_dot"]] <- c(point_inner_boundary_dot, polygon_inner_boundary_dot)
        info_list[["Inner_boundary_bias"]] <- c(point_inner_boundary_bias, polygon_inner_boundary_bias)
        info_list[["Outer_boundary_size"]] <- c(point_outer_boundary_size, polygon_outer_boundary_size)
        info_list[["Outer_boundary_dot"]] <- c(point_outer_boundary_dot, polygon_outer_boundary_dot)
        info_list[["Outer_boundary_bias"]] <- c(point_outer_boundary_bias, polygon_outer_boundary_bias)
        info_list[["Inner_central_size"]] <- c(point_inner_central_size, polygon_inner_central_size)
        info_list[["Inner_central_dot"]] <- c(point_inner_central_dot, polygon_inner_central_dot)
        info_list[["Inner_central_bias"]] <- c(point_inner_central_bias, polygon_inner_central_bias)
        info_list[["Outer_central_size"]] <- c(point_outer_central_size, polygon_outer_central_size)
        info_list[["Outer_central_dot"]] <- c(point_outer_central_dot, polygon_outer_central_dot)
        info_list[["Outer_central_bias"]] <- c(point_outer_central_bias, polygon_outer_central_bias)
        info_list[["Inner_labeled_object_basal_weight"]] <- c(point_inner_labeled_object_basal_weight, polygon_inner_labeled_object_basal_weight)
        info_list[["Outer_non_labeled_object_basal_weight"]] <- c(point_outer_non_labeled_object_basal_weight, polygon_outer_non_labeled_object_basal_weight)
        info_list[["Inner_weight_train"]] <- c(point_inner_weight_train, polygon_inner_weight_train)
        info_list[["Outer_weight_train"]] <- c(point_outer_weight_train, polygon_outer_weight_train)
        info_list[["layer_option"]] <- c(rep("P", length(point_meaning)), rep("G", length(polygon_meaning)))
        info_list[["layer_structure"]] <- input$training_network_type
        info_list[["loss_function"]] <- input$training_loss_function_type
        info_list[["max_train_size"]] <- input$training_tile_size %>% as.numeric()
        info_list[["epoch_number"]] <- input$training_epoch_number %>% as.numeric()
        info_list[["monitor"]] <- input$training_monitor_type
        info_list[["batch_size"]] <- input$training_batch_size %>% as.numeric()
        info_list[["training_model_group"]] <- input$training_model_group
        info_list[["training_model_name"]] <- input$training_model_name
        info_list[["training_patient"]] <- input$training_early_stop_patient
        info_list[["training_fraction"]] <- input$training_validation_fraction
        info_list[["optimizer"]] <- input$training_optimizer_selection
        info_list[["learning_rate"]] <- input$training_learning_rate_selection %>% as.numeric()
        info_list[["red_augmentation"]] <- input$training_red_augmentation %>% as.numeric()
        info_list[["green_augmentation"]] <- input$training_green_augmentation %>% as.numeric()
        info_list[["blue_augmentation"]] <- input$training_blue_augmentation %>% as.numeric()
        info_list[["red_augmentation_AM"]] <- input$training_red_augmentation_AM %>% as.numeric()
        info_list[["green_augmentation_AM"]] <- input$training_green_augmentation_AM %>% as.numeric()
        info_list[["blue_augmentation_AM"]] <- input$training_blue_augmentation_AM %>% as.numeric()
        info_list[["image_rotate"]] <- input$training_image_rotate %>% as.numeric()
        info_list[["image_flip_flop"]] <- input$training_image_flip_flop %>% as.numeric()
        info_list[["basal_train_weight_fraction"]] <- input$Basal_train_weight_fraction
        info_list[["train_on_random_or_sequential"]] <- input$train_on_random_or_sequential
        info_list[["Boundary_train_fraction"]] <- polygon_Boundary_train_fraction
        info_list[["slide_sample_number"]] <- generate_file_fraction_df$df$sample_number
        
        if (input$select_generate_file_fraction_type == "Custom value") {
          info_list[["slide_fraction"]] <- generate_file_fraction_df$df$custom_fraction
        } else if (input$select_generate_file_fraction_type == "Slide size") {
          info_list[["slide_fraction"]] <- generate_file_fraction_df$df$slide_size_fraction
        } else if (input$select_generate_file_fraction_type == "Tissue size") {
          info_list[["slide_fraction"]] <- generate_file_fraction_df$df$tissue_size_fraction
        }
        
        saveRDS(
          info_list,
          paste0(file_location_name, "/detail.rds")
        )
        
        saveRDS(
          file_pointer_df,
          paste0(file_location_name, "/file_pointer_df.rds")
        )
        
        file.copy(paste0("network_database/",input$training_network_class,"/run_me_slide.R"), 
                  paste0(file_location_name, "/Run_me_start_training.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_loss/", input$training_loss_function_type, "/train_loss.R"), 
                  paste0(file_location_name, "/train_loss.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_monitor/", input$training_monitor_type, "/monitor.R"), 
                  paste0(file_location_name, "/monitor.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_network/", input$training_network_type, "/train_use.R"), 
                  paste0(file_location_name, "/train_use.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_network/", input$training_network_type, "/run_use.R"), 
                  paste0(file_location_name, "/run_use.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_optimizer/", input$training_optimizer_selection, "/train_optimizer.R"), 
                  paste0(file_location_name, "/train_optimizer.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_pre_load_function/Pre_load_function.R"), 
                  paste0(file_location_name, "/Pre_load_function.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_salvage_code/Slide/Salvage_from_train.R"), 
                  paste0(file_location_name, "/Salvage_from_train.R"))
        
        file.copy(paste0("network_database/",input$training_network_class,"/Get_salvage_code/Slide/Salvage_from_validation.R"), 
                  paste0(file_location_name, "/Salvage_from_validation.R"))
        
        closeSweetAlert(session = session)
        sendSweetAlert(
          session = session,
          title = "Generate completed !",
          type = "success"
        )
        #print(point_annotated_data)
      }
    }
  } else {
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "No file is selected !!",
      type = "error"
    )
  }
})

output$point_label_on_train_table <- renderRHandsontable({
  file_name <- paste0("www/", input$Training_Slide_or_image, "/", input$Training_Annotation_group, "/points_note.rds")
  if (file.exists(file_name)) {
    points_data <- readRDS(file_name)
    points_data <- points_data %>% 
      filter(Primary_point_class != "") %>% 
      filter(Secondary_point_class != "") %>% 
      filter(Tertiary_point_class != "") %>% 
      filter(Point_number != "") %>% 
      arrange(Primary_point_class, Secondary_point_class, Tertiary_point_class, Point_number)
    if (nrow(points_data) > 0) {
      points_data["Use_for_train"] <- TRUE
      points_data["Use_for_Enhancement"] <- FALSE
      points_data["Use_for_Only_train_area"] <- FALSE
      points_data["Select_to_train_weight_fraction"] <- 1
      points_data["Point_size"] <- 9
      points_data["Inner_labeled_object_basal_weight"] <- 2
      points_data["Inner_weight_train"] <- "N"
      points_data["Inner_boundary_size"] <- 13
      points_data["Inner_boundary_dot"] <- 1
      points_data["Inner_boundary_bias"] <- 0
      points_data["Inner_central_size"] <- 9
      points_data["Inner_central_dot"] <- 1
      points_data["Inner_central_bias"] <- 0
      points_data["Outer_non_labeled_object_basal_weight"] <- 1
      points_data["Outer_weight_train"] <- "N"
      points_data["Outer_boundary_size"] <- 13
      points_data["Outer_boundary_dot"] <- 1
      points_data["Outer_boundary_bias"] <- 0
      points_data["Outer_central_size"] <- 9
      points_data["Outer_central_dot"] <- 1
      points_data["Outer_central_bias"] <- 0
      rhandsontable(points_data) %>% 
        hot_col(col = "Inner_weight_train", type = "dropdown", source = c("N", "B", "C")) %>% 
        hot_col(col = "Outer_weight_train", type = "dropdown", source = c("N", "B", "C"))
    }
  }
})

output$polygon_label_on_train_table <- renderRHandsontable({
  file_name <- paste0("www/", input$Training_Slide_or_image, "/", input$Training_Annotation_group, "/polygons_note.rds")
  if (file.exists(file_name)) {
    polygons_data <- readRDS(file_name)
    polygons_data <- polygons_data %>% 
      filter(Primary_polygon_class != "") %>% 
      filter(Secondary_polygon_class != "") %>% 
      filter(Tertiary_polygon_class != "") %>% 
      arrange(Primary_polygon_class, Secondary_polygon_class, Tertiary_polygon_class)
    if (nrow(polygons_data) > 0) {
      if (input$Training_Slide_or_image == "Image") {
        polygons_data["Use_for_train"] <- TRUE
        polygons_data["Use_for_Enhancement"] <- FALSE
        polygons_data["Use_for_Only_train_area"] <- FALSE
        polygons_data["Select_to_train_weight_fraction"] <- 1
        polygons_data["Inner_labeled_object_basal_weight"] <- 2
        polygons_data["Inner_weight_train"] <- "N"
        polygons_data["Inner_boundary_size"] <- 13
        polygons_data["Inner_boundary_dot"] <- 1
        polygons_data["Inner_boundary_bias"] <- 0
        polygons_data["Inner_central_size"] <- 9
        polygons_data["Inner_central_dot"] <- 1
        polygons_data["Inner_central_bias"] <- 0
        polygons_data["Outer_non_labeled_object_basal_weight"] <- 1
        polygons_data["Outer_weight_train"] <- "N"
        polygons_data["Outer_boundary_size"] <- 13
        polygons_data["Outer_boundary_dot"] <- 1
        polygons_data["Outer_boundary_bias"] <- 0
        polygons_data["Outer_central_size"] <- 9
        polygons_data["Outer_central_dot"] <- 1
        polygons_data["Outer_central_bias"] <- 0
        rhandsontable(polygons_data) %>% 
          hot_col(col = "Inner_weight_train", type = "dropdown", source = c("N", "B", "C")) %>% 
          hot_col(col = "Outer_weight_train", type = "dropdown", source = c("N", "B", "C"))
      } else if (input$Training_Slide_or_image == "Slide") {
        polygons_data["Use_for_train"] <- TRUE
        polygons_data["Use_for_Enhancement"] <- FALSE
        polygons_data["Use_for_Only_train_area"] <- FALSE
        polygons_data["Select_to_train_weight_fraction"] <- 1
        polygons_data["Inner_labeled_object_basal_weight"] <- 2
        polygons_data["Boundary_train_fraction"] <- 0
        polygons_data["Inner_weight_train"] <- "N"
        polygons_data["Inner_boundary_size"] <- 13
        polygons_data["Inner_boundary_dot"] <- 1
        polygons_data["Inner_boundary_bias"] <- 0
        polygons_data["Inner_central_size"] <- 9
        polygons_data["Inner_central_dot"] <- 1
        polygons_data["Inner_central_bias"] <- 0
        polygons_data["Outer_non_labeled_object_basal_weight"] <- 1
        polygons_data["Outer_weight_train"] <- "N"
        polygons_data["Outer_boundary_size"] <- 13
        polygons_data["Outer_boundary_dot"] <- 1
        polygons_data["Outer_boundary_bias"] <- 0
        polygons_data["Outer_central_size"] <- 9
        polygons_data["Outer_central_dot"] <- 1
        polygons_data["Outer_central_bias"] <- 0
        rhandsontable(polygons_data) %>% 
          hot_col(col = "Inner_weight_train", type = "dropdown", source = c("N", "B", "C")) %>% 
          hot_col(col = "Outer_weight_train", type = "dropdown", source = c("N", "B", "C"))
      }

    }
  }
})

#observeEvent(input$training_network_class, {
#  updatePrettyRadioButtons(session,
#                           inputId = "training_network_type",
#                           choices = list.files(paste0("network_database/", input$training_network_class, "/Get_network/")))
#})

observeEvent(input$select_all_generative_file, {
  updateMultiInput(session, inputId = "Choose_file_training_table", selected = list.files(paste0("www/", input$Training_Slide_or_image, "/", input$Training_Annotation_group)) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE))
})

observeEvent(input$delete_all_generative_file, {
  updateMultiInput(session, inputId = "Choose_file_training_table", selected = character(0))
})
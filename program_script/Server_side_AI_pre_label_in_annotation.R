output$model_slide_image_condition <- renderText({
  paste("Model is trained by: ", AI_pre_label$model_slide_image_condition, sep="\n")
})

output$AI_pre_label_slide_input_type <- renderText({
  paste("Model training type: ", AI_pre_label$slide_input_type, sep="\n")
})

output$AI_pre_label_slide_input_layer <- renderText({
  paste("Model use layer: ", paste0(AI_pre_label$slide_input_layer, collapse = ", "), sep="\n")
})


observeEvent(input$AI_pre_label_model_group, {
  updateSelectInput(session, inputId = "AI_pre_label_model", choices = list.files(paste0("model/", input$AI_pre_label_model_group)))
})

observeEvent(input$AI_pre_label_model, {
  if (file.exists(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/detail.rds"))) {
    model_detail <- readRDS(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/detail.rds"))
    
    if (model_detail$image_or_slide == "Slide") {
      if (model_detail$slide_input_type == "Split train") {
        updateSelectizeInput(session, 
                             inputId = "AI_pre_label_in_slide_layer", 
                             choices = model_detail$slide_input_layer,
                             selected = model_detail$slide_input_layer[1],
                             options = list(maxItems = 1))
      } else {
        updateSelectizeInput(session, 
                             inputId = "AI_pre_label_in_slide_layer", 
                             choices = model_detail$slide_input_layer,
                             selected = model_detail$slide_input_layer,
                             options = list(maxItems = length(model_detail$slide_input_layer)))
      }
    } else {
      updateSelectizeInput(session, 
                           inputId = "AI_pre_label_in_slide_layer", 
                           choices = c("40x", 
                                       "20x",
                                       "10x",
                                       "5x",
                                       "2.5x",
                                       "1.25x",
                                       "0.625x",
                                       "0.3125x",
                                       "0.15625x",
                                       "0.078125x"),
                           selected = "40x",
                           options = list(maxItems = 1))
    }
    
    split_word <- c(model_detail$layer_class_id_P, model_detail$layer_class_id_G) %>% str_split("_")
    
    Primary_layer_ID <- sapply(split_word, function(x){
      x[1] %>% as.integer()
    })
    
    Secondary_layer_ID <- sapply(split_word, function(x){
      x[2] %>% as.integer()
    })
    
    Tertiary_layer_ID <- sapply(split_word, function(x){
      x[3] %>% as.integer()
    })
    
    option_word <- sapply(split_word, function(x){
      if (is.na(x[5])) {
        ""
      } else {
        paste0("_", x[5])
      }
    })
    
    layer_number <- length(Primary_layer_ID)
    
    AI_pre_label$model_slide_image_condition <- model_detail$image_or_slide
    AI_pre_label$slide_input_type <- model_detail$slide_input_type
    AI_pre_label$slide_input_layer <- model_detail$slide_input_layer
    
    AI_pre_label$output_table <- AI_pre_label$output_table[0,]
    
    AI_pre_label$output_table[1:layer_number, 1] <- paste0(Primary_layer_ID,
                                                           "_",
                                                           Secondary_layer_ID,
                                                           "_",
                                                           Tertiary_layer_ID,
                                                           "_",
                                                           paste0(model_detail$layer_option, option_word))
    #AI_pre_label$output_table[1:layer_number, 2] <- Secondary_layer_ID
    #AI_pre_label$output_table[1:layer_number, 3] <- Tertiary_layer_ID
    #AI_pre_label$output_table[1:layer_number, 4] <- paste0(model_detail$layer_option, option_word)
    AI_pre_label$output_table[1:layer_number, 2] <- model_detail$layer_meaning
    AI_pre_label$output_table[1:layer_number, 3] <- "Y"
    AI_pre_label$output_table[1:layer_number, 4] <- "R"
    AI_pre_label$output_table[1:layer_number, 5] <- "Y"
    
    for (option_row in 1:layer_number) {
      if (str_detect(model_detail$layer_option[option_row], "P")) {
        AI_pre_label$output_table[option_row, 6] <- "-"
        AI_pre_label$output_table[option_row, 7] <- paste0(Primary_layer_ID[option_row],
                                                            "_",
                                                            Secondary_layer_ID[option_row],
                                                            "_",
                                                            Tertiary_layer_ID[option_row],
                                                            "_P_",
                                                            option_word[option_row] %>% str_replace_all("_", ""))
        AI_pre_label$output_table[option_row, 8] <- "-"
      } else if (paste0(model_detail$layer_option[option_row], option_word[option_row]) == "G") {
        AI_pre_label$output_table[option_row, 6] <- paste0(Primary_layer_ID[option_row],
                                                           "_",
                                                           Secondary_layer_ID[option_row],
                                                           "_",
                                                           Tertiary_layer_ID[option_row],
                                                           "_G_",
                                                           1)
        AI_pre_label$output_table[option_row, 7] <- "-"
        AI_pre_label$output_table[option_row, 8] <- paste0(Primary_layer_ID[option_row],
                                                            "_",
                                                            Secondary_layer_ID[option_row],
                                                            "_",
                                                            Tertiary_layer_ID[option_row],
                                                            "_H_",
                                                            1)
      } else if (str_detect(model_detail$layer_option[option_row], "G")) {
        AI_pre_label$output_table[option_row, 6] <- paste0(Primary_layer_ID[option_row],
                                                           "_",
                                                           Secondary_layer_ID[option_row],
                                                           "_",
                                                           Tertiary_layer_ID[option_row],
                                                           "_G_",
                                                           option_word[option_row] %>% str_replace_all("_", ""))
        AI_pre_label$output_table[option_row, 7] <- "-" 
        AI_pre_label$output_table[option_row, 8] <- paste0(Primary_layer_ID[option_row],
                                                            "_",
                                                            Secondary_layer_ID[option_row],
                                                            "_",
                                                            Tertiary_layer_ID[option_row],
                                                            "_H_",
                                                            option_word[option_row] %>% str_replace_all("_", ""))
      }
    }
    AI_pre_label$output_table[, 9] <- 0.6
    AI_pre_label$output_table[, 10] <- 0 
    AI_pre_label$output_table[, 11] <- "-"
    AI_pre_label$output_table[, 12] <- "0.00005"
  } else {
    AI_pre_label$output_table[["Original_layer"]] <- ""
    AI_pre_label$output_table[["Layer_meaning"]] <- ""
    AI_pre_label$output_table[["Use_in_label"]] <- "N"
    AI_pre_label$output_table[["Replace_Add_Clip"]] <- "R"
    AI_pre_label$output_table[["Fill_hole"]] <- "Y"
    AI_pre_label$output_table[["Inference_G"]] <- "-"
    AI_pre_label$output_table[["Inference_P"]] <- "-"
    AI_pre_label$output_table[["Inference_H"]] <- "-"
    AI_pre_label$output_table[["Threshold"]] <- 0.6
    AI_pre_label$output_table[["Ignore_pixel_min"]] <- 0
    AI_pre_label$output_table[["Ignore_pixel_max"]] <- "-"
    AI_pre_label$output_table[["Polygon_simplify"]] <- "0.00005"
  }
})

AI_pre_label <- reactiveValues(model = "",
                               tissue_model = "",
                               model_name = "",
                               model_type = "",
                               model_detail = "",
                               model_slide_image_condition = "",
                               output_table = data.frame(#"Primary_layer_ID" = "",
                                                         #"Secondary_layer_ID" = "",
                                                         #"Tertiary_layer_ID" = "",
                                                         #"layer_option" = "",
                                                         "Original_layer" = "",
                                                         "Layer_meaning" = "",
                                                         "Use_in_label" = "Y",
                                                         "Replace_Add_Clip" = "R",
                                                         "Fill_hole" = "Y",
                                                         "Inference_G" = "-",
                                                         "Inference_P" = "-",
                                                         "Inference_H" = "-",
                                                         "Threshold" = 0.6,
                                                         "Ignore_pixel_min" = 0,
                                                         "Ignore_pixel_max" = "-",
                                                         "Polygon_simplify" = "0.00005",
                                                         stringsAsFactors = FALSE))

output$AI_pre_label_table <- renderRHandsontable(
  rhandsontable(AI_pre_label$output_table) %>% 
    #hot_col(col = "Primary_layer_ID", type = "dropdown", source = 1:20) %>% 
    #hot_col(col = "Secondary_layer_ID", type = "dropdown", source = 1:20) %>% 
    #hot_col(col = "Tertiary_layer_ID", type = "dropdown", source = 1:20) %>% 
    #hot_col(col = "layer_option", type = "dropdown", source = c("G", paste0("P_", 1:10))) %>% 
    hot_col(col = "Use_in_label", type = "dropdown", source = c("Y", "N")) %>% 
    hot_col(col = "Replace_Add_Clip", type = "dropdown", source = c("R", "A", "C")) %>% 
    hot_col(col = "Fill_hole", type = "dropdown", source = c("Y", "N")) %>% 
    #hot_col(col = "Inference_G", type = "dropdown", source = c("-", 1:10)) %>% 
    #hot_col(col = "Inference_P", type = "dropdown", source = c("-", 1:10)) %>% 
    #hot_col(col = "Inference_H", type = "dropdown", source = c("-", 1:10)) %>% 
    hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = 'white';
              td.style.color = 'black';
           }")
)

observeEvent(input$Start_AI_prelabel, {

Start <- Sys.time()

  progressSweetAlert(
    session = session, id = "AI_pre_label_myprogress",
    title = "Start AI labeling",
    display_pct = TRUE, value = 0
  )
  
  if (input$AI_pre_label_train_or_validation == "By Validation Set") {
    model_location <- paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/Model.h5")
  } else if (input$AI_pre_label_train_or_validation == "By Training Set") {
    model_location <- paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/Model_over.h5")
  }
  
  if (file.exists(model_location)) {
    if (AI_pre_label$model_name != paste0(input$AI_pre_label_model_group, "/", input$AI_pre_label_model) | AI_pre_label$model_type != input$AI_pre_label_train_or_validation) {
      
      updateProgressBar(
        session = session,
        title = "Loading model...",
        id = "AI_pre_label_myprogress",
        value = 10
      )
      
      AI_pre_label$model_name <- paste0(input$AI_pre_label_model_group, "/", input$AI_pre_label_model)
      AI_pre_label$model_type <- input$AI_pre_label_train_or_validation
      AI_pre_label$model_detail <- readRDS(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/detail.rds"))
      
      source(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/run_use.R"), local = TRUE)
      AI_pre_label$model <- load_model_weights_hdf5(object = get_unet(input_shape = list(NULL, NULL, 3 * length(input$AI_pre_label_in_slide_layer)),
                                                                      num_classes = nrow(hot_to_r(input$AI_pre_label_table))), 
                                                    filepath = model_location)
    }
    
    if (input$Slide_or_image == "Image") {
      
      updateProgressBar(
        session = session,
        title = "Model strat predicting...",
        id = "AI_pre_label_myprogress",
        value = 20
      )
      
      img_location <- paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input, "/")
      image_ori <- readImage(list.files(img_location, pattern = ".jpg", full.names = TRUE))
      image_ori2 <- image_ori@.Data %>% array_reshape(dim = c(1,dim(image_ori)))
      batch_result <- predict(AI_pre_label$model, image_ori2)
      batch_size <- dim(batch_result)[1]
      modified_label_table <- hot_to_r(input$AI_pre_label_table)
      batch_number <- 1
      #print(modified_label_table)
      
      #saveRDS(list(
      #  img_location = img_location,
      #  image_ori = image_ori,
      #  image_ori2 = image_ori2, 
      #  batch_result = batch_result,
      #  batch_size = batch_size, 
      #  modified_label_table = modified_label_table,
      #  batch_number = batch_number
      #), "test.rds")
      
      for (i in 1:nrow(modified_label_table)) {
        
        updateProgressBar(
          session = session,
          title = "Post predicting image processing...",
          id = "AI_pre_label_myprogress",
          value = i/length(AI_pre_label$model_detail$layer_option) * 50 + 20
        )
        
        if (modified_label_table$Use_in_label[i] == "Y") {
          if (str_detect(modified_label_table$Inference_G[i], "G")) {
            if (modified_label_table$Fill_hole[i] == "Y") {
              shapes <- batch_result[batch_number,,,i] %>% 
                replace(which(batch_result[batch_number,,,i] <= modified_label_table$Threshold[i]), 0) %>% 
                replace(which(batch_result[batch_number,,,i] > modified_label_table$Threshold[i]), 1) %>% fillHull()
              
              outer <- shapes %>% bwlabel()
              
              if (modified_label_table$Ignore_pixel_min[i] > 0) {
                pixel_condiiton <- outer %>% computeFeatures.shape() %>% as.data.frame()
                pixel_condiiton <- pixel_condiiton$s.area
                pixel_condiiton <- which(pixel_condiiton <= modified_label_table$Ignore_pixel_min[i])
                outer[which(outer %in% pixel_condiiton)] <- 0
                outer <- outer %>% bwlabel()
              }
              
              if (modified_label_table$Ignore_pixel_max[i] != "-") {
                pixel_condiiton <- outer %>% computeFeatures.shape() %>% as.data.frame()
                pixel_condiiton <- pixel_condiiton$s.area
                pixel_condiiton <- which(pixel_condiiton >= as.numeric(modified_label_table$Ignore_pixel_max[i]))
                outer[which(outer %in% pixel_condiiton)] <- 0
                outer <- outer %>% bwlabel()
              }
              
              if (max(outer) > 0) {
                outer_contour <- outer %>% ocontour()
                outer_contour <- lapply(outer_contour, function(x){
                  XY2LonLat(x[,1], 
                            x[,2])
                })
                outer_contour <- lapply(outer_contour, function(x){
                  raw_poly <- lapply(1:nrow(x), function(y){
                    list(x[y,1], x[y,2])
                  })
                  c(raw_poly, list(list(x[1,1], x[1,2])))
                })
                
                update_polygon_class <- modified_label_table$Inference_G[i]
                
                x_chunk <- 1
                y_chunk <- 1
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
              }
              inner_geojson <- list()
              if (length(outer_geojson) > 0) {
                outer_export_list <- list("Check" = list(NULL, NULL),
                                          "Done" = list(outer_geojson, NULL))
              } else {
                outer_export_list <- list("Check" = list(NULL, NULL),
                                          "Done" = list(NULL, NULL))
              }
              
              if (length(inner_geojson) > 0) {
                inner_export_list <- list("Check" = list(NULL, NULL),
                                          "Done" = list(inner_geojson, NULL))
              } else {
                inner_export_list <- list("Check" = list(NULL, NULL),
                                          "Done" = list(NULL, NULL))
              }
            } else if (modified_label_table$Fill_hole[i] == "N") {
              shapes <- batch_result[batch_number,,,i] %>% 
                replace(which(batch_result[batch_number,,,i] <= modified_label_table$Threshold[i]), 0) %>% 
                replace(which(batch_result[batch_number,,,i] > modified_label_table$Threshold[i]), 1)
              
              outer <- shapes %>% bwlabel()
              inner <- (shapes %>% fillHull() - shapes) %>% bwlabel()
              
              if (modified_label_table$Ignore_pixel_min[i] > 0) {
                pixel_condiiton <- outer %>% computeFeatures.shape() %>% as.data.frame()
                pixel_condiiton <- pixel_condiiton$s.area
                pixel_condiiton <- which(pixel_condiiton <= modified_label_table$Ignore_pixel_min[i])
                outer[which(outer %in% pixel_condiiton)] <- 0
                outer <- outer %>% bwlabel()
                
                pixel_condiiton <- inner %>% computeFeatures.shape() %>% as.frame.frame()
                pixel_condiiton <- pixel_condiiton$s.area
                pixel_condiiton <- which(pixel_condiiton <= modified_label_table$Ignore_pixel_min[i])
                inner[which(inner %in% pixel_condiiton)] <- 0
                inner <- inner %>% bwlabel()
              }
              
              if (modified_label_table$Ignore_pixel_max[i] != "-") {
                pixel_condiiton <- outer %>% computeFeatures.shape() %>% as.data.frame()
                pixel_condiiton <- pixel_condiiton$s.area
                pixel_condiiton <- which(pixel_condiiton >= as.numeric(modified_label_table$Ignore_pixel_max[i]))
                outer[which(outer %in% pixel_condiiton)] <- 0
                outer <- outer %>% bwlabel()
                
                pixel_condiiton <- inner %>% computeFeatures.shape() %>% as.data.frame()
                pixel_condiiton <- pixel_condiiton$s.area
                pixel_condiiton <- which(pixel_condiiton >= as.numeric(modified_label_table$Ignore_pixel_max[i]))
                inner[which(inner %in% pixel_condiiton)] <- 0
                inner <- inner %>% bwlabel()
              }
              
              if (max(outer) > 0) {
                outer_contour <- outer %>% ocontour()
                outer_contour <- lapply(outer_contour, function(x){
                  XY2LonLat(x[,1], 
                            x[,2])
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
                  XY2LonLat(x[,1], 
                            x[,2])
                })
                inner_contour <- lapply(inner_contour, function(x){
                  raw_poly <- lapply(1:nrow(x), function(y){
                    list(x[y,1], x[y,2])
                  })
                  c(raw_poly, list(list(x[1,1], x[1,2])))
                })
                
                update_polygon_class <- modified_label_table$Inference_G[i]
                
                x_chunk <- 1
                y_chunk <- 1
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
              }

              if (length(outer_geojson) > 0) {
                outer_export_list <- list("Check" = list(NULL, NULL),
                                          "Done" = list(outer_geojson, NULL))
              } else {
                outer_export_list <- list("Check" = list(NULL, NULL),
                                          "Done" = list(NULL, NULL))
              }
              
              if (length(inner_geojson) > 0) {
                inner_export_list <- list("Check" = list(NULL, NULL),
                                          "Done" = list(inner_geojson, NULL))
              } else {
                inner_export_list <- list("Check" = list(NULL, NULL),
                                          "Done" = list(NULL, NULL))
              }
            }
            
            outer_all_polygon_df <- list(outer_export_list)
            
            inner_all_polygon_df <- list(inner_export_list)
            
            outer_poly <- cluster_geojson_merge(outer_all_polygon_df)
            inner_poly <- cluster_geojson_merge(inner_all_polygon_df)
            
            merge_poly_result <- combine_outer_and_inner_poly_function(outer_all_polygon_df = outer_poly, 
                                                                       inner_all_polygon_df = inner_poly)
            
            for (ii in 1:length(merge_poly_result)) {
              if (ii == 1) {
                merge_all_polygon <- merge_poly_result[[1]]
                merge_all_polygon$geometry$type <- "MultiPolygon"
                merge_all_polygon$geometry$coordinates <- list()
                if (class(merge_poly_result[[ii]]$geometry$coordinates) == "array") {
                  merge_all_polygon$geometry$coordinates[[1]] <- merge_poly_result[[ii]]$geometry$coordinates
                } else if (class(merge_poly_result[[ii]]$geometry$coordinates) == "list") {
                  merge_all_polygon$geometry$coordinates[[1]] <- merge_poly_result[[ii]]$geometry$coordinates
                }
              } else {
                if (class(merge_poly_result[[ii]]$geometry$coordinates) == "array") {
                  merge_all_polygon$geometry$coordinates[[length(merge_all_polygon$geometry$coordinates) + 1]] <- merge_poly_result[[ii]]$geometry$coordinates
                } else if (class(merge_poly_result[[ii]]$geometry$coordinates) == "list") {
                  if (sum(class(merge_poly_result[[ii]]$geometry$coordinates[[1]]) == "matrix") > 0) {
                    merge_all_polygon$geometry$coordinates <- c(merge_all_polygon$geometry$coordinates, list(merge_poly_result[[ii]]$geometry$coordinates))
                  } else {
                    merge_all_polygon$geometry$coordinates <- c(merge_all_polygon$geometry$coordinates, merge_poly_result[[ii]]$geometry$coordinates)
                  }
                }
              }
            }
            
            #print("2")
            #print(modified_label_table$Replace_Add_Clip[i])
            
            if (modified_label_table$Replace_Add_Clip[i] == "R") {
              Polygons_data_current_on_map$list[[update_polygon_class]] <- geojson_split_function(merge_all_polygon) 
            } else if (modified_label_table$Replace_Add_Clip[i] == "A") {
              if (length(Polygons_data_current_on_map$list[[update_polygon_class]]) == 0) {
                Polygons_data_current_on_map$list[[update_polygon_class]] <- geojson_split_function(merge_all_polygon)
              } else {
                merge_all_current_polygon <- geojson_merge_function(Polygons_data_current_on_map$list[[update_polygon_class]],
                                                                    method = "exact")
                
                cut_polygon <- chunk_polygon 
                cut_polygon <- cut_polygon %>% lawn_difference(merge_all_current_polygon) %>% lawn_difference(merge_all_polygon)
                union_all_polygon <- lawn_difference(chunk_polygon, cut_polygon)
                split_all_polygon <- geojson_split_function(union_all_polygon)
                
                Polygons_data_current_on_map$list[[update_polygon_class]] <- split_all_polygon
                
                original_bbox <- lapply(Polygons_data_current_on_map$list[[update_polygon_class]], function(x){
                  lawn_bbox(x)
                })
              }
            } else if (modified_label_table$Replace_Add_Clip[i] == "C") {
              if (length(Polygons_data_current_on_map$list[[update_polygon_class]]) == 0) {
              } else {
                merge_all_current_polygon <- geojson_merge_function(Polygons_data_current_on_map$list[[update_polygon_class]],
                                                                    method = "exact")
                union_all_polygon <- lawn_difference(merge_all_current_polygon, merge_all_polygon)
                split_all_polygon <- geojson_split_function(union_all_polygon)
                
                Polygons_data_current_on_map$list[[update_polygon_class]] <- split_all_polygon
                
                original_bbox <- lapply(Polygons_data_current_on_map$list[[update_polygon_class]], function(x){
                  lawn_bbox(x)
                })
              }
            }
          } 
          
          if (str_detect(modified_label_table$Inference_P[i], "P")) {
            if (modified_label_table$Fill_hole[i] == "Y") {
              pred_result <- batch_result[batch_number,,,i] %>% 
                replace(which(batch_result[batch_number,,,i] <= modified_label_table$Threshold[i]), 0) %>% 
                replace(which(batch_result[batch_number,,,i] > modified_label_table$Threshold[i]), 1) %>% 
                fillHull() %>% 
                bwlabel() 
            } else if (modified_label_table$Fill_hole[i] == "N") {
              pred_result <- batch_result[batch_number,,,i] %>% 
                replace(which(batch_result[batch_number,,,i] <= modified_label_table$Threshold[i]), 0) %>% 
                replace(which(batch_result[batch_number,,,i] > modified_label_table$Threshold[i]), 1) %>% 
                bwlabel() 
            }
            
            if (modified_label_table$Ignore_pixel_min[i] > 0) {
              pixel_condiiton <- pred_result %>% computeFeatures.shape() %>% as.data.frame()
              pixel_condiiton <- pixel_condiiton$s.area
              pixel_condiiton <- which(pixel_condiiton <= modified_label_table$Ignore_pixel_min[i])
              pred_result[which(pred_result %in% pixel_condiiton)] <- 0
              pred_result <- pred_result %>% bwlabel()
            }
            
            pred_result <- pred_result %>% computeFeatures.moment()
            blank_point <- readRDS("store_data_for_program/blank_leaflet_points.rds")
            
            if (length(pred_result) > 0) {
              pred_result <- XY2LonLat(x = pred_result[,1], 
                                       y = pred_result[,2])
              
              update_point_class <- modified_label_table$Inference_P[i]
              
              point_row <- nrow(pred_result)
              
              blank_point[1:point_row, 1] <- (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[4:5] %>% paste0(collapse = "_")
              blank_point[1:point_row, 3] <- pred_result$lon
              blank_point[1:point_row, 4] <- pred_result$lat
              blank_point[1:point_row, 5] <- (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[1]
              blank_point[1:point_row, 6] <- (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[2]
              blank_point[1:point_row, 7] <- (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[3]
              blank_point[, 2] <- paste0(blank_point[, 1], "_", blank_point[, 3], "_", blank_point[, 4])
            }
            
            update_point_class <- paste0(modified_label_table$Inference_P[i])
            
            all_point_df <- blank_point
            if (nrow(all_point_df) > 0) {
              #i <- 999
              if (modified_label_table$Replace_Add_Clip[i] == "R") {
                Points_data_current_on_map$df <- Points_data_current_on_map$df %>% 
                  filter(point_primary != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[1] | 
                           point_secondary != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[2] | 
                           point_tertiary != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[3] | 
                           point_group != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[4:5] %>% paste0(collapse = "_"))
                Points_data_current_on_map$df <- rbind(Points_data_current_on_map$df, all_point_df)
              } else if (modified_label_table$Replace_Add_Clip[i] == "A") {
                Points_data_current_on_map$df <- rbind(Points_data_current_on_map$df, all_point_df)
              } else if (modified_label_table$Replace_Add_Clip[i] == "C") {
                Points_data_current_on_map$df <- Points_data_current_on_map$df %>% filter((point_id %in% all_point_df[, 2]) == FALSE)
              }
            } else {
              if (modified_label_table$Replace_Add_Clip[i] == "R") {
                Points_data_current_on_map$df <- Points_data_current_on_map$df %>% 
                  filter(point_primary != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[1] | 
                           point_secondary != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[2] | 
                           point_tertiary != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[3] | 
                           point_group != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[4:5] %>% paste0(collapse = "_"))
              }
            }
          }
        }
      }
      
      updateProgressBar(
        session = session,
        title = "Saving result...",
        id = "AI_pre_label_myprogress",
        value = 90
      )
      
      current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
      modified_polygon_list <- Polygons_data_current_on_map$list[[current_polygon_class]]
      polygon_names <- Polygons_data_current_on_map$list[[current_polygon_class]] %>% names()
      if (length(modified_polygon_list) > 0) {
        Polygons_data_current_on_map$bbox <- lapply(modified_polygon_list, function(x){
          lawn_bbox(x)
        })
      } else {
        Polygons_data_current_on_map$bbox <- list()
      }
      
      show_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
      show_polygon_list <- Polygons_data_current_on_map$list[str_detect(names(Polygons_data_current_on_map$list), 
                                                                        paste0(show_polygon_class, "_G_"))]
      
      modified_point_table <- Points_data_current_on_map$df %>% 
        filter(point_primary == input$Point_class_1) %>% 
        filter(point_secondary == input$Point_class_2) %>% 
        filter(point_tertiary == input$Point_class_3)
      
      leafletProxy("image_viewer", session) %>% 
        clearGeoJSON() %>% 
        clearMarkers()

      #print(polygon_names)
      #print(modified_polygon_list)
      
      polygon_show_name <- names(show_polygon_list)
      if (length(polygon_show_name) > 0) {
        polygon_show_color_code <- str_replace_all(polygon_show_name, paste0(paste0(show_polygon_class, "_G_")), "")
        
        for (k in polygon_show_name) {
          polygon_names <- Polygons_data_current_on_map$list[[k]] %>% names()
          color_number <- str_replace_all(k, paste0(paste0(show_polygon_class, "_G_")), "")
          
          isolate({
            geocolor <- input$Change_polygon_color_order[as.integer(color_number)]
            geoopcity <- input$Polygon_opacity_value
          })
          
          for (i in polygon_names) {
            leafletProxy("image_viewer", session) %>% 
              addGeoJSON(Polygons_data_current_on_map$list[[k]][[i]], 
                         layerId = i, 
                         group = paste0("G_", color_number),
                         fillOpacity = input$Polygon_opacity_value, 
                         color = input$Change_polygon_color_order[as.integer(color_number)], 
                         options = leafletOptions(pane = "Polygons"))
          }
        }
      }
      
      leafletProxy("image_viewer", session) %>% 
        addCircleMarkers(lng = as.numeric(modified_point_table$point_lon), 
                         lat = as.numeric(modified_point_table$point_lat), 
                         radius = sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting, 
                         fillOpacity = (1 - sum(input$Point_circle_switch)), 
                         color = input$Change_point_color_order[as.integer(modified_point_table$point_group %>% str_replace_all("P_", ""))], 
                         group = modified_point_table$point_group, 
                         layerId = modified_point_table$point_id,
                         options = leafletOptions(pane = "Points"))
      
      
      updateProgressBar(
        session = session,
        title = "Updating result to image...",
        id = "AI_pre_label_myprogress",
        value = 100
      )
    } else if (input$Slide_or_image == "Slide") {
      
      updateProgressBar(
        session = session,
        title = "Model strat predicting...",
        id = "AI_pre_label_myprogress",
        value = 20
      )
      
      img_location <- paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input, "/")
      image_name <- list.files(img_location, pattern = ".ndpi|.isyntax|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif", full.names = TRUE)
	  
#	  if(str_detect(image_name, ".ndpi")==TRUE){
#	     slide_info <- NDPI_info(image_name)
#	  }else{
#	     slide_info <- readRDS(file = paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input, "/info/info.rds")) 
#	  }
      slide_info <- if(str_detect(image_name, ".ndpi")) NDPI_info(image_name) else readRDS(file = paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input, "/info/info.rds")) 

      tile_size <- input$AI_pre_label_in_slide_tile_size %>% as.integer()
      batch_size <- input$AI_pre_label_in_slide_CPU_modify_batch_size %>% as.integer()
      sample_layer <- log2((input$AI_pre_label_in_slide_layer %>% str_replace("x", "") %>% as.numeric())/0.078125) + 3
      check_layer <- max(sample_layer)
      shifting_chunk <- (tile_size * 2^(12 - check_layer))/2
      center_move_pixel <- tile_size * 2^(12 - check_layer)
      tile_x_number <- ceiling(slide_info$Width_pixel/center_move_pixel)
      tile_y_number <- ceiling(slide_info$Height_pixel/center_move_pixel)
      temp_folder_location <- paste0(img_location, "AI_temp")
      modified_label_table <- hot_to_r(input$AI_pre_label_table)
	  image_file_root <- paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input)
	  slide_info_file <- readRDS(file = paste0(image_file_root, "/info/info.rds"))
#To increase execution speed, Numba converts Python byte code to machine code immediately before execution.
#Numba can be used to optimize CPU and GPU functions using callable Python objects called decorators. A decorator is a function that takes another function as input, modifies it, and returns the modified function to the user. This modularity reduces programming time and increases Python's extensibility.	  
	   numba$vectorize(target="parallel", nopython=TRUE)
	   #R speeds up when the Basic Linear Algebra System (BLAS) it uses is well tuned.
	   blas_set_num_threads(parallel::detectCores())
	   
	  
      opened_slide <- openslide$OpenSlide(image_name)
      run_gpu_batch <- input$AI_pre_label_in_slide_batch_size %>% as.integer()
      
      if (file.exists(temp_folder_location) == FALSE) {
        dir.create(temp_folder_location)
      } else {
        rm_file <- list.files(temp_folder_location, full.names = TRUE)
        file.remove(rm_file)
      }
	  
	 ifelse(file.exists(temp_folder_location),file.remove(list.files(temp_folder_location, full.names = TRUE)),dir.create(temp_folder_location))
	  
      #polygon_threshold <- input$AI_pre_label_polygon_threshold
      #point_threshold <- input$AI_pre_label_point_threshold
      #polygon_simplify <- input$AI_pre_label_polygon_simplify
      overlap_pixel <- input$AI_pre_label_in_slide_overlap_pixel %>% as.integer()
      
      #saveRDS(modified_label_table, "modified_label_table.rds")
      
      future_table <- data.frame(
        center_x = integer(tile_x_number * tile_y_number),
        center_y = integer(tile_x_number * tile_y_number),
        base_layer = integer(tile_x_number * tile_y_number)
      )
      #modified_label_table <- hot_to_r(input$AI_pre_label_table)

      df_count <- 0
      for (x in 1:tile_x_number) {
        for (y in 1:tile_y_number) {
          df_count <- df_count + 1
          future_table[df_count, "center_x"] <- (x - 1) * center_move_pixel + center_move_pixel/2
          future_table[df_count, "center_y"] <- (y - 1) * center_move_pixel + center_move_pixel/2
          future_table[df_count, "base_layer"] <- check_layer
        }
      }
	  
      if (input$Tissue_only_or_All_slide_switch && input$AI_labeled_restricted_area == "No") {
	  #----#
#	  if(str_detect(image_name, ".ndpi")==TRUE){
#         thumb_image <- NDPI_center_RGB_batch_Num(file_name = image_name,
#                                                  all_x_pixel = 1024 * 2^(12 - 5),
#                                                  all_y_pixel = 1024 * 2^(12 - 5),
#                                                  all_pyramid_layer = 5,
#                                                  tilesize = 2048)									  
#	  }else{
#		thumb_image <- NDPI_center_RGB_batch_Num_function_Background(opened_slide,image_name,image_file_root,131072L,131072L,5L,2048L,slide_info_file)
#	  }

      #In addition to the slide file .ndpi, other slide files can be supported through the openslide package.
      thumb_image <- if(str_detect(image_name, ".ndpi")) NDPI_center_RGB_batch_Num(file_name = image_name,all_x_pixel = 1024 * 2^(12 - 5),all_y_pixel = 1024 * 2^(12 - 5),all_pyramid_layer = 5,tilesize = 2048) else  NDPI_center_RGB_batch_Num_function_Background(opened_slide,image_name,image_file_root,131072L,131072L,5L,2048L,slide_info_file)

	  #----#
		 
        if (AI_pre_label$tissue_model == "") {
          source("store_data_for_program/Tissue_Model.R", local = TRUE)
          AI_pre_label$tissue_model <- load_model_weights_hdf5(object = get_tissue(num_classes = 1), filepath = "store_data_for_program/Tissue_Model.h5")
        }
		
        thumb_image <- AI_pre_label$tissue_model$predict(thumb_image)[1,,,1]
		
        thumb_image <- replace(thumb_image, which(thumb_image >= input$Tissue_only_threshold), 1) %>% 
          replace(which(thumb_image < input$Tissue_only_threshold), 0) %>% 
          dilate(kern = makeBrush(size = input$Tissue_only_dilate_pixel, shape = "disc"))

        thumb_tile_number <- 2^(check_layer - 5) * 2 * (1024 / tile_size)
        tile_chunk <- untile(thumb_image, c(thumb_tile_number, thumb_tile_number),lwd = 0)
        thumb_image <- sapply(1:(thumb_tile_number^2), function(x){max(tile_chunk[,,x])}) %>% array(c(thumb_tile_number, thumb_tile_number))
		
		thumb_array <- which(thumb_image > 0, arr.ind = TRUE)
        if(length(thumb_array)==0){
          thumb_array <- which(thumb_image >= 0, arr.ind = TRUE)
        }
		
        thumb_table <- data.frame(
          center_x = (thumb_array[,1] - 1) * center_move_pixel + center_move_pixel/2,
          center_y = (thumb_array[,2] - 1) * center_move_pixel + center_move_pixel/2,
          base_layer = check_layer
        )
        

		
        future_table <- inner_join(future_table, thumb_table)
        

      } else if (input$AI_labeled_restricted_area == "Yes only") {

        thumb_image <- thumb_restricted_region_function(slide_file = image_name,
                                                        polygon_layer_select = paste0(input$AI_pre_label_model_restricted_primary_layer, 
                                                                                      "_",
                                                                                      input$AI_pre_label_model_restricted_secondary_layer,
                                                                                      "_",
                                                                                      input$AI_pre_label_model_restricted_tertiary_layer))
		
		
		for (resize_number in 9:(check_layer - 5)) {
          thumb_image <- thumb_image %>% 
            resize(w = 2^(resize_number) * 2 * (1024 / tile_size),
                   h = 2^(resize_number) * 2 * (1024 / tile_size), filter = "bilinear")
        }

		thumb_array <- which(thumb_image > 0, arr.ind = TRUE)
        
        
        thumb_table <- data.frame(
          center_x = (thumb_array[,1] - 1) * center_move_pixel + center_move_pixel/2,
          center_y = (thumb_array[,2] - 1) * center_move_pixel + center_move_pixel/2,
          base_layer = check_layer
        )

        future_table <- inner_join(future_table, thumb_table)
      } else if (input$AI_labeled_restricted_area == "Yes plus tissue") {

        thumb_image1 <- NDPI_center_RGB_batch_Num(file_name = image_name,
                                                  all_x_pixel = 1024 * 2^(12 - 5),
                                                  all_y_pixel = 1024 * 2^(12 - 5),
                                                  all_pyramid_layer = 5,
                                                  tilesize = 2048)
	   

	   
        if (AI_pre_label$tissue_model == "") {
          source("store_data_for_program/Tissue_Model.R", local = TRUE)
          AI_pre_label$tissue_model <- load_model_weights_hdf5(object = get_tissue(num_classes = 1), filepath = "store_data_for_program/Tissue_Model.h5")
        }
        thumb_image1 <- AI_pre_label$tissue_model$predict(thumb_image1)[1,,,1]
        
        #saveRDS(thumb_image1, "test1.rds")
        
        thumb_image1 <- replace(thumb_image1, which(thumb_image1 >= input$Tissue_only_threshold), 1) %>% 
          replace(which(thumb_image1 < input$Tissue_only_threshold), 0) %>% 
          dilate(kern = makeBrush(size = input$Tissue_only_dilate_pixel, shape = "disc")) %>% 
          resize(w = 2^(check_layer - 5) * 2 * (1024 / tile_size),
                 h = 2^(check_layer - 5) * 2 * (1024 / tile_size), filter = "bilinear")
        
        thumb_image2 <- thumb_restricted_region_function(slide_file = image_name,
                                                         polygon_layer_select = paste0(input$AI_pre_label_model_restricted_primary_layer, 
                                                                                       "_",
                                                                                       input$AI_pre_label_model_restricted_secondary_layer,
                                                                                       "_",
                                                                                       input$AI_pre_label_model_restricted_tertiary_layer))
        for (resize_number in 9:(check_layer - 5)) {
          thumb_image2 <- thumb_image2 %>% 
            resize(w = 2^(resize_number) * 2 * (1024 / tile_size),
                   h = 2^(resize_number) * 2 * (1024 / tile_size), filter = "bilinear")
        }
        
        thumb_image <- thumb_image1 + thumb_image2
        
        thumb_array <- which(thumb_image > 0, arr.ind = TRUE)
        #print(thumb_array)
        thumb_table <- data.frame(
          center_x = (thumb_array[,1] - 1) * center_move_pixel + center_move_pixel/2,
          center_y = (thumb_array[,2] - 1) * center_move_pixel + center_move_pixel/2,
          base_layer = check_layer
        )

        future_table <- inner_join(future_table, thumb_table)
      }

      total_check <- ceiling(nrow(future_table)/batch_size)
      #i in 1:ceiling(nrow(future_table)/batch_size)
      #check_future_table <- data.frame(
      #  x = (future_table$center_x - shifting_chunk)/(2^(12 - check_layer))/tile_size,
      #  y = (future_table$center_y - shifting_chunk)/(2^(12 - check_layer))/tile_size,
      #  base_layer = check_layer
      #)
      #saveRDS(check_future_table, paste0(temp_folder_location, "/future_table.rds"))
      
      for (i in 1:total_check) {
        
#        if (i != total_check) {
#          counting_up <- ((i - 1) * batch_size + 1):(i * batch_size)
#        } else {
#          counting_up <- ((i - 1) * batch_size + 1):nrow(future_table)
#        }
		
		counting_up <- if(i != total_check) ((i - 1) * batch_size + 1):(i * batch_size) else ((i - 1) * batch_size + 1):nrow(future_table)
		
        check_file_resolve <- paste0(temp_folder_location, "/check_", i, ".rds")
        saveRDS(i, check_file_resolve)



		
        if (length(sample_layer) == 1) {
		
		#In addition to the slide file .ndpi, other slide files can be supported through the openslide package.
        
        #Detection file .ndpi part		
        if(str_detect(image_name, ".ndpi")==TRUE){
		  
		  image_use <- lapply(counting_up, function(x) {
            NDPI_center_RGB_batch_Num(file_name = image_name,
                                      all_x_pixel = future_table$center_x[x],
                                      all_y_pixel = future_table$center_y[x],
                                      all_pyramid_layer = sample_layer,
                                      tilesize = tile_size + overlap_pixel*2)
           })
		 }else{ 
		  
          image_use <- lapply(counting_up, function(x) {
		    #Detect other file parts
			NDPI_center_RGB_batch_Num_function(opened_slide,image_name,image_file_root,future_table$center_x[x],future_table$center_y[x],sample_layer,tile_size,overlap_pixel,input$AI_pre_label_in_slide_layer,slide_info_file)
		  })
         }
		  
		  
        } else {
          image_use <- lapply(counting_up, function(x) {
            NDPI_center_RGB_pyramid_batch_Num(file_name = image_name,
                                              x_pixel = future_table$center_x[x],
                                              y_pixel = future_table$center_y[x],
                                              all_pyramid_layer = sample_layer,
                                              tilesize = tile_size + overlap_pixel*2)
          })
        }
        predict_result <- lapply(image_use, function(x) {
		  numba$vectorize(target="parallel", nopython=TRUE)
          AI_pre_label$model$predict(x, batch_size = as.integer(input$AI_pre_label_in_slide_batch_size))
        })

        #if (i %in% c(4, 12, 18, 19)) {
        # saveRDS(list(batch_result = abind::abind(predict_result, along = 1),
        #               deviation_x = (future_table$center_x[counting_up] - shifting_chunk)/2^(12 - check_layer) - overlap_pixel,
        #               deviation_y = (future_table$center_y[counting_up] - shifting_chunk)/2^(12 - check_layer) - overlap_pixel,
        #               basal_layer = check_layer,
        #               overlap_pixel = overlap_pixel,
        #               #polygon_threshold = polygon_threshold,
        #               #point_threshold = point_threshold,
        #               #polygon_simplify = polygon_simplify,
        #               modified_label_table = modified_label_table,
        #               temp_AI = temp_folder_location,
        #               check_file_resolve = check_file_resolve),
        #          paste0("list_", i, ".rds"))
        #}
        
        future({
          slide_result_function(batch_result = abind::abind(predict_result, along = 1),
                                deviation_x = (future_table$center_x[counting_up] - shifting_chunk)/2^(12 - check_layer) - overlap_pixel,
                                deviation_y = (future_table$center_y[counting_up] - shifting_chunk)/2^(12 - check_layer) - overlap_pixel,
                                basal_layer = check_layer,
                                overlap_pixel = overlap_pixel,
                                #polygon_threshold = polygon_threshold,
                                #point_threshold = point_threshold,
                                #polygon_simplify = polygon_simplify,
                                modified_label_table = modified_label_table,
                                temp_AI = temp_folder_location,
                                check_file_resolve = check_file_resolve)
        }, seed = NULL)
        
        updateProgressBar(
          session = session,
          title = paste0("Model start predicting...(", i, "/", total_check, ")"),
          id = "AI_pre_label_myprogress",
          value = 20 + 60/total_check * i
        )

	   mem_release()
	   rm(counting_up,check_file_resolve,image_use,predict_result)
       .Internal(gc(verbose = getOption("verbose"), reset = TRUE, full = TRUE))
		
      }
      
      updateProgressBar(
        session = session,
        title = paste0("Wait for all core/thread to resolve...(0/120[max check times])"),
        id = "AI_pre_label_myprogress",
        value = 90
      )
      
      check_time <- 0
      while((list.files(temp_folder_location, pattern = "check") %>% length() != 0) & check_time < 120) {
        Sys.sleep(5)
        check_time <- check_time + 1
        updateProgressBar(
          session = session,
          title = paste0("Wait for all core/thread to resolve...(", check_time, "/120[max check times])"),
          id = "AI_pre_label_myprogress",
          value = 90
        )
      }
      #resolve(last_future)
      
      for (i in 1:nrow(modified_label_table)) {
        updateProgressBar(
          session = session,
          title = paste0("Saving result...(", i, "/", nrow(modified_label_table), ")"),
          id = "AI_pre_label_myprogress",
          value = 90 + 10/(nrow(modified_label_table) + 1) * i
        )
        
        if (modified_label_table$Use_in_label[i] == "Y") {
          if (modified_label_table$Inference_G[i] != "-") {
            update_polygon_class <- modified_label_table$Inference_G[i]
            
            change_polygon_class <- (modified_label_table$Inference_G[i] %>% str_split("_") %>% unlist())[1:3] %>% paste0(collapse = "_")
            
            all_polygon_df_file_location <- list.files(temp_folder_location, 
                                                       pattern = paste0(update_polygon_class, "_"),
                                                       full.names = TRUE)
            
            if (length(all_polygon_df_file_location) > 0) {
              poly_location <- XY2LonLat(x = base::c(0,
                                                     slide_info$Width_pixel,
                                                     slide_info$Width_pixel,
                                                     0,
                                                     0),
                                         y = base::c(0,
                                                     0,
                                                     slide_info$Height_pixel,
                                                     slide_info$Height_pixel,
                                                     0),
                                         zoom = 12 + 1)
              
              chunk_polygon <- lawn_polygon(list(list(
                base::c(poly_location[1,1], poly_location[1,2]),
                base::c(poly_location[2,1], poly_location[2,2]),
                base::c(poly_location[3,1], poly_location[3,2]),
                base::c(poly_location[4,1], poly_location[4,2]),
                base::c(poly_location[5,1], poly_location[5,2])
              )))
              
              cut_polygon <- chunk_polygon 
              
              all_polygon_df <- lapply(all_polygon_df_file_location, function(x) {
                x %>% readRDS()
              })
              
              #saveRDS(all_polygon_df, "test.rds")
              
              outer_all_polygon_df <- lapply(all_polygon_df, function(x){
                x$outer
              })
              
              inner_all_polygon_df <- lapply(all_polygon_df, function(x){
                x$inner
              })
              
              outer_poly <- cluster_geojson_merge(outer_all_polygon_df)
              inner_poly <- cluster_geojson_merge(inner_all_polygon_df)
              
              merge_poly_result <- combine_outer_and_inner_poly_function(outer_all_polygon_df = outer_poly, 
                                                                         inner_all_polygon_df = inner_poly)
              
              
              #for (cut_poly in 1:length(all_polygon_df)) {
              #  cut_polygon <- lawn_difference(cut_polygon, all_polygon_df[[cut_poly]])
              #}
              
              #print("1")
              
              for (ii in 1:length(merge_poly_result)) {
			    merge_poly_result_simplify<- merge_poly_result[[ii]]$geometry$coordinates 
                if (ii == 1) {
                  merge_all_polygon <- merge_poly_result[[1]]
                  merge_all_polygon$geometry$type <- "MultiPolygon"
                  merge_all_polygon$geometry$coordinates <- list()
                  if (class(merge_poly_result_simplify) == "array") {
                    merge_all_polygon$geometry$coordinates[[1]] <- merge_poly_result_simplify
                  } else if (class(merge_poly_result_simplify) == "list") {
                    merge_all_polygon$geometry$coordinates[[1]] <- merge_poly_result_simplify
                  }
                } else {
                  if (class(merge_poly_result_simplify) == "array") {
                    merge_all_polygon$geometry$coordinates[[length(merge_all_polygon$geometry$coordinates) + 1]] <- merge_poly_result_simplify
                  } else if (class(merge_poly_result_simplify) == "list") {
                    if (sum(class(merge_poly_result_simplify[[1]]) == "matrix") > 0) {
                      merge_all_polygon$geometry$coordinates <- c(merge_all_polygon$geometry$coordinates, list(merge_poly_result_simplify))
                    } else {
                      merge_all_polygon$geometry$coordinates <- c(merge_all_polygon$geometry$coordinates, merge_poly_result_simplify)
                    }
                  }
                }
              }
              
              #print("2")
              #print(modified_label_table$Replace_Add_Clip[i])
              Replace_Add_Clip_simplify <- modified_label_table$Replace_Add_Clip[i]
			  
              if (Replace_Add_Clip_simplify == "R") {
                Polygons_data_current_on_map$list[[update_polygon_class]] <- geojson_split_function(merge_all_polygon) 
              } else if (Replace_Add_Clip_simplify == "A") {
                if (length(Polygons_data_current_on_map$list[[update_polygon_class]]) == 0) {
                  Polygons_data_current_on_map$list[[update_polygon_class]] <- geojson_split_function(merge_all_polygon)
                } else {
                  merge_all_current_polygon <- geojson_merge_function(Polygons_data_current_on_map$list[[update_polygon_class]],
                                                                      method = "exact")
                  
                  cut_polygon <- chunk_polygon 
                  cut_polygon <- cut_polygon %>% lawn_difference(merge_all_current_polygon) %>% lawn_difference(merge_all_polygon)
                  union_all_polygon <- lawn_difference(chunk_polygon, cut_polygon)
                  split_all_polygon <- geojson_split_function(union_all_polygon)
                  
                  Polygons_data_current_on_map$list[[update_polygon_class]] <- split_all_polygon
                  
                  original_bbox <- lapply(Polygons_data_current_on_map$list[[update_polygon_class]], function(x){
                    lawn_bbox(x)
                  })
                }
              } else if (Replace_Add_Clip_simplify == "C") {
                if (length(Polygons_data_current_on_map$list[[update_polygon_class]]) == 0) {
                } else {
                  merge_all_current_polygon <- geojson_merge_function(Polygons_data_current_on_map$list[[update_polygon_class]],
                                                                      method = "exact")
                  union_all_polygon <- lawn_difference(merge_all_current_polygon, merge_all_polygon)
                  split_all_polygon <- geojson_split_function(union_all_polygon)
                  
                  Polygons_data_current_on_map$list[[update_polygon_class]] <- split_all_polygon
                  
                  original_bbox <- lapply(Polygons_data_current_on_map$list[[update_polygon_class]], function(x){
                    lawn_bbox(x)
                  })
                }
              }
              
              file.remove(all_polygon_df_file_location)
            }
          } 
          Inference_P_simplify <- modified_label_table$Inference_P[i] 
          if (Inference_P_simplify != "-") {
            update_point_class <- paste0(Inference_P_simplify, "_")
            
            all_point_df_file_location <- list.files(temp_folder_location, 
                                                     pattern = update_point_class,
                                                     full.names = TRUE)
            
            if (length(all_point_df_file_location) > 0) {
              all_point_df <- lapply(all_point_df_file_location, readRDS)
              all_point_df <- all_point_df %>% abind::abind(along = 1)
              
              if (nrow(all_point_df) > 0) {
                #i <- 999
                if (Replace_Add_Clip_simplify == "R") {
                  Points_data_current_on_map$df <- Points_data_current_on_map$df %>% 
                    filter(point_primary != (Inference_P_simplify %>% str_split("_") %>% unlist())[1] | 
                             point_secondary != (Inference_P_simplify %>% str_split("_") %>% unlist())[2] | 
                             point_tertiary != (Inference_P_simplify %>% str_split("_") %>% unlist())[3] | 
                             point_group != (Inference_P_simplify %>% str_split("_") %>% unlist())[4:5] %>% paste0(collapse = "_"))
                  Points_data_current_on_map$df <- rbind(Points_data_current_on_map$df, all_point_df)
                } else if (Replace_Add_Clip_simplify == "A") {
                  Points_data_current_on_map$df <- rbind(Points_data_current_on_map$df, all_point_df)
                } else if (Replace_Add_Clip_simplify == "C") {
                  Points_data_current_on_map$df <- Points_data_current_on_map$df %>% filter((any(all_point_df[, 2] ==point_id)) == FALSE)
                }
              } else {
                if (Replace_Add_Clip_simplify == "R") {
                  Points_data_current_on_map$df <- Points_data_current_on_map$df %>% 
                    filter(point_primary != (Inference_P_simplify %>% str_split("_") %>% unlist())[1] | 
                             point_secondary != (Inference_P_simplify %>% str_split("_") %>% unlist())[2] | 
                             point_tertiary != (Inference_P_simplify %>% str_split("_") %>% unlist())[3] | 
                             point_group != (Inference_P_simplify %>% str_split("_") %>% unlist())[4:5] %>% paste0(collapse = "_"))
                }
              }
              
              file.remove(all_point_df_file_location)
            }
          }
          
		  Inference_H_simplify <- modified_label_table$Inference_H[i]
          if (Inference_H_simplify != "-") {
            update_heat_class <- paste0(Inference_H_simplify, "_")
            
            all_heat_df_file_location <- list.files(temp_folder_location, 
                                                    pattern = update_heat_class,
                                                    full.names = TRUE)
            
            if(length(all_heat_df_file_location)>0){
			
              all_heat_df <- lapply(all_heat_df_file_location, readRDS)
 			#----#
			count_all_heat_df <- NULL
			
			for(ii in 1:length(all_heat_df)){
			 all_heat_df_simplify <- all_heat_df[[ii]]
			 if(length(all_heat_df_simplify) >= 3){
			   if(length(all_heat_df_simplify[[2]]) >0 && length(all_heat_df_simplify[[3]]) > 0 && (NA %in% c(all_heat_df_simplify[[2]],all_heat_df_simplify[[3]]))==FALSE){
			       count_all_heat_df[ii] <- ii
				   }
			   }
			}
			#----#
			if(length(count_all_heat_df) > 0){
			
            max_width <- max(sapply(all_heat_df[count_all_heat_df], function(x){x[[2]]}))
            max_height <- max(sapply(all_heat_df[count_all_heat_df], function(x){x[[3]]}))
            max_size <- max(max_width, max_height)
			
			
            blank_heat <- array(0, c(max_size, max_size,1))
            for (chunk_no in 1:length(all_heat_df)) {
              blank_heat[all_heat_df[[chunk_no]][[2]][1]:all_heat_df[[chunk_no]][[2]][2], 
                         all_heat_df[[chunk_no]][[3]][1]:all_heat_df[[chunk_no]][[3]][2],1] <- all_heat_df[[chunk_no]][[1]]
            }
            
            color_number <- (str_split(Inference_H_simplify, "_H_") %>% unlist())[2]
            color_number <- input$Change_heat_color_order[as.integer(color_number)]
            
            base_heat_R <- array(color_name_value[[color_number]][1], c(max_size, max_size, 1))
            #saveRDS(base_heat_R, "base_heat_R.rds")
            base_heat_G <- array(color_name_value[[color_number]][2], c(max_size, max_size, 1))
            #saveRDS(base_heat_G, "base_heat_G.rds")
            base_heat_B <- array(color_name_value[[color_number]][3], c(max_size, max_size, 1))
            #saveRDS(base_heat_B, "base_heat_B.rds")
            #saveRDS(blank_heat, "blank_heat.rds")
            
            blank_heat <- abind::abind(base_heat_R, 
                                       base_heat_G, 
                                       base_heat_B, 
                                       blank_heat, along = 3)
            #print(blank_heat)
            
            save_image <- Image(blank_heat, colormode = 'Color') 
            Heats_data_current_on_map$list[[Inference_H_simplify]] <- list(save_image, max_size)
            save_image %>% writeImage(paste0("www/",
                                             input$Slide_or_image, "/", 
                                             input$Annotation_group, "/", 
                                             input$Image_input, "/temp/",
                                             update_heat_class,
                                             "0_0.png"))
			saveRDS(Heats_data_current_on_map$list,paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input, "/heats.rds"))
             file.remove(all_heat_df_file_location)
			  }else{
			     file.remove(all_heat_df_file_location) 
			     Heats_data_current_on_map$list <- list()
			     saveRDS(Heats_data_current_on_map$list,paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input, "/heats.rds"))
			   }
			}else{
			  Heats_data_current_on_map$list <- list()
			  saveRDS(Heats_data_current_on_map$list,paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input, "/heats.rds"))
			}
          }
        }
      }
	  
      
      saveRDS(Heats_data_current_on_map$list, paste0("www/",
                                                     input$Slide_or_image, "/", 
                                                     input$Annotation_group, "/", 
                                                     input$Image_input, "/heats.rds"))
      
      current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
      modified_polygon_list <- Polygons_data_current_on_map$list[[current_polygon_class]]
      polygon_names <- Polygons_data_current_on_map$list[[current_polygon_class]] %>% names()
      if (length(modified_polygon_list) > 0) {
        Polygons_data_current_on_map$bbox <- lapply(modified_polygon_list, function(x){
          lawn_bbox(x)
        })
      } else {
        Polygons_data_current_on_map$bbox <- list()
      }
      
      show_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
      show_polygon_list <- Polygons_data_current_on_map$list[str_detect(names(Polygons_data_current_on_map$list), 
                                                                        paste0(show_polygon_class, "_G_"))]
      
      modified_point_table <- Points_data_current_on_map$df %>% 
        filter(point_primary == input$Point_class_1) %>% 
        filter(point_secondary == input$Point_class_2) %>% 
        filter(point_tertiary == input$Point_class_3)
      
      leafletProxy("image_viewer", session) %>% 
        clearGeoJSON() %>% 
        clearMarkers() 
      
	  
      #print(polygon_names)
      #print(modified_polygon_list)
      
      polygon_show_name <- names(show_polygon_list)
      if (length(polygon_show_name) > 0) {
        polygon_show_color_code <- str_replace_all(polygon_show_name, paste0(paste0(show_polygon_class, "_G_")), "")
        
        for (k in polygon_show_name) {
          polygon_names <- Polygons_data_current_on_map$list[[k]] %>% names()
          color_number <- str_replace_all(k, paste0(paste0(show_polygon_class, "_G_")), "")
          
          isolate({
            geocolor <- input$Change_polygon_color_order[as.integer(color_number)]
            geoopcity <- input$Polygon_opacity_value
          })
          
          for (i in polygon_names) {
            leafletProxy("image_viewer", session) %>% 
              addGeoJSON(Polygons_data_current_on_map$list[[k]][[i]], 
                         layerId = i, 
                         group = paste0("G_", color_number),
                         fillOpacity = input$Polygon_opacity_value, 
                         color = input$Change_polygon_color_order[as.integer(color_number)], 
                         options = leafletOptions(pane = "Polygons"))
          }
        }
      }
      
      
      show_heat_class <- paste0(input$Heat_class_1, "_", input$Heat_class_2, "_", input$Heat_class_3)
      show_heat_list <- Heats_data_current_on_map$list[str_detect(names(Heats_data_current_on_map$list), 
                                                                  paste0(show_heat_class, "_H_"))]
      heat_show_name <- names(show_heat_list)
      
      #print(heat_show_name)
      
      if (length(heat_show_name) > 0) {
        #polygon_show_color_code <- str_replace_all(polygon_show_name, paste0(paste0(show_polygon_class, "_G_")), "")
        
        for (k in heat_show_name) {
          size_heat <- Heats_data_current_on_map$list[[k]][[2]]
          #print(size_heat)
          leafletProxy("image_viewer", session) %>% 
            removeTiles(layerId = k) %>% 
            addTiles(urlTemplate = paste0("Slide/", 
                                          input$Annotation_group, "/", 
                                          input$Image_input, "/temp/",
                                          k,
                                          "_{x}_{y}.png?r=",
                                          sample(100000,1)), 
                     options = leafletOptions(tileSize = size_heat, 
                                              zIndex = 405,
                                              maxNativeZoom = 5, 
                                              minNativeZoom = 5,
                                              opacity = input$Heat_opacity_value
                     ), 
                     group = (k %>% str_split("_") %>% unlist())[4:5] %>% paste0(collapse = "_"),
                     layerId = k)
        }
      }
      
      leafletProxy("image_viewer", session) %>% 
        addCircleMarkers(lng = as.numeric(modified_point_table$point_lon), 
                         lat = as.numeric(modified_point_table$point_lat), 
                         radius = sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting, 
                         fillOpacity = (1 - sum(input$Point_circle_switch)), 
                         color = input$Change_point_color_order[as.integer(modified_point_table$point_group %>% str_replace_all("P_", ""))], 
                         group = modified_point_table$point_group, 
                         layerId = modified_point_table$point_id,
                         options = leafletOptions(pane = "Points"))
      
      updateProgressBar(
        session = session,
        title = "Updating result to image...",
        id = "AI_pre_label_myprogress",
        value = 100
      )
    }
	opened_slide$close()
	end <- Sys.time()
	result <- end - Start
	print(result)
	

    future:::ClusterRegistry(action = "stop")
	plan(list(multisession, sequential))
	
    mem_release()
	rm(list=ls(all=TRUE))
	#C code compiled into R at build time can be called directly in what are termed primitives or via the .Internal interface
    .Internal(gc(verbose = getOption("verbose"), reset = TRUE, full = TRUE))
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "Pre-label finish!!",
      text = "The labeled image have been updated!!",
      type = "success"
    )
  }

  
})

observeEvent(input$AI_pre_label_model_group, {
  updateSelectInput(session, inputId = "AI_pre_label_model", choices = list.files(paste0("model/", input$AI_pre_label_model_group)))
})

observeEvent(input$AI_pre_label_model, {
  if (file.exists(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/detail.rds"))) {
    model_detail <- readRDS(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/detail.rds"))
    
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
    
    AI_pre_label$output_table[["Primary_layer_ID"]] <- Primary_layer_ID
    AI_pre_label$output_table[["Secondary_layer_ID"]] <- Secondary_layer_ID
    AI_pre_label$output_table[["Tertiary_layer_ID"]] <- Tertiary_layer_ID
    AI_pre_label$output_table[["layer_option"]] <- paste0(model_detail$layer_option, option_word)
    AI_pre_label$output_table[["layer_meaning"]] <- model_detail$layer_meaning
    AI_pre_label$output_table[["user_in_label"]] <- "Y"
  } else {
    AI_pre_label$output_table[["Primary_layer_ID"]] <- ""
    AI_pre_label$output_table[["Secondary_layer_ID"]] <- ""
    AI_pre_label$output_table[["Tertiary_layer_ID"]] <- ""
    AI_pre_label$output_table[["layer_option"]] <- ""
    AI_pre_label$output_table[["layer_meaning"]] <- ""
    AI_pre_label$output_table[["user_in_label"]] <- "N"
  }
})

AI_pre_label <- reactiveValues(model = "",
                               model_name = "",
                               model_detail = "",
                               output_table = data.frame("Primary_layer_ID" = "",
                                                         "Secondary_layer_ID" = "",
                                                         "Tertiary_layer_ID" = "",
                                                         "layer_option" = "",
                                                         "layer_meaning" = "",
                                                         "user_in_label" = "Y",
                                                         stringsAsFactors = FALSE))

output$AI_pre_label_table <- renderRHandsontable(
  rhandsontable(AI_pre_label$output_table) %>% 
    hot_col(col = "Primary_layer_ID", type = "dropdown", source = 1:20) %>% 
    hot_col(col = "Secondary_layer_ID", type = "dropdown", source = 1:20) %>% 
    hot_col(col = "Tertiary_layer_ID", type = "dropdown", source = 1:20) %>% 
    hot_col(col = "layer_option", type = "dropdown", source = c("G", paste0("P_", 1:10))) %>% 
    hot_col(col = "user_in_label", type = "dropdown", source = c("Y", "N")) %>% 
    hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = 'white';
              td.style.color = 'black';
           }"))

observeEvent(input$Start_AI_prelabel, {
  
  progressSweetAlert(
    session = session, id = "AI_pre_label_myprogress",
    title = "Start AI labeling",
    display_pct = TRUE, value = 0
  )
  
  model_location <- paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/Model.h5")
  if (file.exists(model_location)) {
    if (AI_pre_label$model_name != paste0(input$AI_pre_label_model_group, "/", input$AI_pre_label_model)) {
      
      updateProgressBar(
        session = session,
        title = "Loading model...",
        id = "AI_pre_label_myprogress",
        value = 10
      )
      
      AI_pre_label$model_name <- paste0(input$AI_pre_label_model_group, "/", input$AI_pre_label_model)
      AI_pre_label$model_detail <- readRDS(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/detail.rds"))
      
      if (AI_pre_label$model_detail$loss_function == "Focal loss") {
        if (AI_pre_label$model_detail$weight_function == "Outer boundary enhancement") {
          loss_name <- "custom_loss"
          source("network_database/U_net/loss_function_base/focal_weight_loss.R", local = TRUE)
        }
      }
      
      if (AI_pre_label$model_detail$monitor == "Tversky coefficient") {
        monitor_name <- "Tversky"
        source("network_database/U_net/monitor_base/Tversky.R", local = TRUE)
        AI_pre_label$model <- load_model_hdf5(model_location, custom_objects = c(custom_loss = custom_loss, Tversky = monitor))
      }
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
      pred_result_all <- predict(AI_pre_label$model, image_ori2)
      modified_label_table <- hot_to_r(input$AI_pre_label_table)
      #print(modified_label_table)
      
      for (i in 1:length(AI_pre_label$model_detail$layer_option)) {
        
        updateProgressBar(
          session = session,
          title = "Post predicting image processing...",
          id = "AI_pre_label_myprogress",
          value = i/length(AI_pre_label$model_detail$layer_option) * 50 + 20
        )
        
        if (modified_label_table$user_in_label == "Y") {
          if (modified_label_table$layer_option[i] == "G") {
            pred_result <- pred_result_all[1,,,i] %>% replace(which(pred_result_all[1,,,i] <= input$AI_pre_label_threshold), 0) %>% replace(which(pred_result_all[1,,,i] > input$AI_pre_label_threshold), 1) %>% fillHull() %>% bwlabel()
            pred_result <- lapply(ocontour(pred_result), function(x){
              XY2LonLat(x[,1], x[,2])
            })
            blank_geo <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
            
            pred_result <- lapply(pred_result, function(x){
              lapply(1:nrow(x), function(y){
                list(x[y,1], x[y,2])
              })
            })
            
            pred_result <- lapply(1:length(pred_result), function(x){
              id_num <- x + 1000
              data_in <- blank_geo
              data_in$geometry$coordinates <- list(pred_result[[x]])
              data_in$properties$layerId <- as.character(id_num)
              data_in$properties$edit_id <- as.character(id_num)
              data_in$properties$`_leaflet_id` <- id_num
              data_in
            })
            
            names(pred_result) <- (c(1:length(pred_result)) + 1000)
            
            #saveRDS(pred_result, "test.rds")
            
            pred_result <- lapply(pred_result, function(x){
              if (lawn_area(x) >= 1) {
                x$geometry$coordinates[[1]][[x$geometry$coordinates[[1]] %>% length() + 1]] <- x$geometry$coordinates[[1]][[1]]
                lawn_simplify(x, tolerance = input$AI_pre_label_polygon_simplify)
              } else {
                NULL
              }
            })
            
            #saveRDS(pred_result, "test.rds")
            
            null_case <- sapply(pred_result, length)
            null_case <- which(null_case == 0)
            
            if (length(null_case) != 0) {
              pred_result <- pred_result[-null_case]
            }

            Polygons_data_current_on_map$list[[paste0(modified_label_table$Primary_layer_ID[i],
                                                      "_",
                                                      modified_label_table$Secondary_layer_ID[i],
                                                      "_",
                                                      modified_label_table$Tertiary_layer_ID[i])]] <- pred_result
            
          }
        }
      }
      
      updateProgressBar(
        session = session,
        title = "Saving result...",
        id = "AI_pre_label_myprogress",
        value = 90
      )
      
      modified_point_table <- Points_data_current_on_map$df %>% 
        filter(point_primary == input$Point_class_1) %>% 
        filter(point_secondary == input$Point_class_2) %>% 
        filter(point_tertiary == input$Point_class_3)
      current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
      modified_polygon_list <- Polygons_data_current_on_map$list[[current_polygon_class]]
      polygon_names <- Polygons_data_current_on_map$list[[current_polygon_class]] %>% names()
      
      if (length(modified_polygon_list) > 0) {
        Polygons_data_current_on_map$bbox <- lapply(modified_polygon_list, function(x){
          lawn_bbox(x)
        })
      } else {
        Polygons_data_current_on_map$bbox <- list()
      }
      
      leafletProxy("image_viewer", session) %>% 
        clearGeoJSON() %>% 
        clearMarkers() %>% 
        addCircleMarkers(lng = as.numeric(modified_point_table$point_lon), lat = as.numeric(modified_point_table$point_lat), 
                         radius = (7 + sum(input$Point_circle_switch) * 13), fillOpacity = (1 - sum(input$Point_circle_switch)), color = color_name_list[as.integer(modified_point_table$point_group %>% str_replace_all("P_", ""))], group = modified_point_table$point_group, 
                         layerId = modified_point_table$point_id)
      
      for (i in polygon_names) {
        leafletProxy("image_viewer", session) %>% addGeoJSON(modified_polygon_list[[i]], layerId = i, group = "GP", fillOpacity = 0.2 * (1-sum(input$Polygon_opacity_on_off)), color = input$Polygon_viewing_color)
      }
	  
      updateProgressBar(
        session = session,
        title = "Updating result to image...",
        id = "AI_pre_label_myprogress",
        value = 100
      )
    }
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "Pre-label finish!!",
      text = "The labeled image have been updated!!",
      type = "success"
    )
  }
})
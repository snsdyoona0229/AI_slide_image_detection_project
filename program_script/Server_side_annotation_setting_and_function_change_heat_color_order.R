observeEvent(input$Delete_heat_data, {
  confirmSweetAlert(
    session = session,
    inputId = "Check_Delete_heat",
    title = "Delete heatmaps data?",
    btn_labels =  c("Cancel", "Delete anyway!!"),
    btn_colors = c("#FE642E", "#04B404")
  )
})

observeEvent(input$Check_Delete_heat, {
  if (input$Check_Delete_heat) {
    all_heat_name <- names(Heats_data_current_on_map$list)
    if (is.null(all_heat_name) == FALSE) {
      if (length(all_heat_name) > 0) {
        for (k in all_heat_name) {
          leafletProxy("image_viewer", session) %>% 
            removeTiles(layerId = k)
        }
      }
    }
    
    Heats_data_current_on_map$list <- list()
    saveRDS(Heats_data_current_on_map$list, paste0("www/",
                                                   input$Slide_or_image, "/", 
                                                   input$Annotation_group, "/", 
                                                   input$Image_input, "/heats.rds"))
    heat_file <- list.files(paste0("www/",
                                   input$Slide_or_image, "/", 
                                   input$Annotation_group, "/", 
                                   input$Image_input, "/temp/"), pattern = ".png", full.names = TRUE)
    file.remove(heat_file)
    
  }
})

observeEvent(input$Redraw_heat_data, {
  confirmSweetAlert(
    session = session,
    inputId = "Check_Redraw_heat",
    title = "Redraw heatmaps data?",
    btn_labels =  c("Cancel", "Redraw anyway!!"),
    btn_colors = c("#FE642E", "#04B404")
  )
})

observeEvent(input$Check_Redraw_heat, {
  if (input$Check_Redraw_heat) {
    all_heat_names <- Heats_data_current_on_map$list %>% names()
    if (length(all_heat_names) > 0) {
      for (i in 1:length(all_heat_names)) {
        color_number <- (str_split(all_heat_names[i], "_H_") %>% unlist())[2]
        color_number <- input$Change_heat_color_order[as.integer(color_number)]
        
        save_image <- Heats_data_current_on_map$list[[all_heat_names[i]]][[1]]
        save_image[,,1] <- color_name_value[[color_number]][1]
        save_image[,,2] <- color_name_value[[color_number]][2]
        save_image[,,3] <- color_name_value[[color_number]][3]
        Heats_data_current_on_map$list[[all_heat_names[i]]][[1]] <- save_image
        
        save_image %>% writeImage(paste0("www/",
                                         input$Slide_or_image, "/", 
                                         input$Annotation_group, "/", 
                                         input$Image_input, "/temp/",
                                         all_heat_names[i],
                                         "_0_0.png"))
      }
      saveRDS(Heats_data_current_on_map$list, paste0("www/",
                                                     input$Slide_or_image, "/", 
                                                     input$Annotation_group, "/", 
                                                     input$Image_input, "/heats.rds"))
      
      show_heat_class <- paste0(input$Heat_class_1, "_", input$Heat_class_2, "_", input$Heat_class_3)
      show_heat_list <- Heats_data_current_on_map$list[str_detect(names(Heats_data_current_on_map$list), 
                                                                  paste0(show_heat_class, "_H_"))]
      heat_show_name <- names(show_heat_list)
      
      if (length(heat_show_name) > 0) {
        #polygon_show_color_code <- str_replace_all(polygon_show_name, paste0(paste0(show_polygon_class, "_G_")), "")
        all_heat_name <- names(Heats_data_current_on_map$list)
        if (is.null(all_heat_name) == FALSE) {
          if (length(all_heat_name) > 0) {
            for (k in all_heat_name) {
              leafletProxy("image_viewer", session) %>% 
                removeTiles(layerId = k)
            }
          }
        }
        
        for (k in heat_show_name) {
          size_heat <- Heats_data_current_on_map$list[[k]][[2]]
          #print(size_heat)
          leafletProxy("image_viewer", session) %>% 
            #removeTiles(layerId = k) %>% 
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
    }
  }
})
observeEvent(input$Heat_opacity_value, {
  show_heat_class <- paste0(input$Heat_class_1, "_", input$Heat_class_2, "_", input$Heat_class_3)
  show_heat_list <- Heats_data_current_on_map$list[str_detect(names(Heats_data_current_on_map$list), 
                                                              paste0(show_heat_class, "_H_"))]
  heat_show_name <- names(show_heat_list)
  
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
})

observeEvent(input$Heat_class_1, {
  
  all_heat_name <- names(Heats_data_current_on_map$list)
  if (is.null(all_heat_name) == FALSE) {
    if (length(all_heat_name) > 0) {
      for (k in all_heat_name) {
        leafletProxy("image_viewer", session) %>% 
          removeTiles(layerId = k)
      }
    }
  }
  
  show_heat_class <- paste0(input$Heat_class_1, "_", input$Heat_class_2, "_", input$Heat_class_3)
  show_heat_list <- Heats_data_current_on_map$list[str_detect(names(Heats_data_current_on_map$list), 
                                                              paste0(show_heat_class, "_H_"))]
  heat_show_name <- names(show_heat_list)
  
  if (length(heat_show_name) > 0) {
    #polygon_show_color_code <- str_replace_all(polygon_show_name, paste0(paste0(show_polygon_class, "_G_")), "")
    
    for (k in heat_show_name) {
      size_heat <- Heats_data_current_on_map$list[[k]][[2]]
      #print(size_heat)
      leafletProxy("image_viewer", session) %>% 
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
})

observeEvent(input$Heat_class_2, {
  
  all_heat_name <- names(Heats_data_current_on_map$list)
  if (is.null(all_heat_name) == FALSE) {
    if (length(all_heat_name) > 0) {
      for (k in all_heat_name) {
        leafletProxy("image_viewer", session) %>% 
          removeTiles(layerId = k)
      }
    }
  }
  
  show_heat_class <- paste0(input$Heat_class_1, "_", input$Heat_class_2, "_", input$Heat_class_3)
  show_heat_list <- Heats_data_current_on_map$list[str_detect(names(Heats_data_current_on_map$list), 
                                                              paste0(show_heat_class, "_H_"))]
  heat_show_name <- names(show_heat_list)
  
  if (length(heat_show_name) > 0) {
    #polygon_show_color_code <- str_replace_all(polygon_show_name, paste0(paste0(show_polygon_class, "_G_")), "")
    
    for (k in heat_show_name) {
      size_heat <- Heats_data_current_on_map$list[[k]][[2]]
      #print(size_heat)
      leafletProxy("image_viewer", session) %>% 
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
})

observeEvent(input$Heat_class_3, {
  
  all_heat_name <- names(Heats_data_current_on_map$list)
  if (is.null(all_heat_name) == FALSE) {
    if (length(all_heat_name) > 0) {
      for (k in all_heat_name) {
        leafletProxy("image_viewer", session) %>% 
          removeTiles(layerId = k)
      }
    }
  }
  
  show_heat_class <- paste0(input$Heat_class_1, "_", input$Heat_class_2, "_", input$Heat_class_3)
  show_heat_list <- Heats_data_current_on_map$list[str_detect(names(Heats_data_current_on_map$list), 
                                                              paste0(show_heat_class, "_H_"))]
  heat_show_name <- names(show_heat_list)
  
  if (length(heat_show_name) > 0) {
    #polygon_show_color_code <- str_replace_all(polygon_show_name, paste0(paste0(show_polygon_class, "_G_")), "")
    
    for (k in heat_show_name) {
      size_heat <- Heats_data_current_on_map$list[[k]][[2]]
      #print(size_heat)
      leafletProxy("image_viewer", session) %>% 
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
})
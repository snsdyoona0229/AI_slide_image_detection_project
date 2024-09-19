observeEvent(input$Point_class_1, {
  modified_point_table <- Points_data_current_on_map$df %>% 
    filter(point_primary == input$Point_class_1) %>% 
    filter(point_secondary == input$Point_class_2) %>% 
    filter(point_tertiary == input$Point_class_3)
  leafletProxy("image_viewer", session) %>% 
    clearMarkers() %>% 
    addCircleMarkers(lng = as.numeric(modified_point_table$point_lon), 
                     lat = as.numeric(modified_point_table$point_lat), 
                     radius = sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting, 
                     fillOpacity = (1 - sum(input$Point_circle_switch)), 
                     color = input$Change_point_color_order[as.integer(modified_point_table$point_group %>% str_replace_all("P_", ""))], 
                     group = modified_point_table$point_group, 
                     layerId = modified_point_table$point_id,
                     options = leafletOptions(pane = "Points")) 
})

observeEvent(input$Point_class_2, {
  modified_point_table <- Points_data_current_on_map$df %>% 
    filter(point_primary == input$Point_class_1) %>% 
    filter(point_secondary == input$Point_class_2) %>% 
    filter(point_tertiary == input$Point_class_3)
  leafletProxy("image_viewer", session) %>% 
    clearMarkers() %>% 
    addCircleMarkers(lng = as.numeric(modified_point_table$point_lon), 
                     lat = as.numeric(modified_point_table$point_lat), 
                     radius = sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting, 
                     fillOpacity = (1 - sum(input$Point_circle_switch)), 
                     color = input$Change_point_color_order[as.integer(modified_point_table$point_group %>% str_replace_all("P_", ""))], 
                     group = modified_point_table$point_group, 
                     layerId = modified_point_table$point_id,
                     options = leafletOptions(pane = "Points")) 
})

observeEvent(input$Point_class_3, {
  modified_point_table <- Points_data_current_on_map$df %>% 
    filter(point_primary == input$Point_class_1) %>% 
    filter(point_secondary == input$Point_class_2) %>% 
    filter(point_tertiary == input$Point_class_3)
  leafletProxy("image_viewer", session) %>% 
    clearMarkers() %>% 
    addCircleMarkers(lng = as.numeric(modified_point_table$point_lon), 
                     lat = as.numeric(modified_point_table$point_lat), 
                     radius = sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting, 
                     fillOpacity = (1 - sum(input$Point_circle_switch)), 
                     color = input$Change_point_color_order[as.integer(modified_point_table$point_group %>% str_replace_all("P_", ""))], 
                     group = modified_point_table$point_group, 
                     layerId = modified_point_table$point_id,
                     options = leafletOptions(pane = "Points")) 
})

observeEvent(input$Polygon_class_1, {
  #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
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
  #print(Polygons_data_current_on_map$bbox)
  
  #modified_polygon_list <- modified_polygon_list %>% unname()
  
  leafletProxy("image_viewer", session) %>% 
    clearGeoJSON() 
  
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
})

observeEvent(input$Polygon_class_2, {
  #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
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
  #print(Polygons_data_current_on_map$bbox)
  
  #modified_polygon_list <- modified_polygon_list %>% unname()
  
  leafletProxy("image_viewer", session) %>% 
    clearGeoJSON() 
  
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
})

observeEvent(input$Polygon_class_3, {
  #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
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
  #print(Polygons_data_current_on_map$bbox)
  
  #modified_polygon_list <- modified_polygon_list %>% unname()
  
  leafletProxy("image_viewer", session) %>% 
    clearGeoJSON() 
  
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
})


observeEvent(input$Draw_polygon_layers, {
  #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
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
  
  #print(Polygons_data_current_on_map$bbox)
  
  #modified_polygon_list <- modified_polygon_list %>% unname()
  
  #leafletProxy("image_viewer", session) %>% 
  #  clearGeoJSON() 
  
  #for (i in polygon_names) {
  #  leafletProxy("image_viewer", session) %>% 
  #    addGeoJSON(modified_polygon_list[[i]], 
  #               layerId = i, 
  #               group = "GP", 
  #               fillOpacity = input$Polygon_opacity_value, 
  #               color = input$Polygon_viewing_color,
  #               options = leafletOptions(pane = "Polygons"))
  #}
})


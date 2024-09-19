observeEvent(input$Un_do, {
  #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
  current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
  if (Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] != 1) {
    Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] - 1
    modified_polygon_list <- Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]]
    polygon_names <- modified_polygon_list %>% names()
    
    
    Polygons_data_current_on_map$list[[current_polygon_class]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]]
    
    if (length(modified_polygon_list) > 0) {
      Polygons_data_current_on_map$bbox <- lapply(modified_polygon_list, function(x){
        lawn_bbox(x)
      })
    } else {
      Polygons_data_current_on_map$bbox <- list()
    }
    
    leafletProxy("image_viewer", session) %>% 
      clearGeoJSON()
    
    for (i in polygon_names) {
      leafletProxy("image_viewer", session) %>% 
        addGeoJSON(modified_polygon_list[[i]], 
                   layerId = i, 
                   group = "GP", 
                   fillOpacity = input$Polygon_opacity_value, 
                   color = input$Polygon_viewing_color,
                   options = leafletOptions(pane = "Polygons"))
    }
  }
})

observeEvent(input$Re_do, {
  #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
  current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
  if (Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] != length(Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]])) {
    Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] + 1
    modified_polygon_list <- Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]]
    polygon_names <- modified_polygon_list %>% names()
    
    
    Polygons_data_current_on_map$list[[current_polygon_class]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]]
    
    if (length(modified_polygon_list) > 0) {
      Polygons_data_current_on_map$bbox <- lapply(modified_polygon_list, function(x){
        lawn_bbox(x)
      })
    } else {
      Polygons_data_current_on_map$bbox <- list()
    }
    
    leafletProxy("image_viewer", session) %>% 
      clearGeoJSON()
    
    for (i in polygon_names) {
      leafletProxy("image_viewer", session) %>% 
        addGeoJSON(modified_polygon_list[[i]], 
                   layerId = i, 
                   group = "GP", 
                   fillOpacity = input$Polygon_opacity_value, 
                   color = input$Polygon_viewing_color,
                   options = leafletOptions(pane = "Polygons"))
    }
  }
})
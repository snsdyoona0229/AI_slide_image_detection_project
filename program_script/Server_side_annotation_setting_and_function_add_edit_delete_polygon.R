#observeEvent(input$image_viewer_draw_new_feature, {
#  new_polygon <- input$image_viewer_draw_new_feature
#  new_polygon_id <- input$image_viewer_draw_new_feature$properties$layerId
  #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
#  current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
#  if (is.null(Polygons_data_current_on_map$list[[current_polygon_class]])) {
#    Polygons_data_current_on_map$list[[current_polygon_class]] <- list()
#  }
#  Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id]] <- new_polygon
  
#  if (is.null(Polygons_history_current_on_map$list[[current_polygon_class]])) {
#    Polygons_history_current_on_map$list[[current_polygon_class]] <- list()
#    Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] <- 1
#    Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]] <- list()
#    Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[1]] <- list()
#  } else {
#    if (Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] < 10) {
#      Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] + 1
#    }
#  }
#  if (Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] == 10) {
#    if (length(Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]]) == 10) {
#      Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][-1]
#      Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]] <- Polygons_data_current_on_map$list[[current_polygon_class]]
#    } else {
#      Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]] <- Polygons_data_current_on_map$list[[current_polygon_class]]
#    }
#  } else {
#    Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]] <- Polygons_data_current_on_map$list[[current_polygon_class]]
#  }
#  if (Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] != length(Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]])) {
#    Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][1:Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]
#  }
  #print(str(new_polygon))
  #print(Polygons_history_current_on_map$list)
  #print(Polygons_data_current_on_map$list)
  #saveRDS(Polygons_history_current_on_map$list, "test.rds")
  
  #saveRDS(new_polygon, "test.rds")
#  leafletProxy("image_viewer", session) %>% 
#    addGeoJSON(geojson = new_polygon, 
#               group = "GP", 
#               fillOpacity = input$Polygon_opacity_value, 
#               color = input$Polygon_viewing_color,
#               options = leafletOptions(pane = "Polygons"))
  
  
  #print(Polygons_data_current_on_map$list)
#})

#observeEvent(input$image_viewer_geojson_move, {
#  print(input$image_viewer_geojson_move)
#})


#observeEvent(input$image_viewer_draw_edited_features, {
#  edit_polygon <- input$image_viewer_draw_edited_features
#  edit_polygon_id <- input$image_viewer_draw_edited_features$properties$layerId
  
#  print(edit_polygon)
  
  #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
#  current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
#  if (is.null(Polygons_data_current_on_map$list[[current_polygon_class]])) {
#    Polygons_data_current_on_map$list[[current_polygon_class]] <- list()
#  }
  
  
#  Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]] <- edit_polygon
#  edit_polygon_bbox <- lawn_bbox(Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]])
#  Polygons_data_current_on_map$bbox[[edit_polygon_id]] <- edit_polygon_bbox
  
  
#  current_all_id <- Polygons_data_current_on_map$list[[current_polygon_class]] %>% names() %>% as.integer()
  
#  current_polygon_data <- abind::abind(Polygons_data_current_on_map$bbox, along = 0)
  #print(current_polygon_data)
  
#  current_polygon_matrix <- matrix(data = c((edit_polygon_bbox[1] > current_polygon_data[,1]),
#                                            (edit_polygon_bbox[1] > current_polygon_data[,3]),
#                                            (edit_polygon_bbox[3] > current_polygon_data[,1]),
#                                            (edit_polygon_bbox[3] > current_polygon_data[,3]),
#                                            (edit_polygon_bbox[2] > current_polygon_data[,2]),
#                                            (edit_polygon_bbox[2] > current_polygon_data[,4]),
#                                            (edit_polygon_bbox[4] > current_polygon_data[,2]),
#                                            (edit_polygon_bbox[4] > current_polygon_data[,4])
#  ), ncol = 8, byrow = FALSE)
  #print(current_polygon_matrix)
#  check_matrix_1 <- current_polygon_matrix[,1] + current_polygon_matrix[,2] + current_polygon_matrix[,3] + current_polygon_matrix[,4]
#  check_matrix_2 <- current_polygon_matrix[,5] + current_polygon_matrix[,6] + current_polygon_matrix[,7] + current_polygon_matrix[,8]
  
#  check_matrix_1 <- (check_matrix_1 > 0) & (check_matrix_1 < 4)
#  check_matrix_2 <- (check_matrix_2 > 0) & (check_matrix_2 < 4)
  
#  use_matrix <- which(check_matrix_1 & check_matrix_2)
#  overlap_name <- current_all_id[use_matrix] %>% as.character()
#  overlap_name <- overlap_name[which(overlap_name != edit_polygon_id)]
  
#  if (length(overlap_name) == 0) {
#    leafletProxy("image_viewer", session) %>% 
#      removeGeoJSON(layerId = edit_polygon_id) %>% 
#      addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]], 
#                 group = "GP", 
#                 fillOpacity = input$Polygon_opacity_value, 
#                 color = input$Polygon_viewing_color, 
#                 layerId = edit_polygon_id,
#                 options = leafletOptions(pane = "Polygons"))
#  } else {
#    for (i in overlap_name) {
#      if (lawn_boolean_within(Polygons_data_current_on_map$list[[current_polygon_class]][[i]], Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]])) {
#        Polygons_data_current_on_map$list[[current_polygon_class]][[i]] <- NULL
#        Polygons_data_current_on_map$bbox[[i]] <- NULL
#        leafletProxy("image_viewer", session) %>% 
#          removeGeoJSON(layerId = i) %>% 
#          addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]], 
#                     group = "GP", 
#                     fillOpacity = input$Polygon_opacity_value, 
#                     color = input$Polygon_viewing_color, 
#                     layerId = edit_polygon_id,
#                     options = leafletOptions(pane = "Polygons"))
#      } else if (lawn_boolean_overlap(Polygons_data_current_on_map$list[[current_polygon_class]][[i]], Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]])) {
#        Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]] <- lawn_union(Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]], 
#                                                                                                    Polygons_data_current_on_map$list[[current_polygon_class]][[i]])
#        Polygons_data_current_on_map$list[[current_polygon_class]][[i]] <- NULL
#        Polygons_data_current_on_map$bbox[[i]] <- NULL
#        Polygons_data_current_on_map$bbox[[edit_polygon_id]] <- lawn_bbox(Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]])
#        leafletProxy("image_viewer", session) %>% 
#          removeGeoJSON(layerId = i) %>% 
#          removeGeoJSON(layerId = edit_polygon_id) %>% 
#          addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]], 
#                     group = "GP", 
#                     fillOpacity = input$Polygon_opacity_value, 
#                     color = input$Polygon_viewing_color, 
#                     layerId = edit_polygon_id,
#                     options = leafletOptions(pane = "Polygons"))
#      } else if (lawn_boolean_within(Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]], Polygons_data_current_on_map$list[[current_polygon_class]][[i]])) {
#        Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]] <- NULL
#        Polygons_data_current_on_map$bbox[[edit_polygon_id]] <- NULL
#        leafletProxy("image_viewer", session) %>% 
#          removeGeoJSON(layerId = edit_polygon_id)
#      } else {
#        leafletProxy("image_viewer", session) %>% 
#          removeGeoJSON(layerId = edit_polygon_id) %>% 
#          addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[edit_polygon_id]], 
#                     group = "GP", 
#                     fillOpacity = input$Polygon_opacity_value, 
#                     color = input$Polygon_viewing_color, 
#                     layerId = edit_polygon_id,
#                     options = leafletOptions(pane = "Polygons"))
#      }
#    }
#  }
  
  
#  if (is.null(Polygons_history_current_on_map$list[[current_polygon_class]])) {
#    Polygons_history_current_on_map$list[[current_polygon_class]] <- list()
#    Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] <- 1
#    Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]] <- list()
#    Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[1]] <- list()
#  } else {
#    if (Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] < 10) {
#      Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] + 1
#    }
#  }
#  if (Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] == 10) {
#    if (length(Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]]) == 10) {
#      Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][-1]
#      Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]] <- Polygons_data_current_on_map$list[[current_polygon_class]]
#    } else {
#      Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]] <- Polygons_data_current_on_map$list[[current_polygon_class]]
#    }
#  } else {
#    Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]] <- Polygons_data_current_on_map$list[[current_polygon_class]]
#  }
#  if (Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] != length(Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]])) {
#    Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][1:Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]
#  }
  
  #print(input$image_viewer_draw_edited_features)
  #print("edit")
#})

#observeEvent(input$image_viewer_draw_deleted_features, {
#  print(input$image_viewer_draw_deleted_features)
#  print("delet")
#})

observeEvent(input$image_viewer_geojson_right_click, { # update the location selectInput on map clicks
  if (input$Polygon_free_draw_on_off | input$fix_area_draw_on_off) {
    p <- input$image_viewer_geojson_right_click
    
    #print(p)
    
    #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
    current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
    polygon_name <- p$id %>% as.character()
    
    
    Polygons_data_current_on_map$list[[current_polygon_class]][[polygon_name]] <- NULL
    Polygons_data_current_on_map$bbox[[polygon_name]] <- NULL
    
    leafletProxy("image_viewer", session) %>% 
      removeGeoJSON(p$id)
    
    if (is.null(Polygons_history_current_on_map$list[[current_polygon_class]])) {
      Polygons_history_current_on_map$list[[current_polygon_class]] <- list()
      Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] <- 1
      Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]] <- list()
      Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[1]] <- list()
    } else {
      if (Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] < 10) {
        Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] + 1
      }
    }
    if (Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] == 10) {
      if (length(Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]]) == 10) {
        Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][-1]
        Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]] <- Polygons_data_current_on_map$list[[current_polygon_class]]
      } else {
        Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]] <- Polygons_data_current_on_map$list[[current_polygon_class]]
      }
    } else {
      Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][[Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]] <- Polygons_data_current_on_map$list[[current_polygon_class]]
    }
    if (Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]] != length(Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]])) {
      Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]] <- Polygons_history_current_on_map$list[[current_polygon_class]][["polygon_data"]][1:Polygons_history_current_on_map$list[[current_polygon_class]][["current_number"]]]
    }
  }
})
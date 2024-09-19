observeEvent(input$image_viewer_click, {

  if (input$Start_to_draw_point == "Add points") {
	    
	point_row <- nrow(Points_data_current_on_map$df)
		
	x.data.frame <- data.frame(point_group = paste0("P_", input$Draw_point_layers),
	                         point_id = paste0("P_",input$Draw_point_layers, "_", input$image_viewer_click$lng, "_", input$image_viewer_click$lat),
							 point_lon = input$image_viewer_click$lng,
							 point_lat = input$image_viewer_click$lat,
							 point_primary = input$Point_class_1,
							 point_secondary = input$Point_class_2,
							 point_tertiary = input$Point_class_3
	                        )
	
	Points_data_current_on_map$df <- rbind(Points_data_current_on_map$df,x.data.frame)
	 

#    Points_data_current_on_map$df[point_row +1, 1] <- paste0("P_", input$Draw_point_layers)
#    Points_data_current_on_map$df[point_row +1, 2] <- paste0("P_",input$Draw_point_layers, "_", input$image_viewer_click$lng, "_", input$image_viewer_click$lat)
#    Points_data_current_on_map$df[point_row +1, 3] <- input$image_viewer_click$lng
#    Points_data_current_on_map$df[point_row +1, 4] <- input$image_viewer_click$lat
#    Points_data_current_on_map$df[point_row +1, 5] <- input$Point_class_1
#    Points_data_current_on_map$df[point_row +1, 6] <- input$Point_class_2
#    Points_data_current_on_map$df[point_row +1, 7] <- input$Point_class_3
    
    leafletProxy("image_viewer", session) %>% 
      addCircleMarkers(lng = input$image_viewer_click$lng, 
                       lat = input$image_viewer_click$lat, 
                       radius = sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting, 
                       fillOpacity = (1 - sum(input$Point_circle_switch)), 
                       color = input$Change_point_color_order[as.integer(input$Draw_point_layers)], 
                       group = paste0("P_", input$Draw_point_layers), 
                       layerId = paste0("P_",input$Draw_point_layers, "_", input$image_viewer_click$lng, "_", input$image_viewer_click$lat),
                       options = leafletOptions(pane = "Points"))
  } else if (Measuring_condition$line_M == 1) {
    leafletProxy("image_viewer", session) %>% 
      addCircleMarkers(lng = input$image_viewer_click$lng, 
                       lat = input$image_viewer_click$lat, 
                       radius = 3, 
                       color = "blue", 
                       group = "measure_temp_dot",
                       options = leafletOptions(pane = "Points"))
    Measuring_condition$line_M <- Measuring_condition$line_M + 1
    Measuring_condition$point_1 <- list(LonLat2XY(lon_deg = input$image_viewer_click$lng, 
                                                  lat_deg = input$image_viewer_click$lat, 
                                                  zoom = 13) %>% unlist,
                                        input$image_viewer_click %>% unlist)
  } else if (Measuring_condition$line_M == 2) {
    Measuring_condition$point_2 <- list(LonLat2XY(lon_deg = input$image_viewer_click$lng, 
                                                  lat_deg = input$image_viewer_click$lat, 
                                                  zoom = 13) %>% unlist,
                                        input$image_viewer_click %>% unlist)
    text_file_name <- paste0(input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input)
    slide_name <- list.files(paste0("www/", text_file_name), pattern = ".ndpi", full.names = TRUE)
    point_data_nm <- NDPI_point_to_nm(file_name = slide_name,
                                      x_point = c(Measuring_condition$point_1[[1]][1], Measuring_condition$point_2[[1]][1]),
                                      y_point = c(Measuring_condition$point_1[[1]][2], Measuring_condition$point_2[[1]][2]))
    point_data_nm <- ((point_data_nm$x_nm[2] - point_data_nm$x_nm[1])^2 + (point_data_nm$y_nm[2] - point_data_nm$y_nm[1])^2)^0.5
    
    if (point_data_nm < 1000) {
      label_string <- paste0("Length: ", round(point_data_nm, digits = 2), " nm")
    } else if (point_data_nm < 1000000) {
      label_string <- paste0("Length: ", round(point_data_nm/1000, digits = 2), " um")
    } else if (point_data_nm < 1000000000) {
      label_string <- paste0("Length: ", round(point_data_nm/1000000, digits = 2), " mm")
    } else if (point_data_nm < 10000000000) {
      label_string <- paste0("Length: ", round(point_data_nm/10000000, digits = 2), " cm")
    }
    
    leafletProxy("image_viewer", session) %>% 
      clearGroup("measure_temp_dot") %>% 
      addPolylines(lng = c(Measuring_condition$point_1[[2]][2], Measuring_condition$point_2[[2]][2]), 
                   lat = c(Measuring_condition$point_1[[2]][1], Measuring_condition$point_2[[2]][1]), group = "measure_line", label = label_string, labelOptions = labelOptions(noHide = TRUE))
    Measuring_condition$line_M <- 0
    Measuring_condition$point_1 <- c(0,0)
    Measuring_condition$point_2 <- c(0,0)
  } else if (input$fix_area_draw_on_off) {
    text_file_name <- paste0(input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input)
    slide_name <- list.files(paste0("www/", text_file_name), pattern = ".ndpi", full.names = TRUE)
    slide_pixel_nm <- NDPI_point_to_nm(file_name = slide_name,
                                       x_point = c(0,0),
                                       y_point = c(0,1))
    slide_pixel_nm <- abs(slide_pixel_nm$y_nm[2] - slide_pixel_nm$y_nm[1])
    chunk_area <- input$fix_area_size_ready_to_draw %>% as.numeric()
    if (input$fix_area_polygon_or_circle == "Polygon") {
      chunk_edge_nm <- (chunk_area^0.5) * 1000000
      chunk_pixel_size <- chunk_edge_nm/slide_pixel_nm
      center_xy <- LonLat2XY(lon_deg = input$image_viewer_click$lng,
                             lat_deg = input$image_viewer_click$lat,
                             zoom = 12 + 1)
      
      addpoly_chunk <- center_check_for_point_or_chunk(center_x_all = center_xy$x[1],
                                                       center_y_all = center_xy$y[1],
                                                       zoom_level = 12,
                                                       tile_size = chunk_pixel_size,
                                                       point_vs_chunk = "chunk")
      raw_polygon <- addpoly_chunk[[1]]
    } else if (input$fix_area_polygon_or_circle == "Circle") {
      circle_radius_nm <- ((chunk_area/pi)^0.5) * 1000000
      circle_radius_pixel_size <- circle_radius_nm/slide_pixel_nm
      center_xy <- LonLat2XY(lon_deg = input$image_viewer_click$lng,
                             lat_deg = input$image_viewer_click$lat,
                             zoom = 12 + 1)
      blank_poly <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
      blank_poly$geometry$coordinates <- array(0, c(1,361,2))
      lonlat_circle <- XY2LonLat(x = center_xy$x[1] + circle_radius_pixel_size * sapply(0:360, function(x){cos(2*pi/360*x)}),
                                 y = center_xy$y[1] + circle_radius_pixel_size * sapply(0:360, function(x){sin(2*pi/360*x)}),
                                 zoom = 12+1)
      blank_poly$geometry$coordinates[1,,1] <- lonlat_circle[,1]
      blank_poly$geometry$coordinates[1,,2] <- lonlat_circle[,2]
      raw_polygon <- blank_poly
    }
    
    polygon_length <- raw_polygon$geometry$coordinates[[1]] %>% length()
    
    if (polygon_length > 0) {
      Free_draw_polygon_current_on_map$leaflet_geojson <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
      #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
      current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
      if (is.null(Polygons_data_current_on_map$list[[current_polygon_class]])) {
        Polygons_data_current_on_map$list[[current_polygon_class]] <- list()
      }
      current_all_id <- Polygons_data_current_on_map$list[[current_polygon_class]] %>% names() %>% as.integer()
      
      raw_polygon_coor <- lawn_coordall(raw_polygon)
      
      clip_polygon <- polysimplify(list(list(x = raw_polygon_coor[,1], y = raw_polygon_coor[,2])))
      
      if (length(current_all_id) == 0) {
        new_polygon_id <- sample(1:100000, length(clip_polygon)) %>% as.character()
      } else {
        sample_number <- c(1:100000)
        sample_number <- sample_number[-current_all_id]
        new_polygon_id <- sample(sample_number, length(clip_polygon)) %>% as.character()
      }
      
      for (poly_number in 1:length(clip_polygon)) {
        new_polygon <- Free_draw_polygon_current_on_map$leaflet_geojson
        current_clip <- clip_polygon[[poly_number]] %>% abind::abind(along = 2) %>% unname()
        choose_poly <- lapply(1:nrow(current_clip), function(x){
          list(current_clip[x,1], current_clip[x,2])
        })
        new_polygon$geometry$coordinates <- list(choose_poly)
        new_polygon$geometry$coordinates[[1]][[new_polygon$geometry$coordinates[[1]] %>% length() + 1]] <- new_polygon$geometry$coordinates[[1]][[1]]
        new_polygon$properties$layerId <- new_polygon_id[poly_number]
        new_polygon$properties$edit_id <- new_polygon_id[poly_number]
        new_polygon$properties$`_leaflet_id` <- new_polygon_id[poly_number] %>% as.integer()
        new_polygon <- new_polygon %>% lawn_simplify(tolerance = input$Polygon_free_draw_simplify)
        new_polygon_bbox <- lawn_bbox(new_polygon)
        
        current_polygon_data <- abind::abind(Polygons_data_current_on_map$bbox, along = 0)
        #print(current_polygon_data)
        
        current_polygon_matrix <- matrix(data = c((new_polygon_bbox[1] > current_polygon_data[,1]),
                                                  (new_polygon_bbox[1] > current_polygon_data[,3]),
                                                  (new_polygon_bbox[3] > current_polygon_data[,1]),
                                                  (new_polygon_bbox[3] > current_polygon_data[,3]),
                                                  (new_polygon_bbox[2] > current_polygon_data[,2]),
                                                  (new_polygon_bbox[2] > current_polygon_data[,4]),
                                                  (new_polygon_bbox[4] > current_polygon_data[,2]),
                                                  (new_polygon_bbox[4] > current_polygon_data[,4])
        ), ncol = 8, byrow = FALSE)
        #print(current_polygon_matrix)
        check_matrix_1 <- current_polygon_matrix[,1] + current_polygon_matrix[,2] + current_polygon_matrix[,3] + current_polygon_matrix[,4]
        check_matrix_2 <- current_polygon_matrix[,5] + current_polygon_matrix[,6] + current_polygon_matrix[,7] + current_polygon_matrix[,8]
        
        check_matrix_1 <- (check_matrix_1 > 0) & (check_matrix_1 < 4)
        check_matrix_2 <- (check_matrix_2 > 0) & (check_matrix_2 < 4)
        
        use_matrix <- which(check_matrix_1 & check_matrix_2)
        
        current_all_id <- Polygons_data_current_on_map$list[[current_polygon_class]] %>% names() %>% as.integer()
        overlap_name <- current_all_id[use_matrix] %>% as.character()
        #print(use_matrix)
        
        Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]] <- new_polygon
        
        Polygons_data_current_on_map$bbox[[new_polygon_id[poly_number]]] <- new_polygon_bbox
        if (length(overlap_name) == 0) {
          leafletProxy("image_viewer", session) %>% 
            addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]], 
                       group = paste0("G_", input$Draw_polygon_layers),
                       fillOpacity = input$Polygon_opacity_value, 
                       color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                       layerId = new_polygon_id[poly_number],
                       options = leafletOptions(pane = "Polygons"))
        } else {
          for (i in overlap_name) {
            if (lawn_boolean_within(Polygons_data_current_on_map$list[[current_polygon_class]][[i]], Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]])) {
              Polygons_data_current_on_map$list[[current_polygon_class]][[i]] <- NULL
              Polygons_data_current_on_map$bbox[[i]] <- NULL
              leafletProxy("image_viewer", session) %>% 
                removeGeoJSON(layerId = i) %>% 
                addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]], 
                           group = paste0("G_", input$Draw_polygon_layers),
                           fillOpacity = input$Polygon_opacity_value, 
                           color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                           layerId = new_polygon_id[poly_number],
                           options = leafletOptions(pane = "Polygons"))
            } else if (lawn_boolean_overlap(Polygons_data_current_on_map$list[[current_polygon_class]][[i]], Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]])) {
              intersect_poly_area <- lawn_intersect(Polygons_data_current_on_map$list[[current_polygon_class]][[i]], Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]) %>% lawn_area()
              if (intersect_poly_area > 0) {
                Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]] <- lawn_union(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]], 
                                                                                                                        Polygons_data_current_on_map$list[[current_polygon_class]][[i]])
                Polygons_data_current_on_map$list[[current_polygon_class]][[i]] <- NULL
                Polygons_data_current_on_map$bbox[[i]] <- NULL
                Polygons_data_current_on_map$bbox[[new_polygon_id[poly_number]]] <- lawn_bbox(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]])
                leafletProxy("image_viewer", session) %>% 
                  removeGeoJSON(layerId = i) %>% 
                  removeGeoJSON(layerId = new_polygon_id[poly_number]) %>% 
                  addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]], 
                             group = paste0("G_", input$Draw_polygon_layers),
                             fillOpacity = input$Polygon_opacity_value, 
                             color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                             layerId = new_polygon_id[poly_number],
                             options = leafletOptions(pane = "Polygons"))
              } else {
                leafletProxy("image_viewer", session) %>% 
                  addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]], 
                             group = paste0("G_", input$Draw_polygon_layers),
                             fillOpacity = input$Polygon_opacity_value, 
                             color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                             layerId = new_polygon_id[poly_number],
                             options = leafletOptions(pane = "Polygons"))
              }
            } else if (lawn_boolean_within(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]], Polygons_data_current_on_map$list[[current_polygon_class]][[i]])) {
              Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]] <- NULL
              Polygons_data_current_on_map$bbox[[new_polygon_id[poly_number]]] <- NULL
            } else {
              leafletProxy("image_viewer", session) %>% 
                addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]], 
                           group = paste0("G_", input$Draw_polygon_layers),
                           fillOpacity = input$Polygon_opacity_value, 
                           color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                           layerId = new_polygon_id[poly_number],
                           options = leafletOptions(pane = "Polygons"))
            }
          }
        }
      }
      
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
  }
})

observeEvent(input$image_viewer_marker_click, {
  if (input$Start_to_draw_point == "Remove points") {
    remove_row <- which(Points_data_current_on_map$df$point_id == input$image_viewer_marker_click$id)
    
    #print(Points_data_current_on_map$df$point_id)
    #print(input$image_viewer_marker_click$id)
    
    Points_data_current_on_map$df <- Points_data_current_on_map$df[-remove_row,]
    leafletProxy("image_viewer", session) %>% 
      removeMarker(input$image_viewer_marker_click$id)
  }
})

observeEvent(input$image_viewer_marker_right_click, {
  if (input$Polygon_free_draw_on_off) {
    remove_row <- which(Points_data_current_on_map$df$point_id == input$image_viewer_marker_right_click$id)
    
    #print(Points_data_current_on_map$df$point_id)
    #print(input$image_viewer_marker_click$id)
    
    Points_data_current_on_map$df <- Points_data_current_on_map$df[-remove_row,]
    leafletProxy("image_viewer", session) %>% 
      removeMarker(input$image_viewer_marker_right_click$id)
  }
})
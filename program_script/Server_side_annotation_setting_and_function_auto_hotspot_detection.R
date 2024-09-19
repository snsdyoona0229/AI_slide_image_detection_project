output$fix_area_detect_point_layer <- renderText({ 
  paste0("Currcent detect point layer: ",
         input$Point_class_1, "_",
         input$Point_class_2, "_",
         input$Point_class_3, "_P_",
         input$Draw_point_layers)
})

output$fix_area_draw_polygon_layer <- renderText({ 
  paste0("Currcent detect polygon layer: ",
         input$Polygon_class_1, "_",
         input$Polygon_class_2, "_",
         input$Polygon_class_3, "_G_",
         input$Draw_polygon_layers)
})

observeEvent(input$fix_area_auto_hotspot_draw, {
  progressSweetAlert(
    session = session, 
    id = "auto_hotspot_draw_myprogress",
    title = "Initiate Auto Hotspot Detection......",
    display_pct = TRUE, value = 0
  )
  
  
  point_shape <- Points_data_current_on_map$df
  point_shape <- point_shape %>% 
    filter(point_primary == input$Point_class_1) %>% 
    filter(point_secondary == input$Point_class_2) %>% 
    filter(point_tertiary == input$Point_class_3) %>% 
    filter(point_group == paste0("P_", input$Draw_point_layers))
  
  point_xy <- LonLat2XY(lon_deg = point_shape$point_lon,
                        lat_deg = point_shape$point_lat,
                        zoom = 12 + 1)
  
  point_xy$id <- point_shape$point_id
  point_xy$pair_down_id <- NA
  point_xy$pair_center_x <- NA
  point_xy$pair_center_y <- NA
  point_xy$max_number <- 0
  
  updateProgressBar(
    session = session,
    id = "auto_hotspot_draw_myprogress",
    title = "Slide margin and pixel size analysis",
    value = 10
  )
  
  text_file_name <- paste0(input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input)
  slide_name <- list.files(paste0("www/", text_file_name), pattern = ".ndpi", full.names = TRUE)
  slide_pixel_nm <- NDPI_point_to_nm(file_name = slide_name,
                                     x_point = c(0,0),
                                     y_point = c(0,1))
  slide_pixel_nm <- abs(slide_pixel_nm$y_nm[2] - slide_pixel_nm$y_nm[1])
  chunk_area <- input$fix_area_size_ready_to_draw %>% as.numeric()
  
  updateProgressBar(
    session = session,
    id = "auto_hotspot_draw_myprogress",
    title = "Intersect area and point analysis......",
    value = 20
  )
  
  if (input$fix_area_polygon_or_circle == "Polygon") {
    chunk_edge_nm <- (chunk_area^0.5) * 1000000
    chunk_pixel_size <- chunk_edge_nm/slide_pixel_nm
    point_xy <- fix_square_area_point_within_check_cpp(point_xy = point_xy, length_size = chunk_pixel_size)
    point_xy <- point_xy %>% arrange(desc(max_number))
    center_use_df <- point_xy[1,]
    #print(input$fix_area_auto_hotspot_number)
    
    updateProgressBar(
      session = session,
      id = "auto_hotspot_draw_myprogress",
      title = "select avalible area to present......",
      value = 80
    )
    
    if (input$fix_area_auto_hotspot_number > 1) {
      for (i in 2:nrow(point_xy)) {
        #print(i)
        #print(point_xy$pair_center_x[i])
        #print(center_use_df$pair_center_x)
        if ((is.na(point_xy$pair_center_x[i]) | is.na(point_xy$pair_center_y[i])) == FALSE) {
          if (sum(abs(point_xy$pair_center_x[i] - center_use_df$pair_center_x) <= chunk_pixel_size & 
                  abs(point_xy$pair_center_y[i] - center_use_df$pair_center_y) <= chunk_pixel_size) == 0) {
            center_use_df[nrow(center_use_df) + 1,] <- point_xy[i,]
          }
          if (nrow(center_use_df) == input$fix_area_auto_hotspot_number) {
            break
          }
        }
      }
    }
    #print(center_use_df)
    addpoly_chunk <- center_check_for_point_or_chunk(center_x_all = center_use_df$pair_center_x,
                                                     center_y_all = center_use_df$pair_center_y,
                                                     zoom_level = 12,
                                                     tile_size = chunk_pixel_size + 5,
                                                     point_vs_chunk = "chunk")
    #saveRDS(addpoly_chunk, "test.rds")
    #
    #print(addpoly_chunk)
    #raw_polygon <- addpoly_chunk[[1]]
  } else if (input$fix_area_polygon_or_circle == "Circle") {
    circle_radius_nm <- ((chunk_area/pi)^0.5) * 1000000
    circle_radius_pixel_size <- circle_radius_nm/slide_pixel_nm
    point_xy <- fix_circle_area_point_within_check_cpp(point_xy = point_xy, length_size = circle_radius_pixel_size * 2)
    point_xy <- point_xy %>% arrange(desc(max_number))
    diameter_square <- (circle_radius_pixel_size*2)^2
    center_use_df <- point_xy[1,]
    
    updateProgressBar(
      session = session,
      id = "auto_hotspot_draw_myprogress",
      title = "select avalible area to present......",
      value = 80
    )
    
    #print(input$fix_area_auto_hotspot_number)
    if (input$fix_area_auto_hotspot_number > 1) {
      for (i in 2:nrow(point_xy)) {
        #print(i)
        #print(point_xy$pair_center_x[i])
        #print(center_use_df$pair_center_x)
        if ((is.na(point_xy$pair_center_x[i]) | is.na(point_xy$pair_center_y[i])) == FALSE) {
          if (sum((point_xy$pair_center_x[i] - center_use_df$pair_center_x)^2 + 
                  (point_xy$pair_center_y[i] - center_use_df$pair_center_y)^2 <= diameter_square) == 0) {
            center_use_df[nrow(center_use_df) + 1,] <- point_xy[i,]
          }
          if (nrow(center_use_df) == input$fix_area_auto_hotspot_number) {
            break
          }
        }
      }
    }
    
    addpoly_chunk <- lapply(1:nrow(center_use_df), function(x){
      blank_poly <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
      blank_poly$geometry$coordinates <- array(0, c(1,361,2))
      lonlat_circle <- XY2LonLat(x = center_use_df$pair_center_x[x] + (circle_radius_pixel_size + 2.5) * sapply(0:360, function(x){cos(2*pi/360*x)}),
                                 y = center_use_df$pair_center_y[x] + (circle_radius_pixel_size + 2.5) * sapply(0:360, function(x){sin(2*pi/360*x)}),
                                 zoom = 12+1)
      blank_poly$geometry$coordinates[1,,1] <- lonlat_circle[,1]
      blank_poly$geometry$coordinates[1,,2] <- lonlat_circle[,2]
      blank_poly
    })
    #saveRDS(addpoly_chunk, "test.rds")
  }
  
  updateProgressBar(
    session = session,
    id = "auto_hotspot_draw_myprogress",
    title = "Display area on map!",
    value = 95
  )
  
  for (i in 1:length(addpoly_chunk)) {
    raw_polygon <- addpoly_chunk[[i]]
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
  
  sendSweetAlert(
    session = session,
    title = "Finish!!!",
    type = "success"
  )
  #leafletProxy("image_viewer", session) %>% addGeoJSON(addpoly_chunk, color = "red")
  #print(point_shape)
})
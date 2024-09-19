#observeEvent(input$hover_coordinates, {
#  print(input$hover_coordinates)
#})

observeEvent(input$Point_circle_switch, {
  
  if (is.null(Points_data_current_on_map$df) == FALSE) {
    modified_point_table <- Points_data_current_on_map$df %>% 
      filter(point_primary == input$Point_class_1) %>% 
      filter(point_secondary == input$Point_class_2) %>% 
      filter(point_tertiary == input$Point_class_3)
    leafletProxy("image_viewer", session) %>% 
      clearMarkers() %>% 
      addCircleMarkers(lng = as.numeric(modified_point_table$point_lon), 
                       lat = as.numeric(modified_point_table$point_lat), 
                       radius = sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting, #(7 + sum(input$Point_circle_switch) * 13), 
                       fillOpacity = (1 - sum(input$Point_circle_switch)), 
                       color = input$Change_point_color_order[as.integer(modified_point_table$point_group %>% str_replace_all("P_", ""))], 
                       group = modified_point_table$point_group, 
                       layerId = modified_point_table$point_id,
                       options = leafletOptions(pane = "Points")) 
					    
  }
})

observeEvent(input$Point_radius_setting, {
  if (is.null(Points_data_current_on_map$df) == FALSE) {
    modified_point_table <- Points_data_current_on_map$df %>% 
      filter(point_primary == input$Point_class_1) %>% 
      filter(point_secondary == input$Point_class_2) %>% 
      filter(point_tertiary == input$Point_class_3)
    leafletProxy("image_viewer", session) %>% 
      clearMarkers() %>% 
      addCircleMarkers(lng = as.numeric(modified_point_table$point_lon), 
                       lat = as.numeric(modified_point_table$point_lat), 
                       radius = sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting, #(7 + sum(input$Point_circle_switch) * 13), 
                       fillOpacity = (1 - sum(input$Point_circle_switch)), 
                       color = input$Change_point_color_order[as.integer(modified_point_table$point_group %>% str_replace_all("P_", ""))], 
                       group = modified_point_table$point_group, 
                       layerId = modified_point_table$point_id,
                       options = leafletOptions(pane = "Points")) 
  }
})

observeEvent(input$Circle_radius_setting, {
  if (is.null(Points_data_current_on_map$df) == FALSE) {
    modified_point_table <- Points_data_current_on_map$df %>% 
      filter(point_primary == input$Point_class_1) %>% 
      filter(point_secondary == input$Point_class_2) %>% 
      filter(point_tertiary == input$Point_class_3)
    leafletProxy("image_viewer", session) %>% 
      clearMarkers() %>% 
      addCircleMarkers(lng = as.numeric(modified_point_table$point_lon), 
                       lat = as.numeric(modified_point_table$point_lat), 
                       radius = sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting, #(7 + sum(input$Point_circle_switch) * 13), 
                       fillOpacity = (1 - sum(input$Point_circle_switch)), 
                       color = input$Change_point_color_order[as.integer(modified_point_table$point_group %>% str_replace_all("P_", ""))], 
                       group = modified_point_table$point_group, 
                       layerId = modified_point_table$point_id,
                       options = leafletOptions(pane = "Points")) 
  }
})



observeEvent(input$Polygon_opacity_value, {
  #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
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

output$Points_note_table <- renderRHandsontable({
  rhandsontable(Points_note_on_map$df) %>% 
    hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = 'white';
              td.style.color = 'black';
           }")
})

output$Polygons_note_table <- renderRHandsontable({
  rhandsontable(Polygons_note_on_map$df) %>% 
    hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = 'white';
              td.style.color = 'black';
           }")
})

output$Tags_note_table <- renderRHandsontable({
  rhandsontable(Tags_note_on_map$df) %>% 
    hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = 'white';
              td.style.color = 'black';
           }")
})

observeEvent(input$mouse_coordinates, {
  if (is.null(input$free_draw_start) == FALSE) {
    if (input$Polygon_free_draw_on_off) {
      if (input$free_draw_start == "Z") {
        list_length <- length(Free_draw_polygon_current_on_map$leaflet_geojson$geometry$coordinates[[1]])
        Free_draw_polygon_current_on_map$leaflet_geojson$geometry$coordinates[[1]][[list_length + 1]] <- list(input$mouse_coordinates[1], input$mouse_coordinates[2])
        leafletProxy("image_viewer", session) %>% 
          addCircleMarkers(lng = input$mouse_coordinates[1], 
                           lat = input$mouse_coordinates[2], 
                           radius = 3, 
                           color = input$Polygon_free_draw_color, 
                           group = "free_draw", 
                           layerId = paste0("free_draw_", list_length),
                           options = leafletOptions(pane = "Points"))
						   	
						   		   				   						   
      }
    }
  }
})


observeEvent(input$mouse,{


# current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
# current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
# modified_polygon_list <- Polygons_data_current_on_map$list[[current_polygon_class]]
  
#  leafletProxy("image_viewer", session) %>% 
#        clearGroup(group = "G_P")
  
#  if(length(modified_polygon_list) > 1 & input$mouse[1] > 2){
  
#    Annotation_count <- 0
#	Annotation <- c()
  
#    lon_boundary <- c(input$mouse[5],input$mouse[7],input$mouse[7],input$mouse[5])
#	lat_boundary <- c(input$mouse[6],input$mouse[6],input$mouse[4],input$mouse[4])

#     polygon_names <- readRDS("www/poly.RDS")
	 
#	 inside_points <- point.in.polygon(polygon_names$lon,polygon_names$lat,lon_boundary ,lat_boundary )
	 ###
#	 for(j in 1:length(inside_points)){	 
#	    if(inside_points[j] == 1){
#		   Annotation_count <- Annotation_count + 1 
#	       Annotation[Annotation_count] <- j
#	    }
#    }


#leafletProxy("image_viewer", session) %>% 
#        clearGroup(group = "G_P")


#for (i in Annotation) {

#    leafletProxy("image_viewer", session) %>% 
#      addGeoJSON(modified_polygon_list[[polygon_names$data[i]]],  
#                 group = "G_P", 
#                 fillOpacity = input$Polygon_opacity_value, 
#                 color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)],
#                 options = leafletOptions(pane = "Polygons"))
#    }
#}



#if(length(Polygons_data_current_on_map$list[[polygon_show_name]]) > 2000){
#}

if (length(Points_data_current_on_map$df$point_id) > 20000 & input$mouse[1] < 1.5){
	 interface_Annotation <- c()
	 interface_Annotation_count <- 0

	 lon_boundary <- c(input$mouse[5],input$mouse[7],input$mouse[7],input$mouse[5])
	 lat_boundary <- c(input$mouse[6],input$mouse[6],input$mouse[4],input$mouse[4])

    x <- length(Points_data_current_on_map$df$point_id)/ceiling(length(Points_data_current_on_map$df$point_id)/20000)
	sample_points_x <- sample(1:length(Points_data_current_on_map$df$point_id),x, replace=FALSE)
	 
	 inside <- point.in.polygon(Points_data_current_on_map$df$point_lon[sample_points_x],Points_data_current_on_map$df$point_lat[sample_points_x],lon_boundary ,lat_boundary )

 
 	 for(j in 1:length(inside)){	 
	    if(inside[j] == 1){
		   interface_Annotation_count <- interface_Annotation_count + 1 
	       interface_Annotation[interface_Annotation_count] <- j
	    }
    }

	  modified_point_table <- Points_data_current_on_map$df %>% 
      filter(point_primary == input$Point_class_1) %>% 
      filter(point_secondary == input$Point_class_2) %>% 
      filter(point_tertiary == input$Point_class_3)
      leafletProxy("image_viewer", session) %>% 
      clearMarkers() %>% 
      addCircleMarkers(lng = as.numeric(modified_point_table$point_lon[sample_points_x[interface_Annotation]]), 
                       lat = as.numeric(modified_point_table$point_lat[sample_points_x[interface_Annotation]]), 
                       radius = sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting, #(7 + sum(input$Point_circle_switch) * 13), 
                      fillOpacity = (1 - sum(input$Point_circle_switch)), 
                       color = input$Change_point_color_order[as.integer(modified_point_table$point_group[sample_points_x[interface_Annotation]] %>% str_replace_all("P_", ""))],  
                       group = modified_point_table$point_group[sample_points_x[interface_Annotation]], 
                       layerId = modified_point_table$point_id[sample_points_x[interface_Annotation]],
                       options = leafletOptions(pane = "Points")) 
	  }
#######################################################################################################################
	 
if (length(Points_data_current_on_map$df$point_id) > 20000 & input$mouse[1] > 1.5 ){
  	 
interface_Annotation_move_scale <- c()
interface_Annotation_count_move_scale <- 0
	 
	 lon_move_scale <- c(input$mouse[5],input$mouse[7],input$mouse[7],input$mouse[5])
	 lat_move_scale <- c(input$mouse[6],input$mouse[6],input$mouse[4],input$mouse[4])
	 
	 inside_move_scale <- point.in.polygon(Points_data_current_on_map$df$point_lon,Points_data_current_on_map$df$point_lat,lon_move_scale ,lat_move_scale )
	 
	 for(i in 1:length(inside_move_scale)){	 
	    if(inside_move_scale[i] == 1){
		   interface_Annotation_count_move_scale  <- interface_Annotation_count_move_scale + 1 
	       interface_Annotation_move_scale[interface_Annotation_count_move_scale ] <- i
	 }
  }

	  modified_point_table <- Points_data_current_on_map$df %>% 
      filter(point_primary == input$Point_class_1) %>% 
      filter(point_secondary == input$Point_class_2) %>% 
      filter(point_tertiary == input$Point_class_3)
      leafletProxy("image_viewer", session) %>% 
      clearMarkers() %>% 
      addCircleMarkers(lng = as.numeric(modified_point_table$point_lon[interface_Annotation_move_scale]), 
                       lat = as.numeric(modified_point_table$point_lat[interface_Annotation_move_scale]), 
                       radius = sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting, #(7 + sum(input$Point_circle_switch) * 13), 
                      fillOpacity = (1 - sum(input$Point_circle_switch)), 
                       color = input$Change_point_color_order[as.integer(modified_point_table$point_group[interface_Annotation_move_scale] %>% str_replace_all("P_", ""))],  
                       group = modified_point_table$point_group[interface_Annotation_move_scale], 
                       layerId = modified_point_table$point_id[interface_Annotation_move_scale],
                       options = leafletOptions(pane = "Points")) 
	  }
})
#11/04-----------------AddHeatmap-----------------
 # observeEvent(input$free_draw_start, {
 #   if (input$Polygon_free_draw_on_off) {
 #     if (input$free_draw_start == "B") {
        #print(input$mouse_coordinates)
        
#		polygons <- readRDS("E:/Desktop/leaflet_image/www/Slide/RUN_AI/S112_15027G_1-/polygons.rds")
		
       
	   
		
#		point_in_polygon <- point.in.polygon(c(-180.0896,-173.0896),c(90.72471,83.72478),polygons[[1]][[1]]$geometry$coordinates[[1]][,1],polygons[[1]][[1]]$geometry$coordinates[[1]][,2])
		
		#print(point_in_polygon)
		
		#print(st_union(c(polygons[[1]][[1]]$geometry$coordinates[[1]][,1],polygons[[1]][[1]]$geometry$coordinates[[1]][,2]),c(polygons[[1]][[1]]$geometry$coordinates[[1]][,1],polygons[[1]][[1]]$geometry$coordinates[[1]][,2])))
		
	#	for( i in 1:100){
    #    leafletProxy("image_viewer", session) %>% 
    #      addPolygons(lng = polygons[[1]][[1]]$geometry$coordinates[[1]][,1], 
    #                  lat = polygons[[1]][[1]]$geometry$coordinates[[1]][,2],
	#   				  opacity = 1,
	#   				  fillOpacity = 0 )
                           #color = input$Polygon_free_draw_color, 
                           #group = "free_draw")
                           #layerId = paste0("free_draw_", list_length),
                          # options = leafletOptions(pane = "Points"))
	#	}				  
						   	
						   		   				   						   
  #    }
 # }
#})

#11/04-----------------AddHeatmap-----------------

#observeEvent(input$Polygon_viewing_color,{
  #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
#  current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
#  modified_polygon_list <- Polygons_data_current_on_map$list[[current_polygon_class]]
  
#  polygon_names <- modified_polygon_list %>% names()
  
#  leafletProxy("image_viewer", session) %>% 
#    clearGeoJSON()
  
#  for (i in polygon_names) {
#    leafletProxy("image_viewer", session) %>% 
#      addGeoJSON(modified_polygon_list[[i]], 
#                 layerId = i, 
#                 group = "GP", 
#                 fillOpacity = input$Polygon_opacity_value, 
#                 color = input$Polygon_viewing_color,
#                 options = leafletOptions(pane = "Polygons"))
#  }
#})

observeEvent(input$image_viewer_geojson_click, { # update the location selectInput on map clicks
  if (is.null(input$delete_mode) == FALSE) {
    if (input$delete_mode == 1) {
      p <- input$image_viewer_geojson_click
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
  }
})



observeEvent(input$free_draw_start, {
  if (input$Polygon_free_draw_on_off) {
    if (input$free_draw_start == "S") { 

     start <- Sys.time()	
	
	progressSweetAlert(
      session = session, id = "Polygon_progress",
      title = "Start Polygon processing",
      display_pct = TRUE, value = 0
    )
	
	
	
tryCatch({	
      raw_polygon <- Free_draw_polygon_current_on_map$leaflet_geojson
      polygon_length <- raw_polygon$geometry$coordinates[[1]] %>% length()
	  
    tryCatch({

     # if (polygon_length > 0 ) {
	   if (polygon_length > 2 ) {
	   
	   
        Free_draw_polygon_current_on_map$leaflet_geojson <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
        #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
        current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
		
		
        if (is.null(Polygons_data_current_on_map$list[[current_polygon_class]])) {
          Polygons_data_current_on_map$list[[current_polygon_class]] <- list()
        }
        current_all_id <- Polygons_data_current_on_map$list[[current_polygon_class]] %>% names() %>% as.integer()
        
        raw_polygon_coor <- lawn_coordall(raw_polygon)
        clip_polygon <- polysimplify(list(list(x = raw_polygon_coor[,1], y = raw_polygon_coor[,2])))
		
		#---------------------
		 clip_polygon <- samplify_polygon(clip_polygon)
		#---------------------

		
        if (length(current_all_id) == 0) {
          new_polygon_id <- sample(1:100000, length(clip_polygon)) %>% as.character()
        } else {
          sample_number <- c(1:100000)
          sample_number <- sample_number[-current_all_id]
          new_polygon_id <- sample(sample_number, length(clip_polygon)) %>% as.character()
        }
		updateProgressBar(
               session = session,
               title = paste0("Start Polygon processing..."),
               id = "Polygon_progress",
               value = 10
            )
	    Sys.sleep(1)
			
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
          
          Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]] <- new_polygon
          Polygons_data_current_on_map$bbox[[new_polygon_id[poly_number]]] <- new_polygon_bbox
		  
	
################################################1225################################################	
		   
          #---------intersection----------overlap_name-----------#
		  current_all_id_name <- current_all_id %>% as.character()
		  current_all_id_overlay <- c()
		  current_all_id_overlay_count <- 1
		  
		 
        if (length(current_all_id_name) > 0) {		 
		  for( ii in 1:length(current_all_id_name)){
		  
		     updateProgressBar(
               session = session, 
               title = paste0("Polygons processing...",poly_number,"/",length(clip_polygon)),
			   id = "Polygon_progress",
               value = (ii*(40/length(current_all_id_name))) + 10
            )
		
		    if(length(Polygons_data_current_on_map$list[[current_polygon_class]][[ii]]$geometry$coordinates[[1]])==1 & length(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]])==1){
			 
			 intersection <- py$intersection(Polygons_data_current_on_map$list[[current_polygon_class]][[ii]]$geometry$coordinates[,,1],Polygons_data_current_on_map$list[[current_polygon_class]][[ii]]$geometry$coordinates[,,2],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[,,1],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[,,2])				
			 
			 if(intersection == TRUE){
				current_all_id_overlay[current_all_id_overlay_count] <- current_all_id_name[ii]
				current_all_id_overlay_count <- current_all_id_overlay_count + 1
			 }
			}
			
			if(length(Polygons_data_current_on_map$list[[current_polygon_class]][[ii]]$geometry$coordinates[[1]]) > 1 & length(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]])==1){
			   
			  intersection <- py$intersection(Polygons_data_current_on_map$list[[current_polygon_class]][[ii]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[ii]]$geometry$coordinates[[1]][,2],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[,,1],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[,,2])	
			
			  if(intersection == TRUE){
				current_all_id_overlay[current_all_id_overlay_count] <- current_all_id_name[ii]
				current_all_id_overlay_count <- current_all_id_overlay_count + 1
			 }
			}
			if(length(Polygons_data_current_on_map$list[[current_polygon_class]][[ii]]$geometry$coordinates[[1]]) > 1  & length(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]]) > 1){
			    
			  intersection <- py$intersection(Polygons_data_current_on_map$list[[current_polygon_class]][[ii]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[ii]]$geometry$coordinates[[1]][,2],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]][,2])
			
			  if(intersection == TRUE){
				current_all_id_overlay[current_all_id_overlay_count] <- current_all_id_name[ii]
				current_all_id_overlay_count <- current_all_id_overlay_count + 1
			 }
			}
			if(length(Polygons_data_current_on_map$list[[current_polygon_class]][[ii]]$geometry$coordinates[[1]]) == 1  & length(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]]) > 1){

				intersection <- py$intersection(Polygons_data_current_on_map$list[[current_polygon_class]][[ii]]$geometry$coordinates[,,1],Polygons_data_current_on_map$list[[current_polygon_class]][[ii]]$geometry$coordinates[,,2],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]][,2])
				if(intersection == TRUE){
				  current_all_id_overlay[current_all_id_overlay_count] <- current_all_id_name[ii]
				  current_all_id_overlay_count <- current_all_id_overlay_count + 1
			  }
			
			}
           }
		 } 
		   
		  overlap_name <- unique(current_all_id_overlay)
		  #---------intersection----------overlap_name-----------#

		  polygon_count <- c()
		  times <- 1
		  count_num <- 1
		  overlap_name <- overlap_name[!is.na(overlap_name)]
		 
		  
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

			 updateProgressBar(
               session = session,
               title = paste0("Polygons merging...",poly_number,"/",length(clip_polygon)),
			   id = "Polygon_progress",
               value = (times*(50/length(overlap_name))) + 50
              )
			  times <- times + 1
			
              if (lawn_boolean_within(Polygons_data_current_on_map$list[[current_polygon_class]][[i]], Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]])) {
			  
	
				polygon_count[count_num] <- i	
                count_num <- count_num + 1
                 
				 leafletProxy("image_viewer", session) %>%	
                    removeGeoJSON(layerId = i) %>%
                    removeGeoJSON(layerId = new_polygon_id[poly_number])
                #Polygons_data_current_on_map$list[[current_polygon_class]][[i]] <- NULL
                #Polygons_data_current_on_map$bbox[[i]] <- NULL
		 
							 
              }
			 if(length(overlap_name) > 0){
			 
			   if(length(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]])==1 & length(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]])==1){
			      

			      Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]] <- lawn_union(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]], 
                                                                                                                          Polygons_data_current_on_map$list[[current_polygon_class]][[i]])
                  polygon_count[count_num] <- i	
                  count_num <- count_num + 1
				  
                   leafletProxy("image_viewer", session) %>%	
                    removeGeoJSON(layerId = i) %>%
                    removeGeoJSON(layerId = new_polygon_id[poly_number])				  
																														  
			     
			    }


			  if(length(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]]) > 1 & length(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]])==1){
			    ### 
				polygon_add <- c()
				All_polygons_length <- length(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates)
	
				
				list_num <- py$list_num(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]][,2],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[,,1],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[,,2])	
				#---Intersection_try---#
				
				polygon_hole <- py$interior_coords(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]][,2],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[,,1],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[,,2])
                

                polygon_count[count_num] <- i	
                count_num <- count_num + 1
				 ##############
				 #------------------------------ADD_POLYGON_HOLES------------------------------#
               if(length(polygon_hole) > 0){
			   
				 for(inter in 1:length(polygon_hole)){#
				 
				   polygon_add_inside <- c()
				   All_polygons_length <- All_polygons_length + 1 

				   #---#
				   double_num_inside <- length(polygon_hole[[inter]]) * 2

				   for( z_inside in 1:double_num_inside){
				      
                     if(z_inside  <= length(polygon_hole[[inter]])){  					
				        polygon_add_inside[z_inside ] <- polygon_hole[[inter]][[z_inside]][[2]]
				     }
                     if(z_inside  > length(polygon_hole[[inter]])){
                        polygon_add_inside[z_inside] <- polygon_hole[[inter]][[z_inside-length(polygon_hole[[inter]])]][[1]]
                      }				  
	
			        }	
				   x.mat_inside <- matrix(polygon_add_inside[1:length(polygon_add_inside)],length(polygon_hole[[inter]]), 2)
				   
                   Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[All_polygons_length]] <- x.mat_inside
				   #---#
				   }#
				
				}
				#------------------------------ADD_POLYGON_HOLES------------------------------#
				
				double_num <- length(list_num) * 2

				for( z in 1:double_num){
				    
                 if(z <= length(list_num)){  					
				    polygon_add[z] <- list_num[[z]][[2]]
				 }
                 if(z > length(list_num)){
                    polygon_add[z] <- list_num[[z-length(list_num)]][[1]]
                 }				  
	
			    }	
				x.mat <- matrix(polygon_add[1:length(polygon_add)],length(list_num), 2)
			    Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]] <- x.mat

			   Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates <- NULL
			   Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates <- Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates

			    #----#
				#Polygons_data_current_on_map$list[[current_polygon_class]][[i]] <- NULL
                #Polygons_data_current_on_map$bbox[[i]] <- NULL
				Polygons_data_current_on_map$bbox[[new_polygon_id[poly_number]]] <- lawn_bbox(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]])
                #---##
				leafletProxy("image_viewer", session) %>%	
                    removeGeoJSON(layerId = i) %>%
                    removeGeoJSON(layerId = new_polygon_id[poly_number])

			}
           if(length(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]]) > 1  & length(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]]) > 1){
			   ###
				polygon_add <- c()
				
				
				new_length <- length(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates)
				sum_length <- length(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates)
				

				
				#source_python("E:/Desktop/leaflet_image/program_script/Pre_load_function_cpp/polygon.py")
				
                list_num <- py$list_num(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]][,2],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]][,2])
				
				
				polygon_count[count_num] <- i	
                count_num <- count_num + 1
				
				
				##############
				 All_polygons_length <- list()
				 #------------------------------ADD_POLYGON_HOLES------------------------------#
			   polygon_hole <- py$interior_coords(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]][,2],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]][,2])	   
			   
               if(length(polygon_hole) > 0){
			   
				 for(inter in 1:length(polygon_hole)){#
				 
				   polygon_add_inside <- c()

				   #---#
				   double_num_inside <- length(polygon_hole[[inter]]) * 2

				   for( z_inside in 1:double_num_inside){
				      
                     if(z_inside  <= length(polygon_hole[[inter]])){  					
				        polygon_add_inside[z_inside ] <- polygon_hole[[inter]][[z_inside]][[2]]
				     }
                     if(z_inside  > length(polygon_hole[[inter]])){
                        polygon_add_inside[z_inside] <- polygon_hole[[inter]][[z_inside-length(polygon_hole[[inter]])]][[1]]
                      }				  
	
			        }	
				   x.mat_inside <- matrix(polygon_add_inside[1:length(polygon_add_inside)],length(polygon_hole[[inter]]), 2)
                   All_polygons_length[[inter]] <- x.mat_inside
				   #---#
				   }
				
				}
				#------------------------------ADD_POLYGON_HOLES------------------------------#

				
                double_num <- length(list_num) * 2
				
				for( zc in 1:double_num){
				    
                 if(zc <= length(list_num)){  					
				    polygon_add[zc] <- list_num[[zc]][[2]]
				 }
                 if(zc > length(list_num)){
                    polygon_add[zc] <- list_num[[zc-length(list_num)]][[1]]
                 }				  
	
			    }
				
               x.mat <- matrix(polygon_add[1:length(polygon_add)],length(list_num), 2)
			   Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]] <- x.mat

			   if(new_length >= 2){
			   for(add_hole in 2:new_length){
			   
				 sum_length <- sum_length + 1
			     Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[sum_length]] <- Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[add_hole]]
			     }
              }
			  #add_another#
			  add_another <- length(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates)
			  
			  if(length(All_polygons_length) > 0 ){
			     for(add_another_hole in 1:length(All_polygons_length)){
			  
			       add_another <- add_another + 1
			       Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[add_another]] <- All_polygons_length[[add_another_hole]]
			     }
                
			 }
			 #add_another#
			 
			  Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates <- NULL
			  Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates <- Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates

			   #----#
				#Polygons_data_current_on_map$list[[current_polygon_class]][[i]] <- NULL
                #Polygons_data_current_on_map$bbox[[i]] <- NULL
				Polygons_data_current_on_map$bbox[[new_polygon_id[poly_number]]] <- lawn_bbox(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]])
                #---#
                  
              leafletProxy("image_viewer", session) %>%	
                    removeGeoJSON(layerId = i) %>%
                    removeGeoJSON(layerId = new_polygon_id[poly_number])					


            }
			 if(length(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]]) == 1  & length(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]]) > 1){
			   ###
				polygon_add <- c()

				list_num <- py$list_num(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[,,1],Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[,,2],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]][,2])
				
				polygon_count[count_num] <- i	
                count_num <- count_num + 1

				##############
				 All_polygons_length <- list()
				 #------------------------------ADD_POLYGON_HOLES------------------------------#
			   polygon_hole <- py$interior_coords(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[,,1],Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[,,2],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[[1]][,2])	   
			   
               if(length(polygon_hole) > 0){
			   
				 for(inter in 1:length(polygon_hole)){#
				 
				   polygon_add_inside <- c()

				   #---#
				   double_num_inside <- length(polygon_hole[[inter]]) * 2

				   for( z_inside in 1:double_num_inside){
				      
                     if(z_inside  <= length(polygon_hole[[inter]])){  					
				        polygon_add_inside[z_inside ] <- polygon_hole[[inter]][[z_inside]][[2]]
				     }
                     if(z_inside  > length(polygon_hole[[inter]])){
                        polygon_add_inside[z_inside] <- polygon_hole[[inter]][[z_inside-length(polygon_hole[[inter]])]][[1]]
                      }				  
	
			        }	
				   x.mat_inside <- matrix(polygon_add_inside[1:length(polygon_add_inside)],length(polygon_hole[[inter]]), 2)
                   All_polygons_length[[inter]] <- x.mat_inside
				   #---#
				   }
				
				}
				#------------------------------ADD_POLYGON_HOLES------------------------------#
				
                double_num <- length(list_num) * 2
				
				for( z in 1:double_num){
				    
                 if(z <= length(list_num)){  					
				    polygon_add[z] <- list_num[[z]][[2]]
				 }
                 if(z > length(list_num)){
                    polygon_add[z] <- list_num[[z-length(list_num)]][[1]]
                 }				  
	
			    }

               x.mat <- matrix(polygon_add[1:length(polygon_add)],length(list_num), 2)	   
			   Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates <- Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates
			   Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]] <- x.mat
			   
			   add_another <- length(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates)
			  
			  if(length(All_polygons_length) > 0 ){
			     for(add_another_hole_D in 1:length(All_polygons_length)){
			  
			       add_another <- add_another + 1
			       Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[add_another]] <- All_polygons_length[[add_another_hole_D]]
			     }
                
			 }
			   
			  Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates <- NULL
			  Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates <- Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates

			   #----#
				#Polygons_data_current_on_map$list[[current_polygon_class]][[i]] <- NULL
                #Polygons_data_current_on_map$bbox[[i]] <- NULL

				Polygons_data_current_on_map$bbox[[new_polygon_id[poly_number]]] <- lawn_bbox(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]])
                #---#
				leafletProxy("image_viewer", session) %>%	
                    removeGeoJSON(layerId = i) %>%
                    removeGeoJSON(layerId = new_polygon_id[poly_number])


            }

################################################1225################################################

              ############################################################################################
                } #else {
                  #leafletProxy("image_viewer", session) %>% 
                  #  addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]], 
                  #             group = paste0("G_", input$Draw_polygon_layers),
                  #             fillOpacity = input$Polygon_opacity_value, 
                  #             color ="green", #input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                  #             layerId = new_polygon_id[poly_number],
                  #             options = leafletOptions(pane = "Polygons"))

                #}}
               else if (lawn_boolean_within(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]], Polygons_data_current_on_map$list[[current_polygon_class]][[i]])) {
                Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]] <- NULL
                Polygons_data_current_on_map$bbox[[new_polygon_id[poly_number]]] <- NULL
              } else {
                leafletProxy("image_viewer", session) %>%
                  removeGeoJSON(layerId = i) %>% 
                  removeGeoJSON(layerId = new_polygon_id[poly_number]) %>% 				
                  addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]], 
                             group = paste0("G_", input$Draw_polygon_layers),
                             fillOpacity = input$Polygon_opacity_value, 
                             color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                             layerId = new_polygon_id[poly_number],
                             options = leafletOptions(pane = "Polygons"))
              }
            }
			#saveRDS(Polygons_data_current_on_map$list[[current_polygon_class]], "test2.rds")
			#saveRDS(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]],paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/","test.rds"))

			leafletProxy("image_viewer", session) %>%
                  removeGeoJSON(layerId = i) %>% 
                  removeGeoJSON(layerId = new_polygon_id[poly_number]) %>% 				
                  addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]], 
                             group = paste0("G_", input$Draw_polygon_layers),
                             fillOpacity = input$Polygon_opacity_value, 
                             color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                             layerId = new_polygon_id[poly_number],
                             options = leafletOptions(pane = "Polygons"))
			#-------------polygon_count---------------#

			all_delete <- c(unique(polygon_count),overlap_name)
			if(length(unique(polygon_count)) == length(overlap_name)){

			   for(a in unique(all_delete)){
			
		          new_poly <- which(Polygons_data_current_on_map$list[[current_polygon_class]] %>% names() == a)
				  Polygons_data_current_on_map$list[[current_polygon_class]][[a]] <- NULL
	             
             }
		   }
		   #-------------polygon_count---------------#
		  }
        }
		
		closeSweetAlert(session = session)
		
		saveRDS(Polygons_data_current_on_map$list,paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/temp/previous.rds"))

        leafletProxy("image_viewer", session) %>% 
          clearGroup(group = "free_draw")
		  
###############################################################################################################################################################################################################	
		  #print("Polygons_history_current_on_map")
		  #print(Polygons_history_current_on_map$list)
		  
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
		
      }else{


	     leafletProxy("image_viewer", session) %>% 
              clearGroup(group = "free_draw")
			   
		pre_length <- length(Free_draw_polygon_current_on_map$leaflet_geojson$geometry$coordinates[[1]])
		Free_draw_polygon_current_on_map$leaflet_geojson$geometry$coordinates[[1]] <- Free_draw_polygon_current_on_map$leaflet_geojson$geometry$coordinates[[1]][-c(1:pre_length)]

		 sendSweetAlert(
                  session = session,
                  title = "Error !!",
                  text = "Function operation error, restart operation!",
                  type = "error"
               )   
	  
	  }
	  
	#----

    }, error = function(e) {

  confirmSweetAlert(
    session = session,
    inputId = "polygons_error",
	type = "warning",
    title = "Polygon processing error",
    btn_labels =  c("previous step", "Ignore errors"),
    btn_colors = c("#FE642E", "#04B404")
  )

	
    remove_polygon <- c(overlap_name)
    leafletProxy("image_viewer", session) %>% 
               clearGroup(group = "free_draw")
		
   current_all <- current_all_id %>% as.character()
   now_all <- Polygons_data_current_on_map$list[[current_polygon_class]]%>% names()


   diff <- c(setdiff(overlap_name,polygon_count),new_polygon_id)
   
	if(length(polygon_count)> 0)
	for(b in diff){	
	   new_poly <- which(Polygons_data_current_on_map$list[[current_polygon_class]] %>% names() == b)
	   Polygons_data_current_on_map$list[[current_polygon_class]][[b]] <- NULL
    }
    
	
    leafletProxy("image_viewer", session) %>%
	              removeGeoJSON(layerId = remove_polygon) %>% 
                  removeGeoJSON(layerId = new_polygon_id[poly_number])
		  
	for (zzz in remove_polygon){
	leafletProxy("image_viewer", session) %>% 
              addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[zzz]], 
                         group = paste0("G_", input$Draw_polygon_layers),
                         fillOpacity = input$Polygon_opacity_value, 
                         color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                         layerId = zzz,
                         options = leafletOptions(pane = "Polygons"))	
    }
 	  
})


    }, error = function(e) {
	
    leafletProxy("image_viewer", session) %>% 
               clearGroup(group = "free_draw")
			   
    confirmSweetAlert(
       session = session,
       inputId = "polygons_error",
	   type = "warning",
       title = "Polygon processing error",
       btn_labels =  c("previous step", "Ignore errors"),
       btn_colors = c("#FE642E", "#04B404")
    )
			   
	   
	Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]] <- NULL
	
		  
})


 new_polygon$geometry$coordinates <- list(choose_poly)
          new_polygon$geometry$coordinates[[1]][[new_polygon$geometry$coordinates[[1]] %>% length() + 1]] <- new_polygon$geometry$coordinates[[1]][[1]]
          new_polygon$properties$layerId <- new_polygon_id[poly_number]
          new_polygon$properties$edit_id <- new_polygon_id[poly_number]
          new_polygon$properties$`_leaflet_id` <- new_polygon_id[poly_number] %>% as.integer()
          new_polygon <- new_polygon %>% lawn_simplify(tolerance = input$Polygon_free_draw_simplify)
          new_polygon_bbox <- lawn_bbox(new_polygon)
################################################1225################################################
	end <- Sys.time()
	result <- end - start
	print(result)
      
    } else if (input$free_draw_start == "D") {
      raw_polygon <- Free_draw_polygon_current_on_map$leaflet_geojson
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
          
          current_polygon_matrix <- matrix(data = c((new_polygon_bbox[1] > current_polygon_data[,1]),
                                                    (new_polygon_bbox[1] > current_polygon_data[,3]),
                                                    (new_polygon_bbox[3] > current_polygon_data[,1]),
                                                    (new_polygon_bbox[3] > current_polygon_data[,3]),
                                                    (new_polygon_bbox[2] > current_polygon_data[,2]),
                                                    (new_polygon_bbox[2] > current_polygon_data[,4]),
                                                    (new_polygon_bbox[4] > current_polygon_data[,2]),
                                                    (new_polygon_bbox[4] > current_polygon_data[,4])
          ), ncol = 8, byrow = FALSE)
          
          check_matrix_1 <- current_polygon_matrix[,1] + current_polygon_matrix[,2] + current_polygon_matrix[,3] + current_polygon_matrix[,4]
          check_matrix_2 <- current_polygon_matrix[,5] + current_polygon_matrix[,6] + current_polygon_matrix[,7] + current_polygon_matrix[,8]
          
          check_matrix_1 <- (check_matrix_1 > 0) & (check_matrix_1 < 4)
          check_matrix_2 <- (check_matrix_2 > 0) & (check_matrix_2 < 4)
          
          use_matrix <- which(check_matrix_1 & check_matrix_2)
          
          current_all_id <- Polygons_data_current_on_map$list[[current_polygon_class]] %>% names() %>% as.integer()
          overlap_name <- current_all_id[use_matrix] %>% as.character()
          
          
          if (length(overlap_name) == 0) {
            #Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id]] <- NULL
            #Polygons_data_current_on_map$bbox[[new_polygon_id]] <- NULL
          } else {
            for (i in overlap_name) {
              if (lawn_boolean_within(Polygons_data_current_on_map$list[[current_polygon_class]][[i]], new_polygon)) {
                Polygons_data_current_on_map$list[[current_polygon_class]][[i]] <- NULL
                Polygons_data_current_on_map$bbox[[i]] <- NULL
                leafletProxy("image_viewer", session) %>% 
                  removeGeoJSON(layerId = i)
              } else if (lawn_boolean_within(new_polygon, Polygons_data_current_on_map$list[[current_polygon_class]][[i]])) {
                Polygons_data_current_on_map$list[[current_polygon_class]][[i]] <- lawn_difference(Polygons_data_current_on_map$list[[current_polygon_class]][[i]], new_polygon)
                Polygons_data_current_on_map$bbox[[i]] <- lawn_bbox(Polygons_data_current_on_map$list[[current_polygon_class]][[i]])
                leafletProxy("image_viewer", session) %>% 
                  removeGeoJSON(layerId = i) %>% 
                  addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[i]], 
                             group = paste0("G_", input$Draw_polygon_layers),
                             fillOpacity = input$Polygon_opacity_value, 
                             color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                             layerId = i,
                             options = leafletOptions(pane = "Polygons"))
              } else if (lawn_boolean_overlap(Polygons_data_current_on_map$list[[current_polygon_class]][[i]], new_polygon)) {
                Polygons_data_current_on_map$list[[current_polygon_class]][[i]] <- lawn_difference(Polygons_data_current_on_map$list[[current_polygon_class]][[i]], new_polygon)
                if (Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$type == "MultiPolygon") {
                  sample_number <- c(1:100000)
                  sample_number <- sample_number[-current_all_id]
                  new_polygon_id_use <- sample(sample_number, (length(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates) - 1)) %>% as.character()
                  
                  for (k in 2:length(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates)) {
                    
                    Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id_use[k-1]]] <- new_polygon
                    Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id_use[k-1]]]$properties$layerId <- new_polygon_id_use[k-1]
                    Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id_use[k-1]]]$properties$edit_id <- new_polygon_id_use[k-1]
                    Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id_use[k-1]]]$properties$`_leaflet_id` <- new_polygon_id_use[k-1] %>% as.integer()
                    
                    Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id_use[k-1]]]$geometry$coordinates <- Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[k]]
                    Polygons_data_current_on_map$bbox[[new_polygon_id_use[k-1]]] <- lawn_bbox(Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id_use[k-1]]])
                    leafletProxy("image_viewer", session) %>% 
                      addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id_use[k-1]]], 
                                 group = paste0("G_", input$Draw_polygon_layers),
                                 fillOpacity = input$Polygon_opacity_value, 
                                 color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                                 layerId = new_polygon_id_use[k-1],
                                 options = leafletOptions(pane = "Polygons"))
                  }
                  
                  temp_polygon <- Polygons_data_current_on_map$list[[current_polygon_class]][[i]] 
                  Polygons_data_current_on_map$list[[current_polygon_class]][[i]] <- new_polygon
                  Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$properties <- temp_polygon$properties
                  Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates <- temp_polygon$geometry$coordinates[[1]]
                  Polygons_data_current_on_map$bbox[[i]] <- lawn_bbox(Polygons_data_current_on_map$list[[current_polygon_class]][[i]])
                  leafletProxy("image_viewer", session) %>% 
                    removeGeoJSON(layerId = i) %>% 
                    addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[i]], 
                               group = paste0("G_", input$Draw_polygon_layers),
                               fillOpacity = input$Polygon_opacity_value, 
                               color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                               layerId = i,
                               options = leafletOptions(pane = "Polygons"))
                } else {
                  Polygons_data_current_on_map$bbox[[i]] <- lawn_bbox(Polygons_data_current_on_map$list[[current_polygon_class]][[i]])
                  leafletProxy("image_viewer", session) %>% 
                    removeGeoJSON(layerId = i) %>% 
                    addGeoJSON(geojson = Polygons_data_current_on_map$list[[current_polygon_class]][[i]], 
                               group = paste0("G_", input$Draw_polygon_layers),
                               fillOpacity = input$Polygon_opacity_value, 
                               color = input$Change_polygon_color_order[as.integer(input$Draw_polygon_layers)], 
                               layerId = i,
                               options = leafletOptions(pane = "Polygons"))
                }
              }
            }
          }
        }
        
        
        leafletProxy("image_viewer", session) %>% 
          clearGroup(group = "free_draw")
        
        #saveRDS(new_polygon, "test.rds")
        #saveRDS(Polygons_data_current_on_map$list[[current_polygon_class]], "test2.rds")

        #Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id]] <- new_polygon
        
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
        
        #saveRDS(Polygons_data_current_on_map$list, "test2.rds")
        
      }
      
    } else if (input$free_draw_start == "R") {
      Free_draw_polygon_current_on_map$leaflet_geojson <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
      leafletProxy("image_viewer", session) %>% 
        clearGroup(group = "free_draw")
    } else if (input$free_draw_start == "A") {
      pre_length <- length(Free_draw_polygon_current_on_map$leaflet_geojson$geometry$coordinates[[1]])
      if (pre_length > 10) {
        Free_draw_polygon_current_on_map$leaflet_geojson$geometry$coordinates[[1]] <- Free_draw_polygon_current_on_map$leaflet_geojson$geometry$coordinates[[1]][1:(pre_length - 10)]
        for (i in 1:10) {
          leafletProxy("image_viewer", session) %>% 
            removeMarker(layerId = paste0("free_draw_", pre_length - 10 + i))
        }
      } else {
        Free_draw_polygon_current_on_map$leaflet_geojson <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
        leafletProxy("image_viewer", session) %>% 
          clearGroup(group = "free_draw")
      }
    } else if (input$free_draw_start == "1") {
      updateAwesomeRadio(session, inputId = "Draw_point_layers", selected = "1")
    } else if (input$free_draw_start == "2") {
      updateAwesomeRadio(session, inputId = "Draw_point_layers", selected = "2")
    } else if (input$free_draw_start == "3") {
      updateAwesomeRadio(session, inputId = "Draw_point_layers", selected = "3")
    } else if (input$free_draw_start == "4") {
      updateAwesomeRadio(session, inputId = "Draw_point_layers", selected = "4")
    } else if (input$free_draw_start == "5") {
      updateAwesomeRadio(session, inputId = "Draw_point_layers", selected = "5")
    } else if (input$free_draw_start == "6") {
      updateAwesomeRadio(session, inputId = "Draw_point_layers", selected = "6")
    } else if (input$free_draw_start == "7") {
      updateAwesomeRadio(session, inputId = "Draw_point_layers", selected = "7")
    } else if (input$free_draw_start == "8") {
      updateAwesomeRadio(session, inputId = "Draw_point_layers", selected = "8")
    } else if (input$free_draw_start == "9") {
      updateAwesomeRadio(session, inputId = "Draw_point_layers", selected = "9")
    } else if (input$free_draw_start == "0") {
      updateAwesomeRadio(session, inputId = "Draw_point_layers", selected = "10")
    } else if (input$free_draw_start == "!") {
      updateAwesomeRadio(session, inputId = "Draw_polygon_layers", selected = "1")
    } else if (input$free_draw_start == "@") {
      updateAwesomeRadio(session, inputId = "Draw_polygon_layers", selected = "2")
    } else if (input$free_draw_start == "#") {
      updateAwesomeRadio(session, inputId = "Draw_polygon_layers", selected = "3")
    } else if (input$free_draw_start == "$") {
      updateAwesomeRadio(session, inputId = "Draw_polygon_layers", selected = "4")
    } else if (input$free_draw_start == "%") {
      updateAwesomeRadio(session, inputId = "Draw_polygon_layers", selected = "5")
    } else if (input$free_draw_start == "^") {
      updateAwesomeRadio(session, inputId = "Draw_polygon_layers", selected = "6")
    } else if (input$free_draw_start == "&") {
      updateAwesomeRadio(session, inputId = "Draw_polygon_layers", selected = "7")
    } else if (input$free_draw_start == "*") {
      updateAwesomeRadio(session, inputId = "Draw_polygon_layers", selected = "8")
    } else if (input$free_draw_start == "(") {
      updateAwesomeRadio(session, inputId = "Draw_polygon_layers", selected = "9")
    } else if (input$free_draw_start == ")") {
      updateAwesomeRadio(session, inputId = "Draw_polygon_layers", selected = "10")
    } else if (input$free_draw_start == "q") {
      updateRadioGroupButtons(session, inputId = "Start_to_draw_point", selected = "None")
    } else if (input$free_draw_start == "w") {
      updateRadioGroupButtons(session, inputId = "Start_to_draw_point", selected = "Add points")
    } else if (input$free_draw_start == "e") {
      updateRadioGroupButtons(session, inputId = "Start_to_draw_point", selected = "Remove points")
    }
  }
})



observeEvent(input$polygons_error, {

if(input$polygons_error == FALSE){

choices <- list.files(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/")) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE)
new_poly <- which(choices == input$Image_input )

if(length(polygon_file_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/temp/previous.rds"))) > 0 ){
	 if(new_poly != 1){
		
		Polygons_data_current_on_map$list <- polygon_file_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/temp/previous.rds"))
		source("program_script/Server_side_leaflet_slide_image_viewer.R", local = TRUE)
	 }
     if(new_poly == 1){	
	 
	   Polygons_data_current_on_map$list <- polygon_file_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/temp/previous.rds"))
	   source("program_script/Server_side_leaflet_slide_image_viewer.R", local = TRUE)
			   
	 }
   }
if(length(polygon_file_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/temp/previous.rds"))) == 0 ){

     Polygons_data_current_on_map$list <- polygon_file_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/polygons.rds"))
	 source("program_script/Server_side_leaflet_slide_image_viewer.R", local = TRUE)


}   
  
  }
})




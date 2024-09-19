slide_training_file_generator_function <- function(loop_jpg_file,
                                                   use_point_df,
                                                   use_polygon_df,
                                                   enhancement_point_df,
                                                   enhancement_polygon_df,
                                                   only_train_point_df,
                                                   only_train_polygon_df,
                                                   Basal_train_weight_fraction,
                                                   tile_size,
                                                   file_location_name,
                                                   file_location_jpg) {
  
  select_region_train_list <- list()
  
  slide_train_df <- data.frame(
    slide_weight = NA,
    region_weight = NA,
    lon = NA,
    lat = NA,
    slide_name = NA,
    stringsAsFactors = FALSE
  )
  
  slide_train_df <- slide_train_df[-1,]
  
  file_info <- NDPI_info(loop_jpg_file)
  
  back_groun_x <- (file_info$Width_pixel/tile_size) %>% ceiling()
  back_groun_y <- (file_info$Height_pixel/tile_size) %>% ceiling()
  backgroup_point <- (which(array(1, c(back_groun_x, back_groun_y)) == 1, arr.ind = TRUE) - 1) * tile_size + tile_size/2
  backgroup_point <- XY2LonLat(x = backgroup_point[,1], y = backgroup_point[,2], zoom = 12 + 1)
  
  background_df <- data.frame(
    slide_weight = NA,
    region_weight = Basal_train_weight_fraction,
    lon = backgroup_point$lon,
    lat = backgroup_point$lat,
    slide_name = NA,
    meaning = "background",
    stringsAsFactors = FALSE
  )
  
  backgroup_point <- backgroup_point %>% as.matrix()
  background_geojson <- lapply(1:nrow(backgroup_point), function(x) {
    lawn_point(backgroup_point[x,])
  }) %>% lawn_featurecollection()
  
  if(file.exists(paste0(loop_jpg_file %>% dirname(), "/polygons.rds"))==FALSE){
	  polygon_annotated_data<- list()
  }
  if(file.exists(paste0(loop_jpg_file %>% dirname(), "/polygons.rds"))==TRUE){
      polygon_annotated_data <- readRDS(paste0(loop_jpg_file %>% dirname(), "/polygons.rds"))
  }
  if(file.exists(paste0(loop_jpg_file %>% dirname(), "/points.rds"))==FALSE){  
	  
	  point_annotated_data <- data.frame(list(
          point_group = character(0),
          point_id = character(0),
          point_lon = character(0),
          point_lat = character(0),
          point_primary = character(0),
          point_secondary = character(0),
          point_tertiary = character(0))
        )
  }
  if(file.exists(paste0(loop_jpg_file %>% dirname(), "/points.rds"))==TRUE){
      point_annotated_data <- readRDS(paste0(loop_jpg_file %>% dirname(), "/points.rds"))
  }  
#  point_annotated_data <- readRDS(paste0(loop_jpg_file %>% dirname(), "/points.rds"))
#  polygon_annotated_data <- readRDS(paste0(loop_jpg_file %>% dirname(), "/polygons.rds"))
  polygon_annotated_data <- polygon_annotated_data[str_detect(names(polygon_annotated_data), "_G_")]
  polygon_annotated_data <- polygon_annotated_data[sapply(polygon_annotated_data, length) > 0]
  
  if (nrow(use_point_df) > 0) {
    for (k in 1:nrow(use_point_df)) {
      point_data_each_layer <- point_annotated_data %>% 
        filter(point_primary == use_point_df$Primary_point_class[k]) %>% 
        filter(point_secondary == use_point_df$Secondary_point_class[k]) %>% 
        filter(point_tertiary == use_point_df$Tertiary_point_class[k]) %>% 
        filter(point_group == paste0("P_", use_point_df$Point_number[k]))
      
      point_meaning <- paste0(use_point_df$Primary_point_class[k], 
                              "_",
                              use_point_df$Secondary_point_class[k], 
                              "_",
                              use_point_df$Tertiary_point_class[k],
                              "_",
                              paste0("P_", use_point_df$Point_number[k]))
      
      if (nrow(point_data_each_layer) != 0) {
        point_df <- data.frame(
          slide_weight = NA,
          region_weight = use_point_df$Select_to_train_weight_fraction[k],
          lon = point_data_each_layer$point_lon,
          lat = point_data_each_layer$point_lat,
          slide_name = NA,
          meaning = paste0(point_meaning, "___train"),
          stringsAsFactors = FALSE
        )
        
        background_df <- rbind(background_df, point_df)
      } 
    }
  }
  
  if (nrow(enhancement_point_df) > 0) {
    for (k in 1:nrow(enhancement_point_df)) {
      point_data_each_layer <- point_annotated_data %>% 
        filter(point_primary == enhancement_point_df$Primary_point_class[k]) %>% 
        filter(point_secondary == enhancement_point_df$Secondary_point_class[k]) %>% 
        filter(point_tertiary == enhancement_point_df$Tertiary_point_class[k]) %>% 
        filter(point_group == paste0("P_", enhancement_point_df$Point_number[k]))
      
      point_meaning <- paste0(enhancement_point_df$Primary_point_class[k], 
                              "_",
                              enhancement_point_df$Secondary_point_class[k], 
                              "_",
                              enhancement_point_df$Tertiary_point_class[k],
                              "_",
                              paste0("P_", enhancement_point_df$Point_number[k]))
      
      if (nrow(point_data_each_layer) != 0) {
        point_df <- data.frame(
          slide_weight = NA,
          region_weight = enhancement_point_df$Select_to_train_weight_fraction[k],
          lon = point_data_each_layer$point_lon,
          lat = point_data_each_layer$point_lat,
          slide_name = NA,
          meaning = paste0(point_meaning, "___enhancement"),
          stringsAsFactors = FALSE
        )
        
        background_df <- rbind(background_df, point_df)
      } 
    }
  }
  
  if (nrow(only_train_point_df) > 0) {
    for (k in 1:nrow(only_train_point_df)) {
      point_data_each_layer <- point_annotated_data %>% 
        filter(point_primary == only_train_point_df$Primary_point_class[k]) %>% 
        filter(point_secondary == only_train_point_df$Secondary_point_class[k]) %>% 
        filter(point_tertiary == only_train_point_df$Tertiary_point_class[k]) %>% 
        filter(point_group == paste0("P_", only_train_point_df$Point_number[k]))
      
      point_meaning <- paste0(only_train_point_df$Primary_point_class[k], 
                              "_",
                              only_train_point_df$Secondary_point_class[k], 
                              "_",
                              only_train_point_df$Tertiary_point_class[k],
                              "_",
                              paste0("P_", only_train_point_df$Point_number[k]))
      
      if (nrow(point_data_each_layer) != 0) {
        point_df <- data.frame(
          slide_weight = NA,
          region_weight = only_train_point_df$Select_to_train_weight_fraction[k],
          lon = point_data_each_layer$point_lon,
          lat = point_data_each_layer$point_lat,
          slide_name = NA,
          meaning = paste0(point_meaning, "___only"),
          stringsAsFactors = FALSE
        )
        
        background_df <- rbind(background_df, point_df)
      } 
    }
  }
  
  if (nrow(use_polygon_df) > 0) {
    for (k in 1:nrow(use_polygon_df)) {
      polygon_data_each_layer <- paste0(use_polygon_df$Primary_polygon_class[k],
                                        "_",
                                        use_polygon_df$Secondary_polygon_class[k],
                                        "_",
                                        use_polygon_df$Tertiary_polygon_class[k],
                                        "_",
                                        paste0("G_", use_polygon_df$Polygon_number[k]))
      polygons <- polygon_annotated_data[[polygon_data_each_layer]]
      
      if (is.null(polygons) == FALSE) {
        if (length(polygons) > 0) {
          merge_polygon <- polygons %>% unname() %>% lawn_featurecollection()
          point_within <- lawn_within(background_geojson, merge_polygon)
          
          point_data_each_layer <- lawn_coordall(point_within)
          
          polygon_meaning <- paste0(polygon_data_each_layer,
                                    "_G")
									
          
          if (nrow(point_data_each_layer) != 0) {
            polygon_df <- data.frame(
              slide_weight = NA,
              region_weight = use_polygon_df$Select_to_train_weight_fraction[k],
              lon = point_data_each_layer[,1],
              lat = point_data_each_layer[,2],
              slide_name = NA,
              meaning = paste0(polygon_meaning, "___train"),
              stringsAsFactors = FALSE
            )
            
            background_df <- rbind(background_df, polygon_df)
          } 
          
          if (use_polygon_df$Boundary_train_fraction[k] > 0) {
            if (length(polygons) > 0) {
              all_list_cordall <- list()
              for (ip in 1:length(polygons)) {
                if (polygons[[ip]]$geometry$coordinates %>% class() == "list") {
                  for (kp in 1:length(polygons[[ip]]$geometry$coordinates)) {
                    all_list_cordall[[length(all_list_cordall) + 1]] <- polygons[[ip]]$geometry$coordinates[[kp]]
                  }
                } else if (polygons[[ip]]$geometry$coordinates %>% class() == "array") {
                  for (kp in 1:(dim(polygons[[ip]]$geometry$coordinates)[1])) {
                    all_list_cordall[[length(all_list_cordall) + 1]] <- polygons[[ip]]$geometry$coordinates[kp,,]
                  }
                }
              }
              point_data_each_layer <- lapply(1:length(all_list_cordall), function(xx) {
                coor_location <- all_list_cordall[[xx]]
                coor_location <- LonLat2XY(lon_deg = coor_location[,1], lat_deg = coor_location[,2], zoom = 13)
                all_line <- lapply(1:(nrow(coor_location) - 1), function(x) {
                  point_line_function(base::c(coor_location$x[x], coor_location$y[x]), base::c(coor_location$x[x+1], coor_location$y[x+1])) %>% as.matrix()
                }) %>% abind::abind(along = 1)
                if (nrow(all_line) < (tile_size/2)) {
                  all_line[1,]
                } else {
                  all_line[(1:(nrow(all_line) %/% (tile_size/2))) * (tile_size/2),]
                }
              }) %>% abind::abind(along = 1)
              point_data_each_layer <- XY2LonLat(x = point_data_each_layer[,1], 
                                                 y = point_data_each_layer[,2],
                                                 zoom = 12 + 1) %>% distinct(.keep_all = TRUE) 
              
              if (nrow(point_data_each_layer) != 0) {
                boundary_df <- data.frame(
                  slide_weight = NA,
                  region_weight = use_polygon_df$Boundary_train_fraction[k],
                  lon = point_data_each_layer$lon,
                  lat = point_data_each_layer$lat,
                  slide_name = NA,
                  meaning = paste0(polygon_meaning, "___train_L"),
                  stringsAsFactors = FALSE
                )
                
                background_df <- rbind(background_df, boundary_df)
              } 
            }
          }
        }
      }
    }
  }
  
  
  
  
  if (nrow(enhancement_polygon_df) > 0) {
    for (k in 1:nrow(enhancement_polygon_df)) {
      polygon_data_each_layer <- paste0(enhancement_polygon_df$Primary_polygon_class[k],
                                        "_",
                                        enhancement_polygon_df$Secondary_polygon_class[k],
                                        "_",
                                        enhancement_polygon_df$Tertiary_polygon_class[k],
                                        "_",
                                        paste0("G_", enhancement_polygon_df$Polygon_number[k]))
      polygons <- polygon_annotated_data[[polygon_data_each_layer]]
      
      if (is.null(polygons) == FALSE) {
        if (length(polygons) > 0) {
          merge_polygon <- polygons %>% unname() %>% lawn_featurecollection()
          point_within <- lawn_within(background_geojson, merge_polygon)
          
          point_data_each_layer <- lawn_coordall(point_within)
          
          polygon_meaning <- paste0(polygon_data_each_layer,
                                    "_G")
									
          
          if (nrow(point_data_each_layer) != 0) {
            polygon_df <- data.frame(
              slide_weight = NA,
              region_weight = enhancement_polygon_df$Select_to_train_weight_fraction[k],
              lon = point_data_each_layer[,1],
              lat = point_data_each_layer[,2],
              slide_name = NA,
              meaning = paste0(polygon_meaning, "___enhancement"),
              stringsAsFactors = FALSE
            )
            
            background_df <- rbind(background_df, polygon_df)
          } 
          
          if (enhancement_polygon_df$Boundary_train_fraction[k] > 0) {
            if (length(polygons) > 0) {
              all_list_cordall <- list()
              for (ip in 1:length(polygons)) {
                if (polygons[[ip]]$geometry$coordinates %>% class() == "list") {
                  for (kp in 1:length(polygons[[ip]]$geometry$coordinates)) {
                    all_list_cordall[[length(all_list_cordall) + 1]] <- polygons[[ip]]$geometry$coordinates[[kp]]
                  }
                } else if (polygons[[ip]]$geometry$coordinates %>% class() == "array") {
                  for (kp in 1:(dim(polygons[[ip]]$geometry$coordinates)[1])) {
                    all_list_cordall[[length(all_list_cordall) + 1]] <- polygons[[ip]]$geometry$coordinates[kp,,]
                  }
                }
              }
              point_data_each_layer <- lapply(1:length(all_list_cordall), function(xx) {
                coor_location <- all_list_cordall[[xx]]
                coor_location <- LonLat2XY(lon_deg = coor_location[,1], lat_deg = coor_location[,2], zoom = 13)
                all_line <- lapply(1:(nrow(coor_location) - 1), function(x) {
                  point_line_function(base::c(coor_location$x[x], coor_location$y[x]), base::c(coor_location$x[x+1], coor_location$y[x+1])) %>% as.matrix()
                }) %>% abind::abind(along = 1)
                if (nrow(all_line) < (tile_size/2)) {
                  all_line[1,]
                } else {
                  all_line[(1:(nrow(all_line) %/% (tile_size/2))) * (tile_size/2),]
                }
              }) %>% abind::abind(along = 1)
              point_data_each_layer <- XY2LonLat(x = point_data_each_layer[,1], 
                                                 y = point_data_each_layer[,2],
                                                 zoom = 12 + 1) %>% distinct(.keep_all = TRUE) 
              
              if (nrow(point_data_each_layer) != 0) {
                boundary_df <- data.frame(
                  slide_weight = NA,
                  region_weight = enhancement_polygon_df$Boundary_train_fraction[k],
                  lon = point_data_each_layer$lon,
                  lat = point_data_each_layer$lat,
                  slide_name = NA,
                  meaning = paste0(polygon_meaning, "___enhancement_L"),
                  stringsAsFactors = FALSE
                )
                
                background_df <- rbind(background_df, boundary_df)
              } 
            }
          }
        }
      }
    }
  }
  
  
  if (nrow(only_train_polygon_df) > 0) {
    for (k in 1:nrow(only_train_polygon_df)) {
	
	  
      polygon_data_each_layer <- paste0(only_train_polygon_df$Primary_polygon_class[k],
                                        "_",
                                        only_train_polygon_df$Secondary_polygon_class[k],
                                        "_",
                                        only_train_polygon_df$Tertiary_polygon_class[k],
                                        "_",
                                        paste0("G_", only_train_polygon_df$Polygon_number[k]))
      polygons <- polygon_annotated_data[[polygon_data_each_layer]]
      
      if (is.null(polygons) == FALSE) {
        if (length(polygons) > 0) {
          merge_polygon <- polygons %>% unname() %>% lawn_featurecollection()
          point_within <- lawn_within(background_geojson, merge_polygon)
          
          point_data_each_layer <- lawn_coordall(point_within)
          
          polygon_meaning <- paste0(polygon_data_each_layer,
                                    "_G")
          
          if (nrow(point_data_each_layer) != 0) {
            polygon_df <- data.frame(
              slide_weight = NA,
              region_weight = only_train_polygon_df$Select_to_train_weight_fraction[k],
              lon = point_data_each_layer[,1],
              lat = point_data_each_layer[,2],
              slide_name = NA,
              meaning = paste0(polygon_meaning, "___only"),
              stringsAsFactors = FALSE
            )
            
            background_df <- rbind(background_df, polygon_df)
          } 
          
          if (only_train_polygon_df$Boundary_train_fraction[k] > 0) {
            if (length(polygons) > 0) {
              all_list_cordall <- list()
              for (ip in 1:length(polygons)) {
                if (polygons[[ip]]$geometry$coordinates %>% class() == "list") {
                  for (kp in 1:length(polygons[[ip]]$geometry$coordinates)) {
                    all_list_cordall[[length(all_list_cordall) + 1]] <- polygons[[ip]]$geometry$coordinates[[kp]]
                  }
                } else if (polygons[[ip]]$geometry$coordinates %>% class() == "array") {
                  for (kp in 1:(dim(polygons[[ip]]$geometry$coordinates)[1])) {
                    all_list_cordall[[length(all_list_cordall) + 1]] <- polygons[[ip]]$geometry$coordinates[kp,,]
                  }
                }
              }
              point_data_each_layer <- lapply(1:length(all_list_cordall), function(xx) {
                coor_location <- all_list_cordall[[xx]]
                coor_location <- LonLat2XY(lon_deg = coor_location[,1], lat_deg = coor_location[,2], zoom = 13)
                all_line <- lapply(1:(nrow(coor_location) - 1), function(x) {
                  point_line_function(base::c(coor_location$x[x], coor_location$y[x]), base::c(coor_location$x[x+1], coor_location$y[x+1])) %>% as.matrix()
                }) %>% abind::abind(along = 1)
                if (nrow(all_line) < (tile_size/2)) {
                  all_line[1,]
                } else {
                  all_line[(1:(nrow(all_line) %/% (tile_size/2))) * (tile_size/2),]
                }
              }) %>% abind::abind(along = 1)
              point_data_each_layer <- XY2LonLat(x = point_data_each_layer[,1], 
                                                 y = point_data_each_layer[,2],
                                                 zoom = 12 + 1) %>% distinct(.keep_all = TRUE) 
              
              if (nrow(point_data_each_layer) != 0) {
                boundary_df <- data.frame(
                  slide_weight = NA,
                  region_weight = only_train_polygon_df$Boundary_train_fraction[k],
                  lon = point_data_each_layer$lon,
                  lat = point_data_each_layer$lat,
                  slide_name = NA,
                  meaning = paste0(polygon_meaning, "___only_L"),
                  stringsAsFactors = FALSE
                )
                
                background_df <- rbind(background_df, boundary_df)
              } 
            }
          }
        }
      }
    }
  }
  
  background_df$slide_name <- paste0(getwd(), "/", loop_jpg_file)
  background_df$annotation_name <- paste0(file_location_name, "/", file_location_jpg %>% str_replace_all(".ndpi", ""), ".rds")

  tryCatch({
        saveRDS(list(file_info, 
               background_df,
               point_annotated_data,
               polygon_annotated_data,
               use_point_df,
               use_polygon_df,
               enhancement_point_df,
               enhancement_polygon_df,
               only_train_point_df,
               only_train_polygon_df,
               lapply(polygon_annotated_data, function(x) {geojson_merge_function(x, method = "exact")})), 
          paste0(file_location_name, "/", file_location_jpg %>% str_replace_all(".ndpi", ""), ".rds"))
}, error = function(e) {
          saveRDS(list(file_info, 
               background_df,
               point_annotated_data,
               polygon_annotated_data,
               use_point_df,
               use_polygon_df,
               enhancement_point_df,
               enhancement_polygon_df,
               only_train_point_df,
               only_train_polygon_df,
               lapply(polygon_annotated_data, function(x) {geojson_merge_function_fix(x, method = "fix")})), 
          paste0(file_location_name, "/", file_location_jpg %>% str_replace_all(".ndpi", ""), ".rds"))
})


}


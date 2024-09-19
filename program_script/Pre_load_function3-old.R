lon_lat_point_all_location <- function(tile_x,
                                       tile_y,
                                       zoom,
                                       tile_size) {
  check_point <- which(array(1, c(tile_size, tile_size)) == 1, arr.ind = TRUE)
  deviation_x <- tile_x * 2^(12 - zoom)
  deviation_y <- tile_y * 2^(12 - zoom)
  
  check_point[,1] <- check_point[,1] + deviation_x
  check_point[,2] <- check_point[,2] + deviation_y
  
  lonlat_location_all <- XY2LonLat(x = check_point[,1], y = check_point[,2], zoom = zoom + 1)
  return(lonlat_location_all)
}

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
  select_region_train_df <- data.frame(
    weight = NA,
    number = NA,
    meaning = NA, 
    stringsAsFactors = FALSE
  )
  
  
  file_info <- NDPI_info(loop_jpg_file)
  
  back_groun_x <- (file_info$Width_pixel/tile_size) %>% ceiling()
  back_groun_y <- (file_info$Height_pixel/tile_size) %>% ceiling()
  backgroup_point <- (which(array(1, c(back_groun_x, back_groun_y)) == 1, arr.ind = TRUE) - 1) * tile_size + tile_size/2
  backgroup_point <- XY2LonLat(x = backgroup_point[,1], y = backgroup_point[,2], zoom = 12 + 1) %>% as.matrix()
  background_geojson <- lapply(1:nrow(backgroup_point), function(x) {
    lawn_point(backgroup_point[x,])
  }) %>% lawn_featurecollection()
  
  select_region_train_list$backgroup <- list(weight = Basal_train_weight_fraction,
                                             region = backgroup_point)
  select_region_train_df$weight[1] <- Basal_train_weight_fraction
  select_region_train_df$number[1] <- nrow(backgroup_point)
  select_region_train_df$meaning[1] <- "backgroup"
  
  point_annotated_data <- readRDS(paste0(loop_jpg_file %>% dirname(), "/points.rds"))
  polygon_annotated_data <- readRDS(paste0(loop_jpg_file %>% dirname(), "/polygons.rds"))
  
  if (nrow(use_point_df) > 0) {
    for (k in 1:nrow(use_point_df)) {
      point_data_each_layer <- point_annotated_data %>% 
        filter(point_primary == use_point_df$Primary_point_class[k]) %>% 
        filter(point_secondary == use_point_df$Secondary_point_class[k]) %>% 
        filter(point_tertiary == use_point_df$Tertiary_point_class[k]) %>% 
        filter(point_group == paste0("P_", use_point_df$Point_number[k]))
      
      point_data_each_layer <- cbind(point_data_each_layer$point_lon, 
                                     point_data_each_layer$point_lat) %>% as.matrix()
      
      point_meaning <- paste0(use_point_df$Primary_point_class[k], 
                              "_",
                              use_point_df$Secondary_point_class[k], 
                              "_",
                              use_point_df$Tertiary_point_class[k],
                              "_",
                              paste0("P_", use_point_df$Point_number[k]))
      
      select_region_train_list[[paste0(point_meaning, "___train")]] <- list(weight = use_point_df$Select_to_train_weight_fraction[k],
                                                                            region = point_data_each_layer)
      
      df_number <- nrow(select_region_train_df)
      
      
      if (nrow(point_data_each_layer) == 0) {
        select_region_train_df[df_number + 1, "weight"] <- 0
      } else {
        select_region_train_df[df_number + 1, "weight"] <- use_point_df$Select_to_train_weight_fraction[k]
      }
      select_region_train_df[df_number + 1, "number"] <- nrow(point_data_each_layer)
      select_region_train_df[df_number + 1, "meaning"] <- paste0(point_meaning, "___train")
    }
  }
  
  if (nrow(enhancement_point_df) > 0) {
    for (k in 1:nrow(enhancement_point_df)) {
      point_data_each_layer <- point_annotated_data %>% 
        filter(point_primary == enhancement_point_df$Primary_point_class[k]) %>% 
        filter(point_secondary == enhancement_point_df$Secondary_point_class[k]) %>% 
        filter(point_tertiary == enhancement_point_df$Tertiary_point_class[k]) %>% 
        filter(point_group == paste0("P_", enhancement_point_df$Point_number[k]))
      
      point_data_each_layer <- cbind(point_data_each_layer$point_lon, 
                                     point_data_each_layer$point_lat) %>% as.matrix()
      
      point_meaning <- paste0(enhancement_point_df$Primary_point_class[k], 
                              "_",
                              enhancement_point_df$Secondary_point_class[k], 
                              "_",
                              enhancement_point_df$Tertiary_point_class[k],
                              "_",
                              paste0("P_", enhancement_point_df$Point_number[k]))
      
      select_region_train_list[[paste0(point_meaning, "___enhancement")]] <- list(weight = enhancement_point_df$Select_to_train_weight_fraction[k],
                                                                                  region = point_data_each_layer)
      
      df_number <- nrow(select_region_train_df)
      
      
      if (nrow(point_data_each_layer) == 0) {
        select_region_train_df[df_number + 1, "weight"] <- 0
      } else {
        select_region_train_df[df_number + 1, "weight"] <- enhancement_point_df$Select_to_train_weight_fraction[k]
      }
      select_region_train_df[df_number + 1, "number"] <- nrow(point_data_each_layer)
      select_region_train_df[df_number + 1, "meaning"] <- paste0(point_meaning, "___enhancement")
    }
  }
  
  if (nrow(only_train_point_df) > 0) {
    for (k in 1:nrow(only_train_point_df)) {
      point_data_each_layer <- point_annotated_data %>% 
        filter(point_primary == only_train_point_df$Primary_point_class[k]) %>% 
        filter(point_secondary == only_train_point_df$Secondary_point_class[k]) %>% 
        filter(point_tertiary == only_train_point_df$Tertiary_point_class[k]) %>% 
        filter(point_group == paste0("P_", only_train_point_df$Point_number[k]))
      
      point_data_each_layer <- cbind(point_data_each_layer$point_lon, 
                                     point_data_each_layer$point_lat) %>% as.matrix()
      
      point_meaning <- paste0(only_train_point_df$Primary_point_class[k], 
                              "_",
                              only_train_point_df$Secondary_point_class[k], 
                              "_",
                              only_train_point_df$Tertiary_point_class[k],
                              "_",
                              paste0("P_", only_train_point_df$Point_number[k]))
      
      select_region_train_list[[paste0(point_meaning, "___only")]] <- list(weight = only_train_point_df$Select_to_train_weight_fraction[k],
                                                                           region = point_data_each_layer)
      
      df_number <- nrow(select_region_train_df)
      
      
      if (nrow(point_data_each_layer) == 0) {
        select_region_train_df[df_number + 1, "weight"] <- 0
      } else {
        select_region_train_df[df_number + 1, "weight"] <- only_train_point_df$Select_to_train_weight_fraction[k]
      }
      select_region_train_df[df_number + 1, "number"] <- nrow(point_data_each_layer)
      select_region_train_df[df_number + 1, "meaning"] <- paste0(point_meaning, "___only")
    }
  }
  
  if (nrow(use_polygon_df) > 0) {
    for (k in 1:nrow(use_polygon_df)) {
      polygon_data_each_layer <- paste0(use_polygon_df$Primary_polygon_class[k],
                                        "_",
                                        use_polygon_df$Secondary_polygon_class[k],
                                        "_",
                                        use_polygon_df$Tertiary_polygon_class[k])
      polygons <- polygon_annotated_data[[polygon_data_each_layer]]
      
      merge_polygon <- polygons %>% unname() %>% lawn_featurecollection()
      point_within <- lawn_within(background_geojson, merge_polygon)
      
      point_data_each_layer <- lawn_coordall(point_within)
      
      polygon_meaning <- paste0(polygon_data_each_layer,
                                "_G")
      
      select_region_train_list[[paste0(polygon_meaning, "___train")]] <- list(weight = use_polygon_df$Select_to_train_weight_fraction[k],
                                                                              region = point_data_each_layer)
      
      df_number <- nrow(select_region_train_df)
      
      if (nrow(point_data_each_layer) == 0) {
        select_region_train_df[df_number + 1, "weight"] <- 0
      } else {
        select_region_train_df[df_number + 1, "weight"] <- use_polygon_df$Select_to_train_weight_fraction[k]
      }
      select_region_train_df[df_number + 1, "number"] <- nrow(point_data_each_layer)
      select_region_train_df[df_number + 1, "meaning"] <- paste0(polygon_meaning, "___train")
      
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
            all_line[(1:(nrow(all_line) %/% (tile_size/2))) * (tile_size/2),]
          }) %>% abind::abind(along = 1)
          point_data_each_layer <- XY2LonLat(x = point_data_each_layer[,1], 
                                             y = point_data_each_layer[,2],
                                             zoom = 12 + 1) %>% distinct(.keep_all = TRUE) %>% as.matrix()
          
          select_region_train_list[[paste0(polygon_meaning, "___train_L")]] <- list(weight = use_polygon_df$Boundary_train_fraction[k],
                                                                                    region = point_data_each_layer)
          
          df_number <- nrow(select_region_train_df)
          
          if (nrow(point_data_each_layer) == 0) {
            select_region_train_df[df_number + 1, "weight"] <- 0
          } else {
            select_region_train_df[df_number + 1, "weight"] <- use_polygon_df$Boundary_train_fraction[k]
          }
          select_region_train_df[df_number + 1, "number"] <- nrow(point_data_each_layer)
          select_region_train_df[df_number + 1, "meaning"] <- paste0(polygon_meaning, "___train_L")
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
                                        enhancement_polygon_df$Tertiary_polygon_class[k])
      polygons <- polygon_annotated_data[[polygon_data_each_layer]]
      
      merge_polygon <- polygons %>% unname() %>% lawn_featurecollection()
      point_within <- lawn_within(background_geojson, merge_polygon)
      
      point_data_each_layer <- lawn_coordall(point_within)
      
      polygon_meaning <- paste0(polygon_data_each_layer,
                                "_G")
      
      select_region_train_list[[paste0(polygon_meaning, "___enhancement")]] <- list(weight = enhancement_polygon_df$Select_to_train_weight_fraction[k],
                                                                                    region = point_data_each_layer)
      
      df_number <- nrow(select_region_train_df)
      
      if (nrow(point_data_each_layer) == 0) {
        select_region_train_df[df_number + 1, "weight"] <- 0
      } else {
        select_region_train_df[df_number + 1, "weight"] <- enhancement_polygon_df$Select_to_train_weight_fraction[k]
      }
      select_region_train_df[df_number + 1, "number"] <- nrow(point_data_each_layer)
      select_region_train_df[df_number + 1, "meaning"] <- paste0(polygon_meaning, "___enhancement")
      
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
            all_line[(1:(nrow(all_line) %/% (tile_size/2))) * (tile_size/2),]
          }) %>% abind::abind(along = 1)
          point_data_each_layer <- XY2LonLat(x = point_data_each_layer[,1], 
                                             y = point_data_each_layer[,2],
                                             zoom = 12 + 1) %>% distinct(.keep_all = TRUE) %>% as.matrix()
          
          df_number <- nrow(select_region_train_df)
          
          if (nrow(point_data_each_layer) == 0) {
            select_region_train_df[df_number + 1, "weight"] <- 0
          } else {
            select_region_train_df[df_number + 1, "weight"] <- enhancement_polygon_df$Boundary_train_fraction[k]
          }
          select_region_train_df[df_number + 1, "number"] <- nrow(point_data_each_layer)
          select_region_train_df[df_number + 1, "meaning"] <- paste0(polygon_meaning, "___enhancement_L")
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
                                        only_train_polygon_df$Tertiary_polygon_class[k])
      polygons <- polygon_annotated_data[[polygon_data_each_layer]]
      
      merge_polygon <- polygons %>% unname() %>% lawn_featurecollection()
      point_within <- lawn_within(background_geojson, merge_polygon)
      
      point_data_each_layer <- lawn_coordall(point_within)
      
      polygon_meaning <- paste0(polygon_data_each_layer,
                                "_G")
      
      select_region_train_list[[paste0(polygon_meaning, "___only")]] <- list(weight = only_train_polygon_df$Select_to_train_weight_fraction[k],
                                                                             region = point_data_each_layer)
      
      df_number <- nrow(select_region_train_df)
      
      if (nrow(point_data_each_layer) == 0) {
        select_region_train_df[df_number + 1, "weight"] <- 0
      } else {
        select_region_train_df[df_number + 1, "weight"] <- only_train_polygon_df$Select_to_train_weight_fraction[k]
      }
      select_region_train_df[df_number + 1, "number"] <- nrow(point_data_each_layer)
      select_region_train_df[df_number + 1, "meaning"] <- paste0(polygon_meaning, "___only")
      
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
            all_line[(1:(nrow(all_line) %/% (tile_size/2))) * (tile_size/2),]
          }) %>% abind::abind(along = 1)
          point_data_each_layer <- XY2LonLat(x = point_data_each_layer[,1], 
                                             y = point_data_each_layer[,2],
                                             zoom = 12 + 1) %>% distinct(.keep_all = TRUE) %>% as.matrix()
          
          select_region_train_list[[paste0(polygon_meaning, "___only_L")]] <- list(weight = only_train_polygon_df$Boundary_train_fraction[k],
                                                                                   region = point_data_each_layer)
          
          df_number <- nrow(select_region_train_df)
          
          if (nrow(point_data_each_layer) == 0) {
            select_region_train_df[df_number + 1, "weight"] <- 0
          } else {
            select_region_train_df[df_number + 1, "weight"] <- only_train_polygon_df$Boundary_train_fraction[k]
          }
          select_region_train_df[df_number + 1, "number"] <- nrow(point_data_each_layer)
          select_region_train_df[df_number + 1, "meaning"] <- paste0(polygon_meaning, "___only_L")
        }
      }
    }
  }
  
  saveRDS(list(file_info, 
               select_region_train_list, 
               select_region_train_df, 
               point_annotated_data,
               polygon_annotated_data,
               use_point_df,
               use_polygon_df,
               lapply(polygon_annotated_data, function(x) {geojson_merge_function(x, method = "exact")})), 
          paste0(file_location_name, "/", file_location_jpg %>% str_replace_all(".ndpi", ""), ".rds"))
  
}

center_check_for_point_or_chunk <- function(center_x_all, center_y_all, zoom_level, tile_size, point_vs_chunk) {
  if (point_vs_chunk == "point") {
    all_point_range <- (1:tile_size - tile_size/2) * 2^(12 - zoom_level)
    all_check_point <- lapply(1:length(center_x_all), function(check_point_each) {
      point_location <- XY2LonLat(x = center_x_all[check_point_each] + rep(all_point_range, tile_size),
                                  y = center_y_all[check_point_each] + sapply(all_point_range, function(y) {rep(y, tile_size)}) %>% as.numeric(),
                                  zoom = 12 + 1) %>% as.matrix()
      lapply(1:nrow(point_location), function(x) {
        lawn_point(point_location[x,])
      }) %>% lawn_featurecollection()
    })
  } else if (point_vs_chunk == "chunk") {
    all_point_range <- (base::c(tile_size, 1) - tile_size/2) * 2^(12 - zoom_level)
    all_check_polygon <- lapply(1:length(center_x_all), function(check_polygon_each) {
      poly_location <- XY2LonLat(x = base::c(center_x_all[check_polygon_each] + all_point_range[1],
                                             center_x_all[check_polygon_each] + all_point_range[2],
                                             center_x_all[check_polygon_each] + all_point_range[2],
                                             center_x_all[check_polygon_each] + all_point_range[1],
                                             center_x_all[check_polygon_each] + all_point_range[1]),
                                 y = base::c(center_y_all[check_polygon_each] + all_point_range[1],
                                             center_y_all[check_polygon_each] + all_point_range[1],
                                             center_y_all[check_polygon_each] + all_point_range[2],
                                             center_y_all[check_polygon_each] + all_point_range[2],
                                             center_y_all[check_polygon_each] + all_point_range[1]),
                                 zoom = 12 + 1)
      
      lawn_polygon(list(list(
        base::c(poly_location[1,1], poly_location[1,2]),
        base::c(poly_location[2,1], poly_location[2,2]),
        base::c(poly_location[3,1], poly_location[3,2]),
        base::c(poly_location[4,1], poly_location[4,2]),
        base::c(poly_location[5,1], poly_location[5,2])
      )))
    })
  }
}

geojson_lonlat2xy_function <- function(lonlat_geojson) {
  if (lonlat_geojson$geometry$type == "Polygon") {
    if (lonlat_geojson$geometry$coordinates %>% class() == "list") {
      for (p_list in 1:length(lonlat_geojson$geometry$coordinates)) {
        origin_location <- lonlat_geojson$geometry$coordinates[[p_list]]
        origin_location <- LonLat2XY(lon_deg = origin_location[,1],
                                     lat_deg = origin_location[,2],
                                     zoom = 12 + 1)
        lonlat_geojson$geometry$coordinates[[p_list]] <- origin_location %>% as.matrix()
      }
      return(lonlat_geojson)
    } else if (lonlat_geojson$geometry$coordinates %>% class() == "array") {
      for (p_list in 1:(dim(lonlat_geojson$geometry$coordinates)[1])) {
        origin_location <- lonlat_geojson$geometry$coordinates[p_list,,]
        origin_location <- LonLat2XY(lon_deg = origin_location[,1],
                                     lat_deg = origin_location[,2],
                                     zoom = 12 + 1)
        lonlat_geojson$geometry$coordinates[p_list,,] <- origin_location %>% as.matrix()
      }
      return(lonlat_geojson)
    }
  } else if (lonlat_geojson$geometry$type == "MultiPolygon") {
    for (m_list in 1:length(lonlat_geojson$geometry$coordinates)) {
      if (lonlat_geojson$geometry$coordinates[[m_list]] %>% class() == "list") {
        for (p_list in 1:length(lonlat_geojson$geometry$coordinates[[m_list]])) {
          origin_location <- lonlat_geojson$geometry$coordinates[[m_list]][[p_list]]
          origin_location <- LonLat2XY(lon_deg = origin_location[,1],
                                       lat_deg = origin_location[,2],
                                       zoom = 12 + 1)
          lonlat_geojson$geometry$coordinates[[m_list]][[p_list]] <- origin_location %>% as.matrix()
        }
        return(lonlat_geojson)
      } else if (lonlat_geojson$geometry$coordinates[[m_list]] %>% class() == "array") {
        for (p_list in 1:(dim(lonlat_geojson$geometry$coordinates[[m_list]])[1])) {
          origin_location <- lonlat_geojson$geometry$coordinates[[m_list]][p_list,,]
          origin_location <- LonLat2XY(lon_deg = origin_location[,1],
                                       lat_deg = origin_location[,2],
                                       zoom = 12 + 1)
          lonlat_geojson$geometry$coordinates[[m_list]][p_list,,] <- origin_location %>% as.matrix()
        }
        return(lonlat_geojson)
      }
    } 
  }
}

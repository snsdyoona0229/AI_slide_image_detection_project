color_name_list <- c("blue", "green", "yellow", "red", "black", "white", "purple", "cyan", "magenta", "orange")
color_name_list_points <- c("red","blue", "green", "yellow", "black", "white", "purple", "cyan", "magenta", "orange")
color_name_list_polygons <- c("yellow","blue", "green", "red", "black", "white", "purple", "cyan", "magenta", "orange")
color_name_value <- list(
  "blue" = c(0/256, 0/256, 256/256),
  "green" = c(0/256, 256/256, 0/256),
  "yellow" = c(256/256, 256/256, 0/256),
  "red" = c(256/256, 0/256, 0/256), 
  "black" = c(0/256, 0/256, 0/256),
  "white" = c(256/256, 256/256, 256/256),
  "purple" = c(160/256, 32/256, 240/256),
  "cyan" = c(0/256, 256/256, 256/256),
  "magenta" = c(256/256, 0/256, 256/256),
  "orange" = c(256/256, 165/256, 0/256)
)

point_file_function <- function(path_data){
  if (file.exists(path_data)) {
    points_information <- readRDS(path_data)
  } else {
    points_information <- data.frame(
      point_group = "",
      point_id = "",
      point_lon = "",
      point_lat = "",
      point_primary = "",
      point_secondary = "",
      point_tertiary = "",
      stringsAsFactors = FALSE
    )
    points_information <- points_information[-1,]
    path_dir <- dirname(path_data)
    if (file.exists(path_dir)) {
      saveRDS(points_information, path_data)
    }
  }
  return(points_information)
}

polygon_file_function <- function(path_data){
  if (file.exists(path_data)) {
    polygons_information <- readRDS(path_data)
    
    #===================================================== ver1 into ver2
    #print(names(polygons_information))
    annotation_name <- names(polygons_information)
    
    if (length(annotation_name) > 0) {
      for (i in 1:length(annotation_name)) {
        if (str_detect(annotation_name[i], "G") == FALSE) {
          if (str_detect(annotation_name, paste0(annotation_name[i], "_G_1")) %>% sum() == 0) {
            polygons_information[[paste0(annotation_name[i], "_G_1")]] <- polygons_information[[annotation_name[i]]]
            path_dir <- dirname(path_data)
            if (file.exists(path_dir)) {
              saveRDS(polygons_information, path_data)
            }
          }
        }
      }
    }
    
    #===================================================== ver1 into ver2
  } else {
    polygons_information <- list()
    path_dir <- dirname(path_data)
    if (file.exists(path_dir)) {
      saveRDS(polygons_information, path_data)
    }
  }
  return(polygons_information)
}

heat_file_function <- function(path_data){
  if (file.exists(path_data)) {
    heats_information <- readRDS(path_data)
  } else {
    heats_information <- list()
    path_dir <- dirname(path_data)
    if (file.exists(path_dir)) {
      saveRDS(heats_information, path_data)
    }
  }
  return(heats_information)
}

tag_file_function <- function(path_data){
  if (file.exists(path_data)) {
    tags_information <- readRDS(path_data)
  } else {
    tags_information <- list()
    path_dir <- dirname(path_data)
    if (file.exists(path_dir)) {
      saveRDS(tags_information, path_data)
    }
  }
  return(tags_information)
}

polygon_history_function <- function(path_data){
  if (file.exists(path_data)) {
    polygons_history <- readRDS(path_data)
  } else {
    polygons_history <- list()
    path_dir <- dirname(path_data)
    if (file.exists(path_dir)) {
      saveRDS(polygons_history, path_data)
    }
  }
  return(polygons_history)
}

point_note_function <- function(path_data){
  if (file.exists(path_data)) {
    points_note_information <- readRDS(path_data)
  } else {
    points_note_information <- data.frame(
      Primary_point_class = rep("",10),
      Secondary_point_class = rep("",10),
      Tertiary_point_class = rep("",10),
      Point_number = rep("",10),
      Meanings = rep("",10),
      #Enhancement = rep("F", 10),
      #Only_train_area = rep("F", 10),
      stringsAsFactors = FALSE
    )
    path_dir <- dirname(path_data)
    if (file.exists(path_dir)) {
      saveRDS(points_note_information, path_data)
    }
  }
  return(points_note_information)
}

polygon_note_function <- function(path_data){
  if (file.exists(path_data)) {
    polygons_note_information <- readRDS(path_data)
    
    
    #===================================================== ver1 into ver2
    if (colnames(polygons_note_information) %>% str_detect("Polygon_number") %>% sum() == 0) {
      polygons_note_information <- data.frame(
        Primary_polygon_class = polygons_note_information$Primary_polygon_class,
        Secondary_polygon_class = polygons_note_information$Secondary_polygon_class,
        Tertiary_polygon_class = polygons_note_information$Tertiary_polygon_class,
        Polygon_number = rep("",10),
        Meanings = polygons_note_information$Meanings,
        stringsAsFactors = FALSE
      )
      path_dir <- dirname(path_data)
      if (file.exists(path_dir)) {
        saveRDS(polygons_note_information, path_data)
      }
    }
    #===================================================== ver1 into ver2
    
  } else {
    polygons_note_information <- data.frame(
      Primary_polygon_class = rep("",10),
      Secondary_polygon_class = rep("",10),
      Tertiary_polygon_class = rep("",10),
      Polygon_number = rep("",10),
      Meanings = rep("",10),
      stringsAsFactors = FALSE
    )
    path_dir <- dirname(path_data)
    if (file.exists(path_dir)) {
      saveRDS(polygons_note_information, path_data)
    }
  }
  return(polygons_note_information)
}

tag_note_function <- function(path_data){
  if (file.exists(path_data)) {
    tags_note_information <- readRDS(path_data)
  } else {
    tags_note_information <- data.frame(
      Primary_tag_class = rep("",10),
      Secondary_tag_class = rep("",10),
      Tertiary_tag_class = rep("",10),
      Tag_number = rep("",10),
      Meanings = rep("",10),
      #Enhancement = rep("F", 10),
      #Only_train_area = rep("F", 10),
      stringsAsFactors = FALSE
    )
    path_dir <- dirname(path_data)
    if (file.exists(path_dir)) {
      saveRDS(tags_note_information, path_data)
    }
  }
  return(tags_note_information)
}
thumb_restricted_region_function <- function(slide_file,
                                             polygon_layer_select) {
  polygon_annotated_data <- readRDS(paste0(slide_file %>% dirname(), "/polygons.rds"))
  polygons <- polygon_annotated_data[[polygon_layer_select]]
  polygon_number <- length(polygons)

  
  if (polygon_number > 0) {
    for (j in 1:polygon_number) {
      polygon_length <- length(polygons[[j]]$geometry$coordinates[[1]])
      if (polygons[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygons[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
        polygons[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygons[[j]]$geometry$coordinates[[1]][[1]]
      }
      if ((polygons[[j]] %>% lawn_kinks())$features %>% length() != 0) {
        all_poly <- polygons[[j]] %>% lawn_coordall()
        clip_poly <- polysimplify(list(list(x = all_poly[,1], y = all_poly[,2])))
        pointer_to_max <- list(polygon_number = 0, point_number = 0)
        for (l in 1:length(clip_poly)) {
          if (clip_poly[[l]]$x %>% length() > pointer_to_max$point_number) {
            pointer_to_max$polygon_number <- l
            pointer_to_max$point_number <- clip_poly[[l]]$x %>% length()
          }
        }
        clip_poly <- clip_poly[[pointer_to_max$polygon_number]] %>% abind::abind(along = 2) %>% unname()
        choose_poly <- lapply(1:nrow(clip_poly), function(x){
          list(clip_poly[x,1], clip_poly[x,2])
        })
        polygons[[j]]$geometry$coordinates <- list(choose_poly)
      }
      polygon_length <- length(polygons[[j]]$geometry$coordinates[[1]])
      if (polygons[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygons[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
        polygons[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygons[[j]]$geometry$coordinates[[1]][[1]]
      }
      if (j == 1) {
        polygon_u <- polygons[[1]]
      } else {
        polygon_u <- lawn_union(polygon_u, polygons[[j]])
      }
    }
    
    img_arr <- array(0, c(2048,2048))
    image_dim <- XY2LonLat(2048, 2048, zoom = 6)
    zero_dim <- XY2LonLat(0, 0, zoom = 6)
    
    image_polygon <- lawn_polygon(list(list(
      c(zero_dim %>% as.numeric()),
      c((zero_dim %>% as.numeric())[1],(image_dim %>% as.numeric())[2]),
      c(image_dim %>% as.numeric()),
      c((image_dim %>% as.numeric())[1], (zero_dim %>% as.numeric())[2]),
      c(zero_dim %>% as.numeric())
    )))
    
    if (class(polygon_u) == "polygon") {
      df <- LonLat2XY(lon_deg = polygon_u$geometry$coordinates[1,,1], lat_deg = polygon_u$geometry$coordinates[1,,2], zoom = 6)
      df$x <- replace(df$x, which(df$x <= 0), 1)
      df$x <- replace(df$x, which(df$x >= 2048), 2048)
      df$y <- replace(df$y, which(df$y <= 0), 1)
      df$y <- replace(df$y, which(df$y >= 2048), 2048)
      rdf <- list()
      if (dim(polygon_u$geometry$coordinates)[1] != 1) {
        for (i in 2:dim(polygon_u$geometry$coordinates)[1]) {
          rdf[[i-1]] <- LonLat2XY(lon_deg = x[i,,1], lat_deg = x[i,,2], zoom = 6)
          rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
          rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= 2048), 2048)
          rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
          rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= 2048), 2048)
        }
      }
      matrix_polygon <- list(list(df, rdf))
    } else {
      matrix_polygon <- lapply(polygon_u$geometry$coordinates, function(x){
        if (class(x) == "list") {
          df <- LonLat2XY(lon_deg = x[[1]][,1], lat_deg = x[[1]][,2], zoom = 6)
          df$x <- replace(df$x, which(df$x <= 0), 1)
          df$x <- replace(df$x, which(df$x >= 2048), 2048)
          df$y <- replace(df$y, which(df$y <= 0), 1)
          df$y <- replace(df$y, which(df$y >= 2048), 2048)
          rdf <- list()
          for (i in 2:length(x)) {
            rdf[[i-1]] <- LonLat2XY(lon_deg = x[[i]][,1], lat_deg = x[[i]][,2], zoom = 6)
            rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
            rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= 2048), 2048)
            rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
            rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= 2048), 2048)
          }
          
        } else if (class(x) == "array") {
          df <- LonLat2XY(lon_deg = x[1,,1], lat_deg = x[1,,2], zoom = 6)
          df$x <- replace(df$x, which(df$x <= 0), 1)
          df$x <- replace(df$x, which(df$x >= 2048), 2048)
          df$y <- replace(df$y, which(df$y <= 0), 1)
          df$y <- replace(df$y, which(df$y >= 2048), 2048)
          rdf <- list()
          if (dim(x)[1] != 1) {
            for (i in 2:dim(x)[1]) {
              rdf[[i-1]] <- LonLat2XY(lon_deg = x[i,,1], lat_deg = x[i,,2], zoom = 6)
              rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x <= 0), 1)
              rdf[[i-1]]$x <- replace(rdf[[i-1]]$x, which(rdf[[i-1]]$x >= 2048), 2048)
              rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y <= 0), 1)
              rdf[[i-1]]$y <- replace(rdf[[i-1]]$y, which(rdf[[i-1]]$y >= 2048), 2048)
            }
          }
        }
        list(df, rdf)
      })
    }
    
    for (kk in 1:length(matrix_polygon)) {
      for (ii in 2:nrow(matrix_polygon[[kk]][[1]])) {
        point_draw <- point_line_function(c(matrix_polygon[[kk]][[1]][ii-1,1], matrix_polygon[[kk]][[1]][ii-1,2]),
                                          c(matrix_polygon[[kk]][[1]][ii,1], matrix_polygon[[kk]][[1]][ii,2]))
        img_arr[point_draw %>% as.matrix()] <- 1
      }
      if (length(matrix_polygon[[kk]][[2]]) > 0) {
        for (jj in 1:length(matrix_polygon[[kk]][[2]])) {
          for (ii in 2:nrow(matrix_polygon[[kk]][[2]][[jj]])) {
            point_draw <- point_line_function(c(matrix_polygon[[kk]][[2]][[jj]][ii-1,1], matrix_polygon[[kk]][[2]][[jj]][ii-1,2]),
                                              c(matrix_polygon[[kk]][[2]][[jj]][ii,1], matrix_polygon[[kk]][[2]][[jj]][ii,2]))
            img_arr[point_draw %>% as.matrix()] <- -1
          }
        }
      }
    }
    
    chunk_img <- img_arr %>% replace(which(img_arr == -1), 0) %>% fillHull()
    rm_chunk_img <- img_arr*-1
    rm_chunk_img <- rm_chunk_img %>% replace(which(rm_chunk_img == -1), 0) %>% fillHull()
    img_arr <- chunk_img - rm_chunk_img
    #blank_label_img_all[,,nrow(use_point_df) + k] <- img_arr
    return(img_arr)
  }
}
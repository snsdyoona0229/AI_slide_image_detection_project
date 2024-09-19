contour_to_geojson_function <- function(contour, polygon_simplify, current_chunk, inner_belong = NULL) {


  
  blank_geo <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
  if (is.null(inner_belong)) {
    pre_result <- lapply(1:length(contour), function(x){
      id_num <- paste0(current_chunk, 
                       ",out,", 
                       paste0("id", names(contour)[x]),
                       ",number1,"
      )
      data_in <- blank_geo
      data_in$geometry$coordinates <- contour[[x]] %>% 
        unlist() %>% 
        matrix(nrow = length(contour[[x]]), 
               ncol = 2, 
               byrow = TRUE) %>% 
        array(c(1,
                length(contour[[x]]),
                2))
      data_in$properties$layerId <- id_num
      data_in$properties$edit_id <- id_num
      data_in$properties$`_leaflet_id` <- id_num
      data_in
      #if (lawn_area(data_in) >= 1) {
      #  lawn_simplify(data_in, tolerance = polygon_simplify)
      #} else {
      #  NULL
      #}
    })
    #pre_result <- pre_result[(pre_result %>% sapply(is.null)) == FALSE]
    if (length(pre_result) > 0) {
      for (j in 1:length(pre_result)) {
        polygon_length <- nrow(pre_result[[j]]$geometry$coordinates[1,,])
        #if ((pre_result[[j]] %>% lawn_kinks())$features %>% length() != 0) {
        all_poly <- pre_result[[j]] %>% lawn_coordall()
        clip_poly <- polysimplify(list(list(x = all_poly[,1], y = all_poly[,2])))
        if (length(clip_poly) > 0) {
          for (i in 1:length(clip_poly)) {
            temp_poly <- clip_poly[[i]] %>% abind::abind(along = 2) %>% unname()
            temp_poly <- abind::abind(temp_poly, temp_poly[1,] %>% array(c(1,2)), along = 1) %>% array(c(1,nrow(temp_poly) + 1, 2)) %>% unname()
            if (i == 1) {
              pre_result[[j]]$geometry$coordinates <- temp_poly
              pre_result[[j]] <- lawn_simplify(pre_result[[j]], tolerance = polygon_simplify)
            } else {
              add_number <- length(pre_result) + 1
              pre_result[[add_number]] <- pre_result[[j]]
              pre_result[[add_number]]$geometry$coordinates <- temp_poly
              pre_result[[add_number]]$properties$layerId <- pre_result[[j]]$properties$layerId %>% str_replace("number1", paste0("number", i))
              pre_result[[add_number]]$properties$`_leaflet_id` <- pre_result[[add_number]]$properties$layerId
              pre_result[[add_number]]$properties$edit_id <- pre_result[[add_number]]$properties$layerId
              pre_result[[add_number]] <- lawn_simplify(pre_result[[add_number]], tolerance = polygon_simplify)
            }
          }
        } else {
          pre_result[[j]] <- NA
        }
        #}
      }
    }
    pre_result <- pre_result[(pre_result %>% is.na()) == FALSE]
    names(pre_result) <- sapply(pre_result, function(x){
      x$properties$layerId
    })
    #return(pre_result)
  } else {
    if (length(inner_belong) > 0) {
      pre_result <- lapply(1:length(contour), function(x){
        id_num <- paste0(current_chunk, 
                         ",in,", 
                         paste0("id", names(contour)[x]),
                         ",number1,",
                         paste0("belong", inner_belong[x], ",")
        )
        data_in <- blank_geo
        data_in$geometry$coordinates <- contour[[x]] %>% 
          unlist() %>% 
          matrix(nrow = length(contour[[x]]), 
                 ncol = 2, 
                 byrow = TRUE) %>% 
          array(c(1,
                  length(contour[[x]]),
                  2))
        data_in$properties$layerId <- id_num
        data_in$properties$edit_id <- id_num
        data_in$properties$`_leaflet_id` <- id_num
        data_in
        #if (lawn_area(data_in) >= 1) {
        #  lawn_simplify(data_in, tolerance = polygon_simplify)
        #} else {
        #  NULL
        #}
      })
      #pre_result <- pre_result[(pre_result %>% sapply(is.null)) == FALSE]
      if (length(pre_result) > 0) {
        for (j in 1:length(pre_result)) {
          polygon_length <- nrow(pre_result[[j]]$geometry$coordinates[1,,])
          #if ((pre_result[[j]] %>% lawn_kinks())$features %>% length() != 0) {
          all_poly <- pre_result[[j]] %>% lawn_coordall()
          clip_poly <- polysimplify(list(list(x = all_poly[,1], y = all_poly[,2])))
          if (length(clip_poly) > 0) {
            for (i in 1:length(clip_poly)) {
              temp_poly <- clip_poly[[i]] %>% abind::abind(along = 2) %>% unname()
              temp_poly <- abind::abind(temp_poly, temp_poly[1,] %>% array(c(1,2)), along = 1) %>% array(c(1,nrow(temp_poly) + 1, 2)) %>% unname()
              if (i == 1) {
                pre_result[[j]]$geometry$coordinates <- temp_poly
                pre_result[[j]] <- lawn_simplify(pre_result[[j]], tolerance = polygon_simplify)
              } else {
                add_number <- length(pre_result) + 1
                pre_result[[add_number]] <- pre_result[[j]]
                pre_result[[add_number]]$geometry$coordinates <- temp_poly
                pre_result[[add_number]]$properties$layerId <- pre_result[[j]]$properties$layerId %>% str_replace("number1", paste0("number", i))
                pre_result[[add_number]]$properties$`_leaflet_id` <- pre_result[[add_number]]$properties$layerId
                pre_result[[add_number]]$properties$edit_id <- pre_result[[add_number]]$properties$layerId
                pre_result[[add_number]] <- lawn_simplify(pre_result[[add_number]], tolerance = polygon_simplify)
              }
            }
          } else {
            pre_result[[j]] <- NA
          }
          #}
        }
      }
      pre_result <- pre_result[(pre_result %>% is.na()) == FALSE]
      names(pre_result) <- sapply(pre_result, function(x){
        x$properties$layerId
      })
    } else {
      pre_result <- NULL
    }
    #return(pre_result)
  }
  #for (ii in 1:length(pre_result)) {
  #  pre_result[[ii]] <- lawn_union(pre_result[[ii]], pre_result[[ii]]) %>% geojson_split_function()
  #}
  #pre_result <- pre_result %>% unlist(recursive = FALSE)
  return(pre_result)
}

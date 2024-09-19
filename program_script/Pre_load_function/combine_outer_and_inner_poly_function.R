combine_outer_and_inner_poly_function <- function(outer_all_polygon_df, inner_all_polygon_df){
  

  outer_key <- lapply(outer_all_polygon_df$Index, function(xx){
    serch_word <- xx %>% 
      str_split(",id|,") %>% 
      sapply(function(x){paste0(x[1],"___",x[3])})
  })
  outer_df <- data.frame(
    outer_id = outer_key %>% unlist(),
    outer_number = sapply(1:length(outer_key), function(x){
      rep(x, length(outer_key[[x]]))
    }) %>% unlist()
  )
  
  if (length(inner_all_polygon_df$Index) > 0) {
    inner_key <- lapply(inner_all_polygon_df$Index, function(xx){
      serch_word <- xx %>% 
        str_split(",belong|,") %>% 
        sapply(function(x){paste0(x[1],"___",x[5])})
    })
    inner_df <- data.frame(
      inner_id = inner_key %>% unlist(),
      inner_number = sapply(1:length(inner_key), function(x){
        rep(x, length(inner_key[[x]]))
      }) %>% unlist()
    )
  } else {
    inner_key <- list()
    inner_df <- data.frame(inner_id = NA,
                           inner_number = NA)
    inner_df <- inner_df[-1,]
  }

  
  #outer_index_match <- future_lapply(outer_key, function(x){
  #  ok_inner_df <- inner_df %>% filter(inner_id %in% x)
  #  ok_inner_df$inner_number %>% unique()
  #}, future.seed = NULL)
  
  outer_index_match <- lapply(outer_key, function(x){
    ok_inner_df <- inner_df %>% filter(inner_id %in% x)
    ok_inner_df$inner_number %>% unique()
  })
  #final_id <- sample(1000000, length(outer_all_polygon_df$Poly))
  final_id <- dqsample.int(1000000, length(outer_all_polygon_df$Poly))
  
  #final_results <- future_lapply(1:length(outer_all_polygon_df$Poly), function(i){
  #  pre_change_poly <- outer_all_polygon_df$Poly[[i]]
  #  pre_change_poly$properties$layerId <- final_id[i] %>% as.character()
  #  pre_change_poly$properties$edit_id <- final_id[i] %>% as.character()
  #  pre_change_poly$properties$`_leaflet_id` <- final_id[i] %>% as.integer()
  #  if (length(outer_index_match[[i]]) > 0) {
  #    all_location <- inner_all_polygon_df$Poly[outer_index_match[[i]]] %>% lapply(lawn_coordall) %>% unname()
  #    original_location <- pre_change_poly$geometry$coordinates
  #    if (class(original_location) == "list") {
  #      pre_change_poly$geometry$coordinates <- c(pre_change_poly$geometry$coordinates, all_location)
  #    } else if (class(original_location) == "array") {
  #      pre_change_poly$geometry$coordinates <- list(array(original_location, c(dim(original_location)[2:3])))
  #      pre_change_poly$geometry$coordinates <- c(pre_change_poly$geometry$coordinates, all_location)
  #    }
  
  #  }
  #  pre_change_poly
  #}, future.seed = NULL)
  final_results <- lapply(1:length(outer_all_polygon_df$Poly), function(i){
    pre_change_poly <- outer_all_polygon_df$Poly[[i]]
    pre_change_poly$properties$layerId <- final_id[i] %>% as.character()
    pre_change_poly$properties$edit_id <- final_id[i] %>% as.character()
    pre_change_poly$properties$`_leaflet_id` <- final_id[i] %>% as.integer()
    if (length(outer_index_match[[i]]) > 0) {
      all_location <- inner_all_polygon_df$Poly[outer_index_match[[i]]] %>% lapply(lawn_coordall) %>% unname()
      original_location <- pre_change_poly$geometry$coordinates
      if (class(original_location) == "list") {
        pre_change_poly$geometry$coordinates <- c(pre_change_poly$geometry$coordinates, all_location)
      } else if (class(original_location) == "array") {
        pre_change_poly$geometry$coordinates <- list(array(original_location, c(dim(original_location)[2:3])))
        pre_change_poly$geometry$coordinates <- c(pre_change_poly$geometry$coordinates, all_location)
      }
    }
    pre_change_poly
  })
  names(final_results) <- final_id
  return(final_results)
}
step1_cluster_geojson_merge <- function(all_polygon_df){
  #names(all_polygon_df) <- basename(all_polygon_df_file_location)
  
  done_poly <- lapply(all_polygon_df, function(x) {
    x$Done[[1]]
  }) %>% unname() %>% unlist(recursive = FALSE)
  
  check_table <- lapply(all_polygon_df, function(x) {
    x$Check[[2]]
  }) %>% abind::abind(along = 1) %>% as.data.frame()
  
  check_poly <- lapply(all_polygon_df, function(x) {
    x$Check[[1]]
  }) %>% unname() %>% unlist(recursive = FALSE)
  
  names(check_poly) <- check_table$id
  
  if (length(check_poly) > 0) {
    check_group <- lapply(1:nrow(check_table), function(x){
      check <- c(check_table$check_left[x],
                 check_table$check_right[x],
                 check_table$check_up[x],
                 check_table$check_down[x])
      list(check = check[!is.na(check)],
           origin = check_table$original_chunk[x])
    })
    
    names(check_group) <- check_table$id
    
    check_cluster <- lapply(1:length(check_group), function(x){
      #x <- 1
      #origin_chunk <- check_group[[x]]$origin
      #check_chunk <- check_group[[x]]$check
      
      cluster_poly <- c(names(check_group)[x])
      total_check_chunk <- check_group[[x]]$check
      check_name <- check_group[[x]]$origin
      for (i in total_check_chunk) {
        #i <- "_1_5_"
        if ((sapply(check_group, function(x){(str_detect(x$origin, i) %>% sum()) > 0}) %>% sum()) > 0) {
          total_check_chunk <- total_check_chunk[total_check_chunk != i]
          check_number_use <- sapply(check_group, function(x){(str_detect(x$origin, i) %>% sum()) > 0}) %>% which()
          check_number_use <- check_number_use[check_number_use > x]
          total_check_poly <- check_poly[check_number_use]
          if (length(total_check_poly) > 0) {
            total_check_poly <- total_check_poly[sapply(check_group[names(total_check_poly)], function(xxx){(intersect(xxx$check, check_name) %>% length()) > 0}) %>% which()]
            if (length(total_check_poly) > 0) {
              cluster_poly <- c(cluster_poly, names(total_check_poly))
            }
          }
        }
      }
      list(Merge_id = cluster_poly, 
           Still_check = total_check_chunk,
           Original_chunk = check_group[[x]]$origin)
    })
    
    names(check_cluster) <- check_table$id
    
    
    #neo_site_cluster <- list()
    
    #merge_name_id <- list()
    site_cluster <- lapply(check_cluster, function(x){
      merge_site <- sapply(check_cluster, function(xx){sum(xx$Merge_id %in% x$Merge_id) > 0}) %>% which()
    })
    
    
    for (i in 1:length(site_cluster)) {
      all_site <- sapply(site_cluster, function(x){sum(x %in% i) > 0}) %>% which()
      site_cluster[[all_site[1]]] <- site_cluster[all_site] %>% unlist() %>% unique()
      
      if (length(all_site) != 1) {
        site_cluster[all_site[2:length(all_site)]] <- NULL
      }
    }
    
    
    per_merge_list <- lapply(1:length(site_cluster), function(x){
      list(Merge_id = check_cluster[site_cluster[[x]]] %>% sapply(function(xx){xx$Merge_id}) %>% unlist() %>% unique(), 
           Still_check = check_cluster[site_cluster[[x]]] %>% sapply(function(xx){xx$Still_check}) %>% unlist() %>% unique(),
           Original_chunk = check_cluster[site_cluster[[x]]] %>% sapply(function(xx){xx$Original_chunk}) %>% unlist() %>% unique(),
           Polygon = check_poly[site_cluster[[x]]])
    })
    
    
    merge_list <- future_lapply(per_merge_list, function(x){
      list(Merge_id = x$Merge_id, 
           Still_check = x$Still_check,
           Original_chunk = x$Original_chunk,
           Polygon = x$Polygon %>% geojson_merge_function(method = "exact"))
    }, future.seed = NULL)
    
    names(merge_list) <- names(site_cluster)
    
    done_merge_poly <- sapply(merge_list, function(x){
      if (length(x$Still_check) == 0) {
        TRUE
      } else {
        FALSE
      }
    }) 
    
    keep_check_list <- merge_list[done_merge_poly == FALSE]
    done_check_list <- merge_list[done_merge_poly]
    done_poly <- c(done_poly, lapply(done_check_list, function(x){
      x$Polygon
    }))
    
    return(list(Done = done_poly,
                Check = keep_check_list))
  } else {
    return(list(Done = done_poly,
                Check = NULL))
  }
}
step2_cluster_geojson_merge <- function(all_polygon_df, last_result){
  pre_step2_result <- step1_cluster_geojson_merge(all_polygon_df = all_polygon_df)
  
  done_poly <- c(last_result$Done, pre_step2_result$Done)
  
  check_group <- c(last_result$Check, pre_step2_result$Check)
  check_poly <- lapply(check_group, function(x){x$Polygon})
  
  check_cluster <- lapply(1:length(check_group), function(x){
    #x <- 10
    #origin_chunk <- check_group[[x]]$origin
    #check_chunk <- check_group[[x]]$check
    
    cluster_poly <- check_group[[x]]$Merge_id
    total_check_chunk <- check_group[[x]]$Still_check
    check_name <- check_group[[x]]$Original_chunk
    for (i in total_check_chunk) {
      #i <- "_1_5_"
      if ((sapply(check_group, function(x){((x$Original_chunk %in% i) %>% sum()) > 0}) %>% sum()) > 0) {
        total_check_chunk <- total_check_chunk[total_check_chunk != i]
        check_number_use <- sapply(check_group, function(x){((x$Original_chunk %in% i) %>% sum()) > 0}) %>% which()
        check_number_use <- check_number_use[check_number_use > x]
        total_check_poly <- check_poly[check_number_use]
        if (length(total_check_poly) > 0) {
          total_check_poly <- total_check_poly[sapply(check_group[names(total_check_poly)], function(xxx){(intersect(xxx$Still_check, check_name) %>% length()) > 0}) %>% which()]
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
  
  names(check_cluster) <- names(check_group)
  
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
}
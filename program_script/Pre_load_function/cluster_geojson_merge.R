cluster_geojson_merge <- function(all_polygon_df){
  #names(all_polygon_df) <- basename(all_polygon_df_file_location)

  
  done_poly <- lapply(all_polygon_df, function(x) {
    x$Done[[1]]
  }) %>% unname() %>% unlist(recursive = FALSE)
  
  done_site <- lapply(all_polygon_df, function(x) {
    x$Done[[1]] %>% names()
  }) %>% unlist()
  done_site <- lapply(done_site, function(x){x})
  names(done_site) <- sapply(done_site, function(x){x})
  
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
      cluster_poly <- c(names(check_group)[x])
      total_check_chunk <- check_group[[x]]$check
      #print(total_check_chunk)
      check_name <- check_group[[x]]$origin
      need_check_region_number <- which(str_detect(names(check_group)[x:length(check_group)], paste0(total_check_chunk, collapse = "|"))) + (x - 1)
      if (length(need_check_region_number) > 0) {
        need_check_region_number <- need_check_region_number[sapply(need_check_region_number, function(xx){check_name %in% check_group[[xx]]$check})]
      }
      if (length(need_check_region_number) > 0) {
        need_check_region_number <- need_check_region_number[sapply(need_check_region_number, function(xx){
          A <- lawn_coordall(check_poly[[x]])
          A <- list(list(x = A[,1], y = A[,2]))
          B <- lawn_coordall(check_poly[[xx]])
          B <- list(list(x = B[,1], y = B[,2]))
          length(polyclip(A, B, op = "intersect")) != 0
        })]
      }
      #print(x)
      list(Merge_id = names(check_group)[c(x,need_check_region_number)], 
           Original_id = names(check_group)[x])
    })
    
    names(check_cluster) <- check_table$id
    
    site_cluster <- lapply(check_cluster, function(x){
      x$Merge_id
    })
    
    
    for (i in 1:length(site_cluster)) {
      all_site <- check_cluster[[i]]$Merge_id
      side_with_number <- (sapply(site_cluster, function(x){(x %in% all_site) %>% sum()}) > 0) %>% which()
      site_cluster[[side_with_number[1]]] <- site_cluster[side_with_number] %>% unlist() %>% unique()
      
      if (length(side_with_number) != 1) {
        site_cluster[side_with_number[2:length(side_with_number)]] <- NULL
      }
    }
    
    merge_poly_list <- lapply(site_cluster, function(x){
      check_poly[x] %>% fast_geojson_merge_function()
    })
    
    return(list(Poly = c(merge_poly_list, done_poly),
                Index = c(site_cluster, done_site)))
  } else {
    return(list(Poly = done_poly,
                Index = done_site))
  }
}


#foreach(batch_number =1:batch_size,.combine = rbind) %dopar% {
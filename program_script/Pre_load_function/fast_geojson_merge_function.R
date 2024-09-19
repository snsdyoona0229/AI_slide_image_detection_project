
fast_geojson_merge_function <- function(polygon_list){
  cor_list <- polygon_list %>% lapply(function(x){
    cor_region <- lawn_coordall(x)
    list(list(x=cor_region[,1], y=cor_region[,2]))
  })
  if (length(cor_list) > 1) {
    while(length(cor_list) > 1){
      check_number <- floor(length(cor_list)/2)
      for (i in 1:check_number) {
        cor_list[[i*2 - 1]] <- polyclip(cor_list[[i * 2 - 1]], cor_list[[i * 2]], op = "union")
      }
      cor_list[(1:check_number) * 2] <- NULL
    }
    first_poly <- lapply(cor_list[[1]], function(x){
      pre_array <- x %>% abind::abind(along = 2)
      abind::abind(pre_array, pre_array[1,] %>% array(c(1,2)), along = 1) %>% array(c(nrow(pre_array) + 1, 2)) %>% unname()
    })
    output_poly <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
    output_poly$geometry$coordinates <- first_poly
    return(output_poly)
  } else {
    first_poly <- cor_list[[1]]
    pre_array <- first_poly[[1]] %>% abind::abind(along = 2)
    first_poly <- abind::abind(pre_array, pre_array[1,] %>% array(c(1,2)), along = 1) %>% array(c(1, nrow(pre_array) + 1, 2)) %>% unname()
    output_poly <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
    output_poly$geometry$coordinates <- first_poly
    return(output_poly)
  }
}

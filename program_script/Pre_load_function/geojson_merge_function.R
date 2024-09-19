geojson_merge_function <- function(polygon_list, method = "blur") {
  if (method == "blur") {
    return(polygon_list %>% unname() %>% lawn_featurecollection() %>% lawn_merge())
  } else if (method == "exact") {
    poly_u <- polygon_list[[1]]
    for (i in 1:length(polygon_list)) {
      poly_u <- lawn_union(., polygon_list[[i]])
    }
    return(poly_u)
  }
}

geojson_merge_function_fix <- function(polygon_list, method = "fix") {

  x <- '[
    [[[]]]
    ]'
  
  polygons_collect <- lawn_multipolygon(x)
  
  for (i in 1:length(polygon_list)) {
      polygons_collect$geometry$coordinates[[i]] <- polygon_list[[i]]$geometry$coordinates
    }

  properties <- list(
    '_leaflet_id' = c(polygon_list[[1]]$properties[[1]]),
    layerId = c(polygon_list[[1]]$properties[[2]]),
    edit_id = c(polygon_list[[1]]$properties[[3]])
   )
   polygons_collect$properties <- properties

   return(polygons_collect)

}

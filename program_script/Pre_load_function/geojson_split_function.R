geojson_split_function <- function(multipolygon) {
  if (multipolygon$geometry$type == "Polygon") {
    return(list(multipolygon))
  } else {
    polygon_number <- multipolygon$geometry$coordinates %>% length()
    blank_geo <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
    new_polygon_id <- sample(1:10000000, polygon_number)
    split_result <- lapply(1:polygon_number, function(x){
      id_num <- new_polygon_id[x]
      data_in <- blank_geo
      data_in$geometry$coordinates <- multipolygon$geometry$coordinates[[x]]
      data_in$properties$layerId <- as.character(id_num)
      data_in$properties$edit_id <- as.character(id_num)
      data_in$properties$`_leaflet_id` <- id_num
      class(data_in) <- "polygon"
      data_in
    })
    names(split_result) <- new_polygon_id %>% as.integer()
    return(split_result)
  }
}
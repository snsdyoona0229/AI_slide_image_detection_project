shinyFileChoose(input = input, 
                id = 'import_annotation_file', 
                roots = getVolumes()(), 
                filetypes=c('ndpa', 
                            "rds", 
                            "json"))

observeEvent(input$import_annotation_file, {
  if (input$import_annotation_file[1] %>% class() == "list") {
    progressSweetAlert(
      session = session, id = "Import_annotation_myprogress",
      title = "Start Importing data",
      display_pct = TRUE, value = 0
    )
    
    
    updateProgressBar(
      session = session,
      title = "Disc file detection and localization",
      id = "Import_annotation_myprogress",
      value = 20
    )
    
    root_name <- getVolumes()()[input$import_annotation_file$root] %>% str_replace("\\/", "")
    choose_file <- sapply(input$import_annotation_file$files, function(x){
      paste0(root_name, unlist(x) %>% paste0(collapse = "/"))
    }) %>% as.character()
    current_file <- list.files(paste0("www/Slide/", 
                                      input$Annotation_group, 
                                      "/", 
                                      input$Image_input, 
                                      "/"), 
                               pattern = ".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax", full.names = TRUE)
    
    if (str_detect(choose_file, ".ndpa") & str_detect(current_file, ".ndpi")) {
      updateProgressBar(
        session = session,
        title = "Loading .ndpa file and processing",
        id = "Import_annotation_myprogress",
        value = 50
      )
      
      blank_leaflet_geojson <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
      NDPI_file_info <- NDPI_info(current_file)
      zero_x_nano <- NDPI_file_info$Physicalx_center - NDPI_file_info$Physicalwidth_nm/2
      zero_y_nano <- NDPI_file_info$Physicaly_cente - NDPI_file_info$Physicalheight_nm/2
      nano_pixel_ratio <- (NDPI_file_info$Physicalwidth_nm/NDPI_file_info$Width_pixel + NDPI_file_info$Physicalheight_nm/NDPI_file_info$Height_pixel)/2
      
      NDPI_annotation_file <- xmlParse(choose_file)
      NDPI_annotation_file <- xmlToList(NDPI_annotation_file)
      polygon_number <- length(NDPI_annotation_file)
      new_polygon_id <- sample(100000, polygon_number) %>% as.character()
      
      NDPI_annotation_file <- lapply(1:length(NDPI_annotation_file), function(x){
        NDPI_annotation_file[[x]]$annotation$pointlist %>% lapply(function(xx){
          (c(xx$x, xx$y) %>% as.integer() - c(zero_x_nano, zero_y_nano))/nano_pixel_ratio
        }) %>% abind::abind(along = 0)
      }) %>% lapply(function(x) {
        XY2LonLat(x = x[,1], y = x[,2], zoom = 12 + 1)
      }) %>% lapply(function(x) {
        pre_add <- blank_leaflet_geojson
        pre_add$geometry$coordinates <- list(x %>% as.matrix())
        lawn_simplify(pre_add, tolerance = 0)
      }) 
      
      NDPI_annotation_file <- lapply(1:length(NDPI_annotation_file), function(x){
        NDPI_annotation_file[[x]]$properties$layerId <- new_polygon_id[x]
        NDPI_annotation_file[[x]]$properties$edit_id <- new_polygon_id[x]
        NDPI_annotation_file[[x]]$properties$`_leaflet_id` <- new_polygon_id[x] %>% as.integer()
        NDPI_annotation_file[[x]]
      })
      names(NDPI_annotation_file) <- new_polygon_id
      #saveRDS(NDPI_annotation_file, "test.rds")
      
      NDPI_annotation_file <- polygon_correction_function(NDPI_annotation_file)
      NDPI_annotation_file <- NDPI_annotation_file %>% geojson_merge_function(method = "exact") %>% geojson_split_function()
      
      
      updateProgressBar(
        session = session,
        title = "Applying annotation to slide",
        id = "Import_annotation_myprogress",
        value = 80
      )
      
      current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
      Polygons_data_current_on_map$list[[current_polygon_class]] <- NDPI_annotation_file
      polygon_names <- Polygons_data_current_on_map$list[[current_polygon_class]] %>% names()
      
      if (length(NDPI_annotation_file) > 0) {
        Polygons_data_current_on_map$bbox <- lapply(NDPI_annotation_file, function(x){
          lawn_bbox(x)
        })
      } else {
        Polygons_data_current_on_map$bbox <- list()
      }
      
      leafletProxy("image_viewer", session) %>% 
        clearGeoJSON()
      
      for (i in polygon_names) {
        leafletProxy("image_viewer", session) %>% 
          addGeoJSON(NDPI_annotation_file[[i]], 
                     layerId = i, 
                     group = "GP", 
                     fillOpacity = 0.2 * (1-sum(input$Polygon_opacity_on_off)), 
                     color = input$Polygon_viewing_color,
                     options = leafletOptions(pane = "Polygons"))
      }
      
      updateProgressBar(
        session = session,
        title = "Finish",
        id = "Import_annotation_myprogress",
        value = 100
      )
    }
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "Importing annotation complete!!",
      text = "The annotation have been updated!!",
      type = "success"
    )
  }
})
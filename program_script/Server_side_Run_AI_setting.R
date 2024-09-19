observeEvent(input$image_viewer_geojson_double_click, {
  p <- input$image_viewer_geojson_double_click
  
  slide_info_location <- paste0("www/",
                                input$Slide_or_image,
                                "/",
                                input$Annotation_group,
                                "/",
                                input$Image_input,
                                "/info/info.rds")
  
  if (file.exists(slide_info_location)) {
    slide_info <- readRDS(slide_info_location)
  } else {
    img_file <- list.files(paste0("www/",
                                  input$Slide_or_image,
                                  "/",
                                  input$Annotation_group,
                                  "/",
                                  input$Image_input), pattern = ".jpg")
    img_file <- img_file %>% str_replace_all(".jpg", "") %>% str_split("___") %>% unlist()
    
    
    slide_info <- list(
      img_file[2],
      img_file[3],
      "-",
      "-",
      "-",
      "-",
      "-",
      "1",
      "3",
      "8"
    )
  }
  

  
  df_note <- as.data.frame(hot_to_r(input$Polygons_note_table))
  if (nrow(df_note) == 0) {
    df_note <- readRDS(paste0("www/",
                              input$Slide_or_image,
                              "/",
                              input$Annotation_group,
                              "/polygons_note.rds"))
  }
  df_note$class <- paste0(
    df_note$Primary_polygon_class,
    "_",
    df_note$Secondary_polygon_class,
    "_",
    df_note$Tertiary_polygon_class,
    "_G_",
    df_note$Polygon_number
  )
  
  current_meaning <- df_note %>% filter(class == paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_", p$group))
  current_meaning <- current_meaning$Meanings
  

  
  #print(lawn_coordall(Polygons_data_current_on_map$list[[current_polygon_class]][[polygon_name]]))
  
  showModal(div(id = "geojson_dbclick_analysis", modalDialog(
    title = "Selected region analysis",
    size = "l",
    column(12,
           column(2,
                  h3("Slide/Image info:"),
                  p(paste0("Width (pixel): ", slide_info[[1]])),
                  p(paste0("Height (pixel): ", slide_info[[2]])),
                  p(paste0("Physical center x (pixel): ", slide_info[[3]])),
                  p(paste0("Physical center y (pixel): ", slide_info[[4]])),
                  p(paste0("Physical width (nm): ", slide_info[[5]])),
                  p(paste0("Physical height (nm): ", slide_info[[6]])),
                  p(paste0("Max zoom: ", slide_info[[7]], "x")),
                  p(paste0("Number of z index: ", slide_info[[8]])),
                  p(paste0("Number of channels: ", slide_info[[9]])),
                  p(paste0("Bits per channel: ", slide_info[[10]])),
                  p(paste0("Current slide/image tags: ")),
                  br(),
                  h3("Layer info:"),
                  p(paste0("Current polygon layer group: ", input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_", p$group)),
                  p(paste0("Current layer meaning: ", current_meaning)),
                  p(paste0("On map point layer: ", input$Point_class_1, "_", input$Point_class_2, "_", input$Point_class_3, "_P_")),
                  br(),
                  h3("Polygon info:"),
                  p(paste0("Current polygon ID: ", p$properties$layerId)),
                  p(paste0("Current polygon tags:"))
           ),
           column(10,
                  tabsetPanel(
                    tabPanel(title = 'Calculation (area and point number within)', 
                             icon = icon("calculator", "fa-2x"),
                             br(),
                             column(6,
                                    radioGroupButtons(
                                      inputId = "Calculation_geojson_type_selection",
                                      label = "Select calculate scope",
                                      choices = c("Selected polygon", "Polygons share same layer and group"),
                                      checkIcon = list(
                                        yes = icon("square-check"),
                                        no = icon("square")
                                      )
                                    )
                             ),
                             column(6,
                                    radioGroupButtons(
                                      inputId = "Calculation_geojson_unit_selection",
                                      label = "Select calculate unit",
                                      choices = c("mm", "pixel in 40x"),
                                      checkIcon = list(
                                        yes = icon("square-check"),
                                        no = icon("square")
                                      )
                                    )
                             ),
                             verticalTabsetPanel(
                               id = "Calculation_geojson_vertical_tab_panel",
                               contentWidth = 10,
                               verticalTabPanel(
                                 title = "Shape analysis",
                                 icon = icon("shapes", "fa-2x"),
                                 column(12,
                                        actionButton(inputId = "calculate_geojson_polygon_area", 
                                                     label = "Calculate area"),
                                        verbatimTextOutput("calculate_geojson_polygon_area_result"),
                                        br(),
                                        actionButton(inputId = "calculate_geojson_polygon_perimeter", 
                                                     label = "Calculate perimeter"),
                                        verbatimTextOutput("calculate_geojson_polygon_perimeter_result"),
                                        br()
                                 )
                               ),
                               verticalTabPanel(
                                 title = "Within point analysis",
                                 icon = icon("bullseye", "fa-2x"),
                                 actionButton(inputId = "calculate_point_in_geojson_polygon", 
                                              label = "Calculate point number"),
                                 DTOutput('calculate_point_in_geojson_polygon_result'),
                                 br()
                               ),
                               verticalTabPanel(
                                 title = "Within polygon analysis",
                                 icon = icon("draw-polygon", "fa-2x"),
                                 "Content panel 3"
                               )
                             )
                    ),
                    tabPanel(title = 'Tags labeling on polygon', 
                             icon = icon("tags", "fa-2x"),
                             br()
                    ),
                    tabPanel(title = 'Object classification (AI classification)', 
                             icon = icon("hive", "fa-2x"),
                             br()
                    ),
                    tabPanel(title = 'Within area detection (AI point/polygon detection)', 
                             icon = icon("magnifying-glass", "fa-2x"),
                             br()
                    )
                  )
           )
    )
  )))
})


output$calculate_geojson_polygon_area_result = renderText({
  input$calculate_geojson_polygon_area
  req(input$calculate_geojson_polygon_area) #to prevent print at first lauch
  p <- isolate(input$image_viewer_geojson_double_click)
  current_polygon_class <- isolate(paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_", p$group))
  polygon_name <- p$id %>% as.character()
  polygon_shape <- Polygons_data_current_on_map$list[[current_polygon_class]][[polygon_name]]
  
  
  
  slide_info_location <- isolate(paste0("www/",
                                        input$Slide_or_image,
                                        "/",
                                        input$Annotation_group,
                                        "/",
                                        input$Image_input,
                                        "/info/info.rds"))
  
  if (file.exists(slide_info_location)) {
    slide_info <- readRDS(slide_info_location)
    pixel_length <- mean(c(slide_info$Physicalwidth_nm/slide_info$Width_pixel,
                           slide_info$Physicalheight_nm/slide_info$Height_pixel))
  } else {
    img_file <- list.files(paste0("www/",
                                  input$Slide_or_image,
                                  "/",
                                  input$Annotation_group,
                                  "/",
                                  input$Image_input), pattern = ".jpg")
    img_file <- img_file %>% str_replace_all(".jpg", "") %>% str_split("___") %>% unlist()
    
    slide_info <- list(
      img_file[2],
      img_file[3],
      "-",
      "-",
      "-",
      "-",
      "-",
      "1",
      "3",
      "8"
    )
    pixel_length <- "-"
  }

  #saveRDS(polygon_shape, "test.rds")
  
  if (input$Calculation_geojson_type_selection == "Selected polygon") {
    polygon_xy <- geojson_lonlat2xy_function(lonlat_geojson = polygon_shape)
    polygon_pixel_area <- xy_geojson_area_function(xy_geojson = polygon_xy)
    
    if (input$Calculation_geojson_unit_selection == "mm") {
      if (input$Slide_or_image == "Slide") {
        polygon_true_area <- polygon_pixel_area/1000000000000 * pixel_length * pixel_length
        paste0("Area (mm2): ", round(polygon_true_area, digits = 5))
      } else {
        paste0("Area (mm2): Unknown")
      }
    } else {
      paste0("Area (pixel): ", round(polygon_pixel_area))
    }
  } else {
    total_area <- sapply(Polygons_data_current_on_map$list[[current_polygon_class]], function(x){
      x %>% geojson_lonlat2xy_function() %>% xy_geojson_area_function()
    }) 
    
    if (input$Calculation_geojson_unit_selection == "mm") {
      if (input$Slide_or_image == "Slide") {
        polygon_true_area <- total_area/1000000000000 * pixel_length * pixel_length
        paste0("Total Area (mm2): ", round(polygon_true_area %>% sum(), digits = 5),
               "\n Total object number: ", length(polygon_true_area),
               "\n Min (mm2): ", round(polygon_true_area %>% min(), digits = 5),
               "\n Max (mm2): ", round(polygon_true_area %>% max(), digits = 5),
               "\n Mean (mm2): ", round(polygon_true_area %>% mean(), digits = 5),
               "\n SD (mm2): ", round(polygon_true_area %>% sd(), digits = 5))
      } else {
        paste0("Total Area (mm2): Unknown",
               "\n Total object number: ", length(total_area),
               "\n Min (mm2): Unknown",
               "\n Max (mm2): Unknown",
               "\n Mean (mm2): Unknown",
               "\n SD (mm2): Unknown")
      }
    } else {
      paste0("Total Area (pixel): ", round(total_area %>% sum()),
             "\n Total object number: ", length(total_area),
             "\n Min (pixel): ", round(total_area %>% min()),
             "\n Max (pixel): ", round(total_area %>% max()),
             "\n Mean (pixel): ", round(total_area %>% mean()),
             "\n SD (pixel): ", round(total_area %>% sd()))
    }
  }
})



output$calculate_geojson_polygon_perimeter_result = renderText({
  input$calculate_geojson_polygon_perimeter
  req(input$calculate_geojson_polygon_perimeter) #to prevent print at first lauch
  p <- isolate(input$image_viewer_geojson_double_click)
  current_polygon_class <- isolate(paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_", p$group))
  polygon_name <- p$id %>% as.character()
  polygon_shape <- Polygons_data_current_on_map$list[[current_polygon_class]][[polygon_name]]
  
  slide_info_location <- isolate(paste0("www/",
                                        input$Slide_or_image,
                                        "/",
                                        input$Annotation_group,
                                        "/",
                                        input$Image_input,
                                        "/info/info.rds"))
  
  if (file.exists(slide_info_location)) {
    slide_info <- readRDS(slide_info_location)
    pixel_length <- mean(c(slide_info$Physicalwidth_nm/slide_info$Width_pixel,
                           slide_info$Physicalheight_nm/slide_info$Height_pixel))
  } else {
    img_file <- list.files(paste0("www/",
                                  input$Slide_or_image,
                                  "/",
                                  input$Annotation_group,
                                  "/",
                                  input$Image_input), pattern = ".jpg")
    img_file <- img_file %>% str_replace_all(".jpg", "") %>% str_split("___") %>% unlist()
    
    slide_info <- list(
      img_file[2],
      img_file[3],
      "-",
      "-",
      "-",
      "-",
      "-",
      "1",
      "3",
      "8"
    )
    pixel_length <- "-"
  }
  #saveRDS(polygon_shape, "test.rds")
  
  if (input$Calculation_geojson_type_selection == "Selected polygon") {
    polygon_xy <- geojson_lonlat2xy_function(lonlat_geojson = polygon_shape)
    #saveRDS(polygon_xy, "test.rds")
    
    
    polygon_pixel_area <- xy_geojson_perimeter_function(xy_geojson = polygon_xy)
    
    if (input$Calculation_geojson_unit_selection == "mm") {
      if (input$Slide_or_image == "Slide") {
        polygon_true_area <- polygon_pixel_area/1000000 * pixel_length 
        paste0("Perimeter (mm): ", round(polygon_true_area, digits = 5))
      } else {
        paste0("Perimeter (mm): Unknown")
      }
    } else {
      paste0("Perimeter (pixel): ", round(polygon_pixel_area))
    }
  } else {
    total_area <- sapply(Polygons_data_current_on_map$list[[current_polygon_class]], function(x){
      x %>% geojson_lonlat2xy_function() %>% xy_geojson_perimeter_function()
    })
    
    if (input$Calculation_geojson_unit_selection == "mm") {
      if (input$Slide_or_image == "Slide") {
        polygon_true_area <- total_area/1000000 * pixel_length 
        paste0("Total Perimeter (mm): ", round(polygon_true_area %>% sum(), digits = 5),
               "\n Total object number: ", length(polygon_true_area),
               "\n Min (mm): ", round(polygon_true_area %>% min(), digits = 5),
               "\n Max (mm): ", round(polygon_true_area %>% max(), digits = 5),
               "\n Mean (mm): ", round(polygon_true_area %>% mean(), digits = 5),
               "\n SD (mm): ", round(polygon_true_area %>% sd(), digits = 5))
      } else {
        paste0("Total Perimeter (mm): Unknown",
               "\n Total object number: ", length(total_area),
               "\n Min (mm): Unknown",
               "\n Max (mm): Unknown",
               "\n Mean (mm): Unknown",
               "\n SD (mm): Unknown")
      }
    } else {
      paste0("Total Perimeter (pixel): ", round(total_area %>% sum()),
             "\n Total object number: ", length(total_area),
             "\n Min (pixel): ", round(total_area %>% min()),
             "\n Max (pixel): ", round(total_area %>% max()),
             "\n Mean (pixel): ", round(total_area %>% mean()),
             "\n SD (pixel): ", round(total_area %>% sd()))
    }
  }
})


output$calculate_point_in_geojson_polygon_result = renderDT({
  input$calculate_point_in_geojson_polygon
  req(input$calculate_point_in_geojson_polygon)
  
  p <- isolate(input$image_viewer_geojson_double_click)
  current_polygon_class <- isolate(paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_", p$group))
  polygon_name <- p$id %>% as.character()
  polygon_shape <- Polygons_data_current_on_map$list[[current_polygon_class]][[polygon_name]]
  
  slide_info_location <- isolate(paste0("www/",
                                        input$Slide_or_image,
                                        "/",
                                        input$Annotation_group,
                                        "/",
                                        input$Image_input,
                                        "/info/info.rds"))
  
  if (file.exists(slide_info_location)) {
    slide_info <- readRDS(slide_info_location)
    pixel_length <- mean(c(slide_info$Physicalwidth_nm/slide_info$Width_pixel,
                           slide_info$Physicalheight_nm/slide_info$Height_pixel))
  } else {
    img_file <- list.files(paste0("www/",
                                  input$Slide_or_image,
                                  "/",
                                  input$Annotation_group,
                                  "/",
                                  input$Image_input), pattern = ".jpg")
    img_file <- img_file %>% str_replace_all(".jpg", "") %>% str_split("___") %>% unlist()
    
    slide_info <- list(
      img_file[2],
      img_file[3],
      "-",
      "-",
      "-",
      "-",
      "-",
      "1",
      "3",
      "8"
    )
    pixel_length <- "-"
  }
  
  point_shape <- Points_data_current_on_map$df
  point_shape <- point_shape %>% 
    filter(point_primary == input$Point_class_1) %>% 
    filter(point_secondary == input$Point_class_2) %>% 
    filter(point_tertiary == input$Point_class_3)
  #print(point_shape)
  point_xy <- LonLat2XY(lon_deg = point_shape$point_lon,
                        lat_deg = point_shape$point_lat,
                        zoom = 12 + 1)
  point_shape$point_lon <- point_xy$x
  point_shape$point_lat <- point_xy$y
  
  if (input$Calculation_geojson_type_selection == "Selected polygon") {
    polygon_xy <- geojson_lonlat2xy_function(lonlat_geojson = polygon_shape)
    polygon_pixel_area <- xy_geojson_area_function(xy_geojson = polygon_xy)
    
    polygon_point_in_area <- xy_geojson_point_in_function(xy_geojson = polygon_xy, xy_point = point_shape)
    polygon_point_in_area <- data.frame(
      "P_1" = polygon_point_in_area[1],
      "P_2" = polygon_point_in_area[2],
      "P_3" = polygon_point_in_area[3],
      "P_4" = polygon_point_in_area[4],
      "P_5" = polygon_point_in_area[5],
      "P_6" = polygon_point_in_area[6],
      "P_7" = polygon_point_in_area[7],
      "P_8" = polygon_point_in_area[8],
      "P_9" = polygon_point_in_area[9],
      "P_10" = polygon_point_in_area[10],
      stringsAsFactors = FALSE
    )
    rownames(polygon_point_in_area) <- "Total number"
    
    if (input$Calculation_geojson_unit_selection == "mm") {
      if (input$Slide_or_image == "Slide") {
        polygon_true_area <- polygon_pixel_area/1000000000000 * pixel_length * pixel_length
        polygon_point_in_area[2,] <- round(polygon_point_in_area[1,]/polygon_true_area, digits = 5)
        rownames(polygon_point_in_area)[2] <- "Number/mm2"
      } 
    } else {
      polygon_point_in_area[2,] <- round(polygon_point_in_area[1,]/polygon_pixel_area*1000000, digits = 5)
      rownames(polygon_point_in_area)[2] <- "Number/1000000 pixel"
    }
    
  } else {
    total_point <- lapply(Polygons_data_current_on_map$list[[current_polygon_class]], function(x){
      x %>% geojson_lonlat2xy_function() %>% xy_geojson_point_in_function(xy_point = point_shape)
    })
    #saveRDS(total_point, "total_point.rds")
    
    total_point_matrix <- abind::abind(total_point, along = 0)
    
    polygon_point_in_area <- data.frame(
      "P_1" = c(total_point_matrix[,1] %>% sum(), total_point_matrix[,1] %>% max(), total_point_matrix[,1] %>% min(), total_point_matrix[,1] %>% mean() %>% round(digits = 5)),
      "P_2" = c(total_point_matrix[,2] %>% sum(), total_point_matrix[,2] %>% max(), total_point_matrix[,2] %>% min(), total_point_matrix[,2] %>% mean() %>% round(digits = 5)),
      "P_3" = c(total_point_matrix[,3] %>% sum(), total_point_matrix[,3] %>% max(), total_point_matrix[,3] %>% min(), total_point_matrix[,3] %>% mean() %>% round(digits = 5)),
      "P_4" = c(total_point_matrix[,4] %>% sum(), total_point_matrix[,4] %>% max(), total_point_matrix[,4] %>% min(), total_point_matrix[,4] %>% mean() %>% round(digits = 5)),
      "P_5" = c(total_point_matrix[,5] %>% sum(), total_point_matrix[,5] %>% max(), total_point_matrix[,5] %>% min(), total_point_matrix[,5] %>% mean() %>% round(digits = 5)),
      "P_6" = c(total_point_matrix[,6] %>% sum(), total_point_matrix[,6] %>% max(), total_point_matrix[,6] %>% min(), total_point_matrix[,6] %>% mean() %>% round(digits = 5)),
      "P_7" = c(total_point_matrix[,7] %>% sum(), total_point_matrix[,7] %>% max(), total_point_matrix[,7] %>% min(), total_point_matrix[,7] %>% mean() %>% round(digits = 5)),
      "P_8" = c(total_point_matrix[,8] %>% sum(), total_point_matrix[,8] %>% max(), total_point_matrix[,8] %>% min(), total_point_matrix[,8] %>% mean() %>% round(digits = 5)),
      "P_9" = c(total_point_matrix[,9] %>% sum(), total_point_matrix[,9] %>% max(), total_point_matrix[,9] %>% min(), total_point_matrix[,9] %>% mean() %>% round(digits = 5)),
      "P_10" = c(total_point_matrix[,10] %>% sum(), total_point_matrix[,10] %>% max(), total_point_matrix[,10] %>% min(), total_point_matrix[,10] %>% mean() %>% round(digits = 5)),
      stringsAsFactors = FALSE
    )
    rownames(polygon_point_in_area) <- c("Total number", "Max number in one polygon", "Min number in one polygon", "Mean number in one polygon")
    
    total_area <- sapply(Polygons_data_current_on_map$list[[current_polygon_class]], function(x){
      x %>% geojson_lonlat2xy_function() %>% xy_geojson_area_function()
    }) 
    
    if (input$Calculation_geojson_unit_selection == "mm") {
      if (input$Slide_or_image == "Slide") {
        polygon_true_area <- total_area/1000000000000 * pixel_length * pixel_length
        for (i in 1:length(polygon_true_area)) {
          total_point_matrix[i,] <- total_point_matrix[i,]/polygon_true_area[i]
        }
        polygon_point_in_area[5,] <- round(polygon_point_in_area[1,]/(polygon_true_area %>% sum()), digits = 5)
        polygon_point_in_area[6,] <- sapply(1:10, function(x){
          max(total_point_matrix[,x]) %>% round(digits = 5)
        })
        polygon_point_in_area[7,] <- sapply(1:10, function(x){
          min(total_point_matrix[,x]) %>% round(digits = 5)
        })
        rownames(polygon_point_in_area)[5:7] <- c("Average density (Number/mm2)", "Max density in one polygon (Number/mm2)", "Min density in one polygon (Number/mm2)")
      }
    } else {
      for (i in 1:length(total_area)) {
        total_point_matrix[i,] <- total_point_matrix[i,]/total_area[i] * 1000000
      }
      polygon_point_in_area[5,] <- round(polygon_point_in_area[1,]/(total_area %>% sum()) * 1000000, digits = 5)
      polygon_point_in_area[6,] <- sapply(1:10, function(x){
        max(total_point_matrix[,x]) %>% round(digits = 5)
      })
      polygon_point_in_area[7,] <- sapply(1:10, function(x){
        min(total_point_matrix[,x]) %>% round(digits = 5)
      })
      rownames(polygon_point_in_area)[5:7] <- c("Average density (Number/1000000 pixel)", "Max density in one polygon (Number/1000000 pixel)", "Min density in one polygon (Number/1000000 pixel)")
      
    }
  }
  polygon_point_in_area
}, options = list(lengthChange = FALSE))
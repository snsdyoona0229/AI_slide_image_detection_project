observeEvent(input$start_print_screen, {
  showModal(modalDialog(
    title = "Download image setting!",
    selectInput("Download_img_format", label = "Choose format", choices = c("jpg", "png", "tiff")),
    downloadBttn("Start_download_img", label = "Start download")
  ))
})

output$Start_download_img <- downloadHandler(
  filename = function() { 
    paste0("export_img.", input$Download_img_format) 
  },
  content = function(file) {
    withProgress(message = 'Export data', value = 0, {
      # Number of steps
      n <- 3
      
      incProgress(1/n, detail = "Pre checks and get data")
      
      # checks if inputs for get_data are well defined
      
      zoom_level <- input$start_print_screen[1] %>% floor()
      text_file_name <- paste0(input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input)
      slide_name <- list.files(paste0("www/", text_file_name), pattern = ".ndpi", full.names = TRUE)
      if (zoom_level > 12) {
        zoom_level <- 12
      } 
      center_point <- LonLat2XY(lon_deg = input$start_print_screen[3], lat_deg = input$start_print_screen[2], zoom = 13) %>% unlist()
      bound_1 <- LonLat2XY(lon_deg = input$start_print_screen[5], lat_deg = input$start_print_screen[4], zoom = zoom_level) %>% unlist()
      bound_2 <- LonLat2XY(lon_deg = input$start_print_screen[7], lat_deg = input$start_print_screen[6], zoom = zoom_level) %>% unlist()
      
      x_max <- max(bound_1[1], bound_2[1]) - min(bound_1[1], bound_2[1])
      y_max <- max(bound_1[2], bound_2[2]) - min(bound_1[2], bound_2[2])
      
      max_xy <- max(x_max, y_max)
      level <- 2^(log2(x_max * 2) %>% ceiling())
      img_array <- NDPI_center_RGB_array_Num(file_name = slide_name, x_pixel = center_point[1], y_pixel = center_point[2], z = zoom_level, tilesize = level)
      incProgress(1/n, detail = "Post Proces and check")
      incProgress(1/n, detail = "generate flatfile")
      img_array <- img_array[(level/2 - x_max):(level/2 + x_max),(level/2 - y_max):(level/2 + y_max),]
      writeImage(rgbImage(img_array[,,1], img_array[,,2], img_array[,,3]), file)
    })
  }
)
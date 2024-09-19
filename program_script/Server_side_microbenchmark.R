observeEvent(input$Start_system_analysis, {
  
  progressSweetAlert(
    session = session, 
    id = "system_analysis_file_myprogress",
    title = "GPU analysis",
    display_pct = TRUE, value = 10
  )
  
  GPU_info <- tf$config$experimental$get_device_details(tf$config$list_physical_devices('GPU')[[1]])
  
  updateProgressBar(
    session = session,
    id = "system_analysis_file_myprogress",
    title = "CPU analysis",
    value = 80
  )
  
  CPU_info <- benchmarkme::get_cpu()
  
  updateProgressBar(
    session = session,
    id = "system_analysis_file_myprogress",
    title = "Ram analysis",
    value = 95
  )
  
  Ram_size <- memory.limit()/1024
  
  closeSweetAlert(session = session)
  
  showModal(modalDialog(
    title = "Current system information",
    size = "l",
    column(12,
           column(6,
                  p(paste0("GPU type: ", GPU_info$device_name)),
                  p(paste0("GPU memory (GB): ", GPU_info$compute_capability %>% unlist() %>% paste0(collapse = " and "))),
                  p(paste0("Ram memory (GB): ", Ram_size))
           ),
           column(6,
                  p(paste0("CPU vendor: ", CPU_info$vendor_id)),
                  p(paste0("CPU type: ", CPU_info$model_name)),
                  p(paste0("CPU core number: ", CPU_info$no_of_cores))
           )
    )
  ))
})


observeEvent(input$Microbenchmark_slide_group, {
  
  slide_case <- list.files(paste0("www/Slide/", input$Microbenchmark_slide_group))
  slide_case <- slide_case[str_detect(slide_case, ".rds", negate = TRUE)]
  
  updateSelectInput(session = session, 
                    inputId = "Microbenchmark_slide_use", 
                    choices = slide_case)
})


observeEvent(input$Start_tile_size_and_cpu_batch_size_analysis, {
  
  progressSweetAlert(
    session = session, 
    id = "tile_size_and_cpu_batch_size_file_myprogress",
    title = "Initiate slide and environment",
    display_pct = TRUE, value = 10
  )
  
  slide_location <- paste0("www/Slide/", input$Microbenchmark_slide_group, "/", input$Microbenchmark_slide_use)
  slide_name <- list.files(slide_location, pattern = ".ndpi", full.names = TRUE)
  try_batch <- input$Microbenchmark_slide_batch_size %>% as.integer()
  try_tile_size <- input$Microbenchmark_slide_tile_size %>% as.integer()
  try_times <- input$Microbenchmark_slide_micro_times %>% as.integer()
  
  present_df <- data.frame(
    batch_size = rep(NA, length(try_batch) * length(try_tile_size)),
    tile_size = rep(NA, length(try_batch) * length(try_tile_size)),
    median_time = rep(NA, length(try_batch) * length(try_tile_size)),
    standard_score = rep(NA, length(try_batch) * length(try_tile_size))
  )
  
  count <- 0
  for (i in 1:length(try_tile_size)) {
    for (j in 1:length(try_batch)) {
      count <- count + 1
      present_df$batch_size[count] <- try_batch[j]
      present_df$tile_size[count] <- try_tile_size[i]
      
      updateProgressBar(
        session = session,
        id = "tile_size_and_cpu_batch_size_file_myprogress",
        title = paste0("Current tile: ", try_tile_size[i], "; Current batch: ", try_batch[j]),
        value = 10 + 80/nrow(present_df)*count
      )
      
      microbench <- microbenchmark(
        lapply(1:try_batch[j], function(x) {
          NDPI_center_RGB_batch_Num(file_name = slide_name,
                                    all_x_pixel = sample(1:100000, 1),
                                    all_y_pixel = sample(1:100000, 1),
                                    all_pyramid_layer = 12,
                                    tilesize = try_tile_size[i])
        }), times = try_times
      )
      
      present_df$median_time[count] <- microbench$time %>% median()
      present_df$standard_score[count] <- present_df$median_time[count]/present_df$batch_size[count]/present_df$tile_size[count]/present_df$tile_size[count]
    }
  }
  
  updateProgressBar(
    session = session,
    id = "tile_size_and_cpu_batch_size_file_myprogress",
    title = "Print result",
    value = 100
  )
  
  output$CPU_analysis <- renderTable(present_df)
  
  closeSweetAlert(session = session)
    
  showModal(modalDialog(
    title = "Current CPU analysis",
    size = "l",
    column(12,
           tableOutput('CPU_analysis')
    )
  ))
  
  #print(present_df)

})
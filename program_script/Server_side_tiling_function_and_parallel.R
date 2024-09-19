observeEvent(input$Tiling_group, {
  slide_file <- list.files(paste0("www/Slide/", input$Tiling_group))
  slide_file <- slide_file[str_detect(slide_file, ".rds", negate = TRUE)]
  updateSelectInput(session = session, inputId = "Tiling_group", choices = list.files("www/Slide"), selected = input$Tiling_group)
  updateSelectInput(session = session, inputId = "Tiling_slide", choices = slide_file)
})

observeEvent(input$Tiling_select_all_slide, {
  slide_file <- list.files(paste0("www/Slide/", input$Tiling_group))
  slide_file <- slide_file[str_detect(slide_file, ".rds", negate = TRUE)]
  updateSelectInput(session = session, inputId = "Tiling_slide", choices = slide_file, selected = slide_file)
})

observeEvent(input$Tiling_remove_all_slide, {
  updateSelectInput(session = session, inputId = "Tiling_slide", selected = "")
})

observeEvent(input$Tiling_start, {
  if (length(input$Tiling_slide[1]) == 0) {
    sendSweetAlert(
      session = session,
      title = "No slide is selected !",
      type = "error"
    )
  } else {
    progressSweetAlert(
      session = session, id = "generate_tiling_jpg",
      title = "Start parallel clustering",
      display_pct = TRUE, value = 0
    )
    
    
    all_slide <- input$Tiling_slide
    all_slide_location <- paste0("www/Slide/", input$Tiling_group, "/", all_slide)
    total_slide_number <- length(all_slide_location)
    for (i in 1:total_slide_number) {
      updateProgressBar(
        session = session,
        id = "generate_tiling_jpg",
        title = paste0("Tiling slide: ", all_slide[i]),
        value = 10 + (i-1)/(total_slide_number) * 90
      )
      
      slide_name <- list.files(all_slide_location[i], pattern = ".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif", full.names = TRUE)
      slide_info <- readRDS(paste0(all_slide_location[i], "/info/info.rds"))
      
      run_list <- list()
      
      if (str_detect(slide_name[1], ".ndpi")) {
        #z 5-12
        for (z in 12:5) {
          x_size <- slide_info$image_size[1]/(1024*(2^(12-z)))
          y_size <- slide_info$image_size[2]/(1024*(2^(12-z)))
          for (x in 0:x_size) {
            for (y in 0:y_size) {
              run_list[[paste0(x, "-", y, "-", z)]] <- c(slide_name, paste0(all_slide_location[i], "/temp/", x, "_", y, "_", z, ".jpg"), x, y, z)
            }
          }
        }
        
        cpu_batch_size <- as.integer(input$Tiling_CPU_batch_size)
        
        #saveRDS(run_list, "run_list.rds")
        
        split_list <- list()
        for (k in 0:(length(run_list) - 1)) {
          if (k %% cpu_batch_size == 0) {
            split_list[[floor(k/cpu_batch_size) + 1]] <- list()
            split_list[[floor(k/cpu_batch_size) + 1]][[1]] <- run_list[[k + 1]][1]
            split_list[[floor(k/cpu_batch_size) + 1]][[2]] <- run_list[[k + 1]][2]
            split_list[[floor(k/cpu_batch_size) + 1]][[3]] <- run_list[[k + 1]][3] %>% as.integer()
            split_list[[floor(k/cpu_batch_size) + 1]][[4]] <- run_list[[k + 1]][4] %>% as.integer()
            split_list[[floor(k/cpu_batch_size) + 1]][[5]] <- run_list[[k + 1]][5] %>% as.integer()
          } else {
            split_list[[floor(k/cpu_batch_size) + 1]][[2]][k %% cpu_batch_size + 1] <- run_list[[k + 1]][2]
            split_list[[floor(k/cpu_batch_size) + 1]][[3]][k %% cpu_batch_size + 1] <- run_list[[k + 1]][3] %>% as.integer()
            split_list[[floor(k/cpu_batch_size) + 1]][[4]][k %% cpu_batch_size + 1] <- run_list[[k + 1]][4] %>% as.integer()
            split_list[[floor(k/cpu_batch_size) + 1]][[5]][k %% cpu_batch_size + 1] <- run_list[[k + 1]][5] %>% as.integer()
          }
        }
        
        future_lapply(X = split_list, function(tiling){
          NDPI_loop_save_jpg(file_name = tiling[[1]],
                             output_name = tiling[[2]],
                             x = tiling[[3]],
                             y = tiling[[4]],
                             z = tiling[[5]],
                             tilesize = 1024)
        }, future.seed = FALSE)
        
        #future_lapply(X = run_list, function(tiling){
        #  NDPI_save_jpg(file_name = tiling[1],
        #                output_name = tiling[2], 
        #                x = as.integer(tiling[3]),
        #                y = as.integer(tiling[4]),
        #                z = as.integer(tiling[5]),
        #                tilesize = 1024)
        #})
        
      } else if (str_detect(slide_name[1], ".svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif")) {
        
        current_slide_in_openslide <- openslide$OpenSlide(filename = slide_name)
        
        openslide_generator <- openslide$deepzoom$DeepZoomGenerator(osr = current_slide_in_openslide,
                                                                    tile_size = as.integer(1024), 
                                                                    overlap = as.integer(0))
        
        level_tile_count <- openslide_generator$level_tiles %>% unlist() %>% matrix(nrow = 2) %>% transpose()
        level_calculate <- nrow(level_tile_count) - 13
        
        for (z in 12:5) {
          x_size <- slide_info$image_size[1]/(1024*(2^(12-z)))
          y_size <- slide_info$image_size[2]/(1024*(2^(12-z)))
          for (x in 0:x_size) {
            for (y in 0:y_size) {
              run_list[[paste0(x, "-", y, "-", z)]] <- c(slide_name, paste0(all_slide_location[i], "/temp/", x, "_", y, "_", z, ".jpg"), x, y, z + level_calculate)
            }
          }
        }
        
        cpu_batch_size <- as.integer(input$Tiling_CPU_batch_size)
        
        split_list <- list()
        for (k in 0:(length(run_list) - 1)) {
          if (k %% cpu_batch_size == 0) {
            split_list[[floor(k/cpu_batch_size) + 1]] <- list()
          }
          split_list[[floor(k/cpu_batch_size) + 1]][[k %% cpu_batch_size + 1]] <- run_list[[k + 1]]
        }
        
        future_lapply(X = split_list, function(tiling_list){
          Sys.setenv("PATH" = paste0("compile_program_callback\\openslide-win64-20171122\\bin;", Sys.getenv("PATH")))
          openslide <- import("openslide")
          BASE64 <- import("base64")
          PIL <- import("PIL")
          
          for (k in 1:length(tiling_list)) {
            blank_image <- PIL$Image$new("RGB", size = c(1024, 1024) %>% as.integer())
            current_slide_in_openslide_par <- openslide$OpenSlide(filename = tiling_list[[k]][1])
            
            openslide_generator_par <- openslide$deepzoom$DeepZoomGenerator(osr = current_slide_in_openslide_par,
                                                                            tile_size = as.integer(1024), 
                                                                            overlap = as.integer(0))
            
            crop_image <- openslide_generator_par$get_tile(level = tiling_list[[k]][5] %>% as.integer(), 
                                                           address = c(tiling_list[[k]][3], tiling_list[[k]][4]) %>% as.integer())
            
            if (crop_image$size %>% unlist() %>% sum() == 2048) {
              crop_image$save(tiling_list[[k]][2], format="JPEG")
            } else {
              blank_image$paste(im = crop_image)
              blank_image$save(tiling_list[[k]][2], format="JPEG")
            }
          }
          current_slide_in_openslide_par$close()
        }, future.seed = FALSE)
      }
    }
    
    updateProgressBar(
      session = session,
      id = "generate_tiling_jpg",
      title = "Stop clusting core",
      value = 100
    )
    
    updateSelectInput(session = session, inputId = "Tiling_slide", selected = "")
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "Tiling complete !",
      type = "success"
    )
  }
})
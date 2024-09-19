Fast_slide_process <- reactiveValues(df = data.frame())

shinyFileChoose(input = input, 
                id = 'Fast_slide_proces_files', 
                roots = getVolumes()(), 
                filetypes=c('tiff', 
                            "ndpi", 
                            "isyntax",
                            "svs",
                            "tif",
                            "vms",
                            "vmu",
                            "scn",
                            "mrxs",
                            "tiff",
                            "svslide",
                            "bif"))

observeEvent(input$Fast_slide_proces_files, {
  if (input$Fast_slide_proces_files[1] %>% class() == "list") {
    root_name <- getVolumes()()[input$Fast_slide_proces_files$root] %>% str_replace("\\/", "")
    choose_file <- sapply(input$Fast_slide_proces_files$files, function(x){
      paste0(root_name, unlist(x) %>% paste0(collapse = "/"))
    })
    
    Fast_slide_process$df <- data.frame("Slide" = choose_file,
                                        "Processing_or_not" = TRUE,
                                        "New_Name" = "",
                                        "Group_of_Slide" = "",
                                        stringsAsFactors = FALSE)
  }
})

observeEvent(input$Fast_process_type, {
  if (input$Fast_process_type == 'From fast process folder') {
    if (list.files("slide_upload_fast_process/", pattern = ".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax") %>% length() > 0) {
      Fast_slide_process$df <- data.frame("Slide" = paste0(getwd(), "/slide_upload_fast_process/", list.files("slide_upload_fast_process/", pattern = ".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax")),
                                          "Processing_or_not" = TRUE,
                                          "New_Name" = "",
                                          "Group_of_Slide" = "",
                                          stringsAsFactors = FALSE)
    } else {
      Fast_slide_process$df <- data.frame()
    }
  } else if (input$Fast_process_type == 'Direct select file') {
    Fast_slide_process$df <- data.frame()
  }
})




output$Fast_slide_file_table <- renderRHandsontable({
  if (nrow(Fast_slide_process$df) > 0) {
    rhandsontable(Fast_slide_process$df) %>% 
      hot_col(col = "Group_of_Slide", type = "dropdown", source = list.files("www/Slide/")) %>% 
      hot_col("Slide", readOnly = TRUE)
  }
})

observeEvent(input$Fast_slide_process_start, {
  if (Fast_slide_process$df %>% nrow() > 0) {
    progressSweetAlert(
      session = session, id = "start_slide_process_into_location",
      title = "Start processing!",
      display_pct = TRUE, value = 0
    )
    original_table <- as.data.frame(hot_to_r(input$Fast_slide_file_table))
    use_table <- original_table %>% filter(Processing_or_not) %>% filter(Group_of_Slide != "")
    if (nrow(use_table) > 0) {
      for (i in 1:nrow(use_table)) {
        updateProgressBar(
          session = session,
          id = "start_slide_process_into_location",
          title = paste0("Now processing: ", use_table$Slide[i]),
          value = 10 + 90/nrow(use_table) * (i-1)
        )
        if (use_table$New_Name[i] != "") {
          file_name_no_ndpi <- use_table$New_Name[i] %>% str_replace_all(".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax", "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-")
          current_name <- list.files(paste0("www/Slide/", use_table$Group_of_Slide[i]))
          if (file_name_no_ndpi %in% current_name) {
            time_name <- Sys.time() %>% as.character() %>% str_replace_all(" ", "_") %>% str_replace_all(":", "-")
            file_name_no_ndpi <- paste0(time_name, "_", time_name)
          }
        } else {
          file_name_no_ndpi <- use_table$Slide[i] %>% basename() %>% str_replace_all(".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax", "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-")
          current_name <- list.files(paste0("www/Slide/", use_table$Group_of_Slide[i]))
          if (file_name_no_ndpi %in% current_name) {
            time_name <- Sys.time() %>% as.character() %>% str_replace_all(" ", "_") %>% str_replace_all(":", "-")
            file_name_no_ndpi <- paste0(time_name, "_", time_name)
          }
        }
        
        if (str_detect(use_table$Slide[i], ".ndpi")) {
          output_list <- NDPI_info(file_name = use_table$Slide[i])
          image_size <- c(output_list$Width_pixel, output_list$Height_pixel)
          source_lens <- paste0(output_list$Lens, "x")
          output_list[["image_size"]] <- image_size
          output_list[["source_lens"]] <- source_lens
          
          dir.create(paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi))
          dir.create(paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/info"))
          dir.create(paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/temp"))
          file_name <- paste0(file_name_no_ndpi, "___", image_size[1], "___", image_size[2], "___", source_lens, ".ndpi")
          
          if (input$Fast_process_select_file_process_type == "Move") {
            file.rename(paste0(use_table$Slide[i]), 
                        paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/", file_name))
          } else if (input$Fast_process_select_file_process_type == "Copy") {
            file.copy(paste0(use_table$Slide[i]), 
                      paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/", file_name))
          }
          
          saveRDS(output_list, paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/info/info.rds"))
          
          NDPI_save_slide_preview(file_name = paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/", file_name),
                                  output_name = paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/info/SlideImage.jpg"))
          
          slide_info <- readRDS(paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/info/info.rds"))
          
          run_list <- list()
          
          #z 5-12
          for (z in 5:2) {
            x_size <- slide_info$image_size[1]/(256*(2^(12-z)))
            y_size <- slide_info$image_size[2]/(256*(2^(12-z)))
            for (x in 0:x_size) {
              for (y in 0:y_size) {
                run_list[[paste0(x, "-", y, "-", z)]] <- c(paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/", file_name), 
                                                           paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/temp/", x, "_", y, "_", z, "minimap.jpg"), 
                                                           x, 
                                                           y, 
                                                           z)
              }
            }
          }
          
          
          sapply(run_list, function(tiling){
            NDPI_save_jpg(file_name = tiling[1], 
                          output_name = tiling[2],
                          x = as.integer(tiling[3]),
                          y = as.integer(tiling[4]),
                          z = as.integer(tiling[5]),
                          tilesize = 256)
          })
        } else if (str_detect(use_table$Slide[i], ".svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif")) {
          
          current_format <- str_extract(use_table$Slide[i], ".svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif")
          
          output_list <- list(
            "Width_pixel" = NA,
            "Height_pixel" = NA,
            "Physicalx_center" = NA,
            "Physicaly_cente" = NA,
            "Physicalwidth_nm" = NA,
            "Physicalheight_nm" = NA,
            "Lens" = 40 %>% as.integer(),             
            "Number_of_z_layers" = 1 %>% as.integer(),
            "Number_of_channels" = 3 %>% as.integer(),
            "Bits_per_channel" = 8 %>% as.integer(),
            "image_size" = NA,
            "source_lens" = "40x"
          )
          
          opened_slide <- openslide$OpenSlide(use_table$Slide[i])
          tile_generator <- openslide$deepzoom$DeepZoomGenerator(osr = opened_slide,
                                                                 tile_size = as.integer(256), 
                                                                 overlap = as.integer(0))
          tile_table <- tile_generator$level_dimensions %>% unlist() %>% matrix(nrow = 2) %>% transpose()
          output_list$image_size <- tile_table[nrow(tile_table),] %>% as.integer()
          output_list$Width_pixel <- tile_table[nrow(tile_table),1] %>% as.integer()
          output_list$Height_pixel <- tile_table[nrow(tile_table),2] %>% as.integer()
          
          dir.create(paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi))
          dir.create(paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/info"))
          dir.create(paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/temp"))
          file_name <- paste0(file_name_no_ndpi, "___", output_list$image_size[1], "___", output_list$image_size[2], "___", output_list$source_lens, current_format)
          saveRDS(output_list, paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/info/info.rds"))
          
          try(
            opened_slide$associated_images['macro']$convert(mode = "RGB")$save(paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/info/SlideImage.jpg"), format="JPEG"),
            silent = TRUE
          )
          
          
          
          tile_table_sapply <- tile_generator$level_tiles %>% unlist() %>% matrix(nrow = 2) %>% transpose()
          last_z_level <- nrow(tile_table_sapply) - 7
          
          run_list <- list()
          
          for (z in last_z_level:(last_z_level-3)) {
            for (x in 1:tile_table_sapply[z,1]) {
              for (y in 1:tile_table_sapply[z,2]) {
                run_list[[paste0(x, "-", y, "-", z)]] <- c(paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/", file_name), 
                                                           paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/temp/", x - 1, "_", y - 1, "_", 5 - last_z_level + z, "minimap.jpg"), 
                                                           x - 1, 
                                                           y - 1, 
                                                           z - 1)
              }
            }
          }
          
          #print(run_list)
          
          sapply(run_list, function(tiling){
            blank_image <- PIL$Image$new("RGB", size = c(256,256) %>% as.integer())
            tile_data <- tile_generator$get_tile(level = as.integer(tiling[5]),
                                                 address = as.integer(c(tiling[3], tiling[4])))
            
            blank_image$paste(im = tile_data)
            blank_image$save(tiling[2], format="JPEG")
          })
          
          if (input$Fast_process_select_file_process_type == "Move") {
            file.rename(use_table$Slide[i], 
                        paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/", file_name))
            
            if (str_detect(use_table$Slide[i], ".mrxs")) {
              folder_name_mrxs <- str_replace_all(use_table$Slide[i], ".mrxs", "")
              file_name_mrxs <- str_replace_all(file_name, ".mrxs", "")
              file.rename(folder_name_mrxs, 
                          paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/", file_name_mrxs))
            }
          } else if (input$Fast_process_select_file_process_type == "Copy") {
            file.copy(use_table$Slide[i], 
                      paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/", file_name))
            if (str_detect(use_table$Slide[i], ".mrxs")) {
              folder_name_mrxs <- str_replace_all(use_table$Slide[i], ".mrxs", "")
              file_name_mrxs <- str_replace_all(file_name, ".mrxs", "")
              dir.create(paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/", file_name_mrxs))
              file.copy(folder_name_mrxs, 
                        paste0("www/Slide/", use_table$Group_of_Slide[i], "/", file_name_no_ndpi, "/", file_name_mrxs), 
                        recursive = TRUE)
            }
          }
        }
      }
    }
    updateProgressBar(
      session = session,
      id = "start_slide_process_into_location",
      title = "Table renewing!",
      value = 100
    )
    
    if (input$Fast_process_type == 'From fast process folder') {
      if (list.files("slide_upload_fast_process/", pattern = ".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax") %>% length() > 0) {
        Fast_slide_process$df <- data.frame("Slide" = paste0(getwd(), "/slide_upload_fast_process/", list.files("slide_upload_fast_process/", pattern = ".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax")),
                                            "Processing_or_not" = TRUE,
                                            "New_Name" = "",
                                            "Group_of_Slide" = "",
                                            stringsAsFactors = FALSE)
      } else {
        Fast_slide_process$df <- data.frame()
      }
    } else if (input$Fast_process_type == 'Direct select file') {
      Fast_slide_process$df <- data.frame()
    }
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "Processing complete!!",
      text = "The slides are all in right position and ready to use!",
      type = "success"
    )
  }
})
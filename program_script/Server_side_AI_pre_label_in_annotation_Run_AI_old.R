output$model_slide_image_condition_Run_AI <- renderText({
  paste("Model is trained by: ", AI_pre_label$model_slide_image_condition_Run_AI, sep="\n")
  
})

output$AI_pre_label_slide_input_type_Run_AI <- renderText({
 paste("Model training type: ", AI_pre_label$slide_input_type, sep="\n")
  
})

output$AI_pre_label_slide_input_layer_Run_AI <- renderText({
 paste("Model use layer: ", paste0(AI_pre_label$slide_input_layer, collapse = ", "), sep="\n")
  
})


observeEvent(input$AI_pre_label_model_group_Run_AI, {
  updateSelectInput(session, inputId = "AI_pre_label_model_Run_AI", choices = list.files(paste0("model/", input$AI_pre_label_model_group_Run_AI)))

})


observeEvent(input$select_all_generative_file_Run_AI, {
  updateMultiInput(session, inputId = "Choose_file_training_table_Run_AI", selected = list.files(paste0("www/", input$Slide_or_image, "/", input$Annotation_group)) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE))
})

observeEvent(input$delete_all_generative_file_Run_AI, {
  updateMultiInput(session, inputId = "Choose_file_training_table_Run_AI", selected = character(0))
})


observeEvent(input$AI_pre_label_model_Run_AI, {

reticulate::py_run_string("import tensorflow as tf")
reticulate::py_run_string("from tensorflow import keras")
reticulate::py_run_string("from keras import backend as K")
reticulate::py_run_string("from keras.backend import set_session")
reticulate::py_run_string("from accelerate import Accelerator")
reticulate::py_run_string("from keras.backend import clear_session")
reticulate::py_run_string("from keras.backend import get_session")
reticulate::py_run_string("from numba import cuda")
reticulate::py_run_string("import torch")
reticulate::py_run_string("import gc")
reticulate::py_run_string("import os")

 
  if (file.exists(paste0("model/", input$AI_pre_label_model_group_Run_AI, "/", input$AI_pre_label_model_Run_AI, "/detail.rds"))) {
    model_detail <- readRDS(paste0("model/", input$AI_pre_label_model_group_Run_AI, "/", input$AI_pre_label_model_Run_AI, "/detail.rds"))
    
    if (model_detail$image_or_slide == "Slide") {
      if (model_detail$slide_input_type == "Split train") {
        updateSelectizeInput(session, 
                             inputId = "AI_pre_label_in_slide_layer_Run_AI", 
                             choices = model_detail$slide_input_layer,
                             selected = model_detail$slide_input_layer[1],
                             options = list(maxItems = 1))
      } else {
        updateSelectizeInput(session, 
                             inputId = "AI_pre_label_in_slide_layer_Run_AI", 
                             choices = model_detail$slide_input_layer,
                             selected = model_detail$slide_input_layer,
                             options = list(maxItems = length(model_detail$slide_input_layer)))
      }
    } else {
      updateSelectizeInput(session, 
                           inputId = "AI_pre_label_in_slide_layer_Run_AI", 
                           choices = c("40x", 
                                       "20x",
                                       "10x",
                                       "5x",
                                       "2.5x",
                                       "1.25x",
                                       "0.625x",
                                       "0.3125x",
                                       "0.15625x",
                                       "0.078125x"),
                           selected = "40x",
                           options = list(maxItems = 1))
    }
    
    split_word <- c(model_detail$layer_class_id_P, model_detail$layer_class_id_G) %>% str_split("_")
    
    Primary_layer_ID <- sapply(split_word, function(x){
      x[1] %>% as.integer()
    })
    
    Secondary_layer_ID <- sapply(split_word, function(x){
      x[2] %>% as.integer()
    })
    
    Tertiary_layer_ID <- sapply(split_word, function(x){
      x[3] %>% as.integer()
    })
    
    option_word <- sapply(split_word, function(x){
      if (is.na(x[5])) {
        ""
      } else {
        paste0("_", x[5])
      }
    })
    
	
    layer_number <- length(Primary_layer_ID)
    
    AI_pre_label$model_slide_image_condition_Run_AI <- model_detail$image_or_slide
    AI_pre_label$slide_input_type <- model_detail$slide_input_type
    AI_pre_label$slide_input_layer <- model_detail$slide_input_layer
    
    AI_pre_label$output_table <- AI_pre_label$output_table[0,]
    
    AI_pre_label$output_table[1:layer_number, 1] <- paste0(Primary_layer_ID,
                                                           "_",
                                                           Secondary_layer_ID,
                                                           "_",
                                                           Tertiary_layer_ID,
                                                           "_",
                                                           paste0(model_detail$layer_option, option_word))
    #AI_pre_label$output_table[1:layer_number, 2] <- Secondary_layer_ID
    #AI_pre_label$output_table[1:layer_number, 3] <- Tertiary_layer_ID
    #AI_pre_label$output_table[1:layer_number, 4] <- paste0(model_detail$layer_option, option_word)
    AI_pre_label$output_table[1:layer_number, 2] <- model_detail$layer_meaning
    AI_pre_label$output_table[1:layer_number, 3] <- "Y"
    AI_pre_label$output_table[1:layer_number, 4] <- "R"
    AI_pre_label$output_table[1:layer_number, 5] <- "Y"
    
    for (option_row in 1:layer_number) {
      if (str_detect(model_detail$layer_option[option_row], "P")) {
        AI_pre_label$output_table[option_row, 6] <- "-"
        AI_pre_label$output_table[option_row, 7] <- paste0(Primary_layer_ID[option_row],
                                                            "_",
                                                            Secondary_layer_ID[option_row],
                                                            "_",
                                                            Tertiary_layer_ID[option_row],
                                                            "_P_",
                                                            option_word[option_row] %>% str_replace_all("_", ""))
        AI_pre_label$output_table[option_row, 8] <- "-"
      } else if (paste0(model_detail$layer_option[option_row], option_word[option_row]) == "G") {
        AI_pre_label$output_table[option_row, 6] <- paste0(Primary_layer_ID[option_row],
                                                           "_",
                                                           Secondary_layer_ID[option_row],
                                                           "_",
                                                           Tertiary_layer_ID[option_row],
                                                           "_G_",
                                                           1)
        AI_pre_label$output_table[option_row, 7] <- "-"
        AI_pre_label$output_table[option_row, 8] <- paste0(Primary_layer_ID[option_row],
                                                            "_",
                                                            Secondary_layer_ID[option_row],
                                                            "_",
                                                            Tertiary_layer_ID[option_row],
                                                            "_H_",
                                                            1)
      } else if (str_detect(model_detail$layer_option[option_row], "G")) {
        AI_pre_label$output_table[option_row, 6] <- paste0(Primary_layer_ID[option_row],
                                                           "_",
                                                           Secondary_layer_ID[option_row],
                                                           "_",
                                                           Tertiary_layer_ID[option_row],
                                                           "_G_",
                                                           option_word[option_row] %>% str_replace_all("_", ""))
        AI_pre_label$output_table[option_row, 7] <- "-" 
        AI_pre_label$output_table[option_row, 8] <- paste0(Primary_layer_ID[option_row],
                                                            "_",
                                                            Secondary_layer_ID[option_row],
                                                            "_",
                                                            Tertiary_layer_ID[option_row],
                                                            "_H_",
                                                            option_word[option_row] %>% str_replace_all("_", ""))
      }
    }
    AI_pre_label$output_table[, 9] <- 0.6
    AI_pre_label$output_table[, 10] <- 0 
    AI_pre_label$output_table[, 11] <- "-"
    AI_pre_label$output_table[, 12] <- "0.00005"
  } else {
    AI_pre_label$output_table[["Original_layer"]] <- ""
    AI_pre_label$output_table[["Layer_meaning"]] <- ""
    AI_pre_label$output_table[["Use_in_label"]] <- "N"
    AI_pre_label$output_table[["Replace_Add_Clip"]] <- "R"
    AI_pre_label$output_table[["Fill_hole"]] <- "Y"
    AI_pre_label$output_table[["Inference_G"]] <- "-"
    AI_pre_label$output_table[["Inference_P"]] <- "-"
    AI_pre_label$output_table[["Inference_H"]] <- "-"
    AI_pre_label$output_table[["Threshold"]] <- 0.6
    AI_pre_label$output_table[["Ignore_pixel_min"]] <- 0
    AI_pre_label$output_table[["Ignore_pixel_max"]] <- "-"
    AI_pre_label$output_table[["Polygon_simplify"]] <- "0.00005"
  }
})

AI_pre_label <- reactiveValues(model = "",
                               tissue_model = "",
                               model_name = "",
                               model_type = "",
                               model_detail = "",
                               model_slide_image_condition_Run_AI = "",
                               output_table = data.frame(#"Primary_layer_ID" = "",
                                                         #"Secondary_layer_ID" = "",
                                                         #"Tertiary_layer_ID" = "",
                                                         #"layer_option" = "",
                                                         "Original_layer" = "",
                                                         "Layer_meaning" = "",
                                                         "Use_in_label" = "Y",
                                                         "Replace_Add_Clip" = "R",
                                                         "Fill_hole" = "Y",
                                                         "Inference_G" = "-",
                                                         "Inference_P" = "-",
                                                         "Inference_H" = "-",
                                                         "Threshold" = 0.6,
                                                         "Ignore_pixel_min" = 0,
                                                         "Ignore_pixel_max" = "-",
                                                         "Polygon_simplify" = "0.00005",
                                                         stringsAsFactors = FALSE))

output$AI_pre_label_table_Run_AI <- renderRHandsontable(
  rhandsontable(AI_pre_label$output_table) %>% 
    #hot_col(col = "Primary_layer_ID", type = "dropdown", source = 1:20) %>% 
    #hot_col(col = "Secondary_layer_ID", type = "dropdown", source = 1:20) %>% 
    #hot_col(col = "Tertiary_layer_ID", type = "dropdown", source = 1:20) %>% 
    #hot_col(col = "layer_option", type = "dropdown", source = c("G", paste0("P_", 1:10))) %>% 
    hot_col(col = "Use_in_label", type = "dropdown", source = c("Y", "N")) %>% 
    hot_col(col = "Replace_Add_Clip", type = "dropdown", source = c("R", "A", "C")) %>% 
    hot_col(col = "Fill_hole", type = "dropdown", source = c("Y", "N")) %>% 
    #hot_col(col = "Inference_G", type = "dropdown", source = c("-", 1:10)) %>% 
    #hot_col(col = "Inference_P", type = "dropdown", source = c("-", 1:10)) %>% 
    #hot_col(col = "Inference_H", type = "dropdown", source = c("-", 1:10)) %>% 
    hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = 'white';
              td.style.color = 'black';
           }")
)

#########

shinyFileChoose(input = input, 
                id = 'Fast_slide_proces_files_Run_AI_file', 
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
                            "bif")
)


Fast_slide_process_Run_AI <- reactiveValues(df = data.frame())

observeEvent(input$Fast_slide_proces_files_Run_AI_file, {
  

  files_list <- c()

  if (input$Fast_slide_proces_files_Run_AI_file[1] %>% class() == "list") {
    root_name <- getVolumes()()[input$Fast_slide_proces_files_Run_AI_file$root] %>% str_replace("\\/", "")
    choose_file <- sapply(input$Fast_slide_proces_files_Run_AI_file$files, function(x){
      paste0(root_name, unlist(x) %>% paste0(collapse = "/"))
    })
    
	
   for(i in 1:length(choose_file)){  
     files_list[i] <- strsplit(choose_file[i], split = '/')[[1]][length(strsplit(choose_file[i], split = '/')[[1]])] %>% str_replace(" ", "")
	  
	  }
										   
	  Fast_slide_process_Run_AI$df <- data.frame("Slide" = files_list,	                                        
                                                 "new_name" = "NULL",
                                                 "Group_of_Slide" = "RUN_AI",
												 "WSL_class" = input$Select_customuze_AI_to_use,
												 "datapath" = choose_file,
                                                  stringsAsFactors = FALSE)	
										
  }

})
 
 output$Fast_slide_file_table_Run_AI <- renderRHandsontable({
  if (nrow(Fast_slide_process_Run_AI$df) > 0) {
    rhandsontable(Fast_slide_process_Run_AI$df )%>% 
      hot_col(col = "Group_of_Slide", type = "dropdown", source = list.files("www/Slide/")) %>% 
      hot_col("Slide", readOnly = TRUE)
      }
  
  })
########

observeEvent(input$Start_AI_prelabel_Run_AI, {


setting_evn()

img_location_Upload <- c()
model_for_report_list_Upload <- c()
####################################################################################

  upload_data <- as.data.frame(hot_to_r(input$Fast_slide_file_table_Run_AI))
  slide_number <- str_detect(upload_data$Slide,".ndpi|.isyntax|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif") %>% sum()

  if(nrow(upload_data) > 0 ){
 
    if (input$upload_Slide_or_image == "Slide" & nrow(upload_data) != slide_number) {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Your upload files are not all slides which are accepted by our system!",
        type = "error"
      )
    }else{
    
      progressSweetAlert(
        session = session, id = "generate_image_or_slice_file_myprogress",
        title = "Image or Slide preparation",
        display_pct = TRUE, value = 0
      )

      for (i in 1:nrow(upload_data)) {
	  
        time_name <- Sys.time() %>% as.character() %>% str_replace_all(" ", "_") %>% str_replace_all(":", "-")
        file_name <- ""
        exist_name <- list.files(paste0("www/", input$Slide_or_image, "/", input$Annotation_group))

	    if (str_detect(upload_data$Slide[i], ".ndpi")) {
	  
	  
          output_list <- NDPI_info(file_name = upload_data$datapath[i])
          image_size <- c(output_list$Width_pixel, output_list$Height_pixel)
          source_lens <- paste0(output_list$Lens, "x")
          output_list[["image_size"]] <- image_size
          output_list[["source_lens"]] <- source_lens
		

          if (upload_data$new_name[i] == "NULL") {
            file_name <- upload_data$Slide[i]
          } else {
            file_name <- paste0(upload_data$new_name[i],".ndpi")
          }
          file_name_no_ndpi <- file_name %>% str_replace_all(".ndpi", "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-")
          if (file_name_no_ndpi %in% exist_name) {
            file_name_no_ndpi <- paste0(file_name_no_ndpi, "_", time_name,"-")
          }

          dir.create(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi))
          dir.create(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/info"))
          dir.create(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/temp"))
          file_name <- paste0(file_name_no_ndpi, "___", image_size[1], "___", image_size[2], "___", source_lens, ".ndpi")
		
		  img_location_Upload[i]  <- paste0("www/Slide/",input$Annotation_group, "/", file_name_no_ndpi)
		  model_for_report_list_Upload[i] <- paste0(file_name_no_ndpi)
		
          file.copy(paste0(upload_data$datapath[i]), 
                      paste0("www/Slide/", upload_data$Group_of_Slide[i], "/", file_name_no_ndpi, "/", file_name))	
		   
#          file.rename(upload_data$datapath[i], paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/", file_name))
          saveRDS(output_list, paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/info/info.rds"))
        
          NDPI_save_slide_preview(file_name = paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/", file_name),
                                output_name = paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/info/SlideImage.jpg"))
        
          slide_info <- readRDS(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/info/info.rds"))
        
          run_list <- list()
        
        #z 5-12
          for (z in 5:2) {
            x_size <- slide_info$image_size[1]/(256*(2^(12-z)))
            y_size <- slide_info$image_size[2]/(256*(2^(12-z)))
            for (x in 0:x_size) {
              for (y in 0:y_size) {
                run_list[[paste0(x, "-", y, "-", z)]] <- c(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/", file_name), 
                                                         paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/temp/", x, "_", y, "_", z, "minimap.jpg"), 
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
        } else if (str_detect(upload_data$Slide[i], ".svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif")) {
        
          current_format <- str_extract(upload_data$datapath[i], ".svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif")
        
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
        
        opened_slide <- openslide$OpenSlide(upload_data$datapath[i])
        tile_generator <- openslide$deepzoom$DeepZoomGenerator(osr = opened_slide,
                                                               tile_size = as.integer(256), 
                                                               overlap = as.integer(0))
        tile_table <- tile_generator$level_dimensions %>% unlist() %>% matrix(nrow = 2) %>% transpose()
        output_list$image_size <- tile_table[nrow(tile_table),] %>% as.integer()
        output_list$Width_pixel <- tile_table[nrow(tile_table),1] %>% as.integer()
        output_list$Height_pixel <- tile_table[nrow(tile_table),2] %>% as.integer()
        
        
        if (upload_data$new_name[i] == "") {
          file_name <- upload_data$Slide[i]
        } else {
          file_name <- paste0(upload_data$new_name[i], current_format)
        }
        file_name_no_ndpi <- file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-")
        if (file_name_no_ndpi %in% exist_name) {
          file_name_no_ndpi <- paste0(file_name_no_ndpi, "_", time_name)
        }
        
        dir.create(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi))
        dir.create(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/info"))
        dir.create(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/temp"))
		
		img_location_Upload[i]  <- paste0("www/Slide/",input$Annotation_group, "/", file_name_no_ndpi)
		model_for_report_list_Upload[i] <- paste0(file_name_no_ndpi)
        
        saveRDS(output_list, paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/info/info.rds"))
        try(
          opened_slide$associated_images['macro']$convert(mode = "RGB")$save(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/info/SlideImage.jpg"), format="JPEG"),
          silent = TRUE
        )
        
        
        
        tile_table_sapply <- tile_generator$level_tiles %>% unlist() %>% matrix(nrow = 2) %>% transpose()
        last_z_level <- nrow(tile_table_sapply) - 7
        
        run_list <- list()
        
        file_name <- paste0(file_name_no_ndpi, "___", output_list$Width_pixel, "___", output_list$Height_pixel, "___", output_list$source_lens, current_format)
        
        for (z in last_z_level:(last_z_level-3)) {
          for (x in 1:tile_table_sapply[z,1]) {
            for (y in 1:tile_table_sapply[z,2]) {
              run_list[[paste0(x, "-", y, "-", z)]] <- c(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/", file_name), 
                                                         paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/temp/", x - 1, "_", y - 1, "_", 5 - last_z_level + z, "minimap.jpg"), 
                                                         x - 1, 
                                                         y - 1, 
                                                         z - 1)
            }
          }
        }
        
        sapply(run_list, function(tiling){
          blank_image <- PIL$Image$new("RGB", size = c(256,256) %>% as.integer())
          tile_data <- tile_generator$get_tile(level = as.integer(tiling[5]),
                                               address = as.integer(c(tiling[3], tiling[4])))
          
          blank_image$paste(im = tile_data)
          blank_image$save(tiling[2], format="JPEG")
        })
        
        opened_slide$close()
        
        file.rename(upload_data$datapath[i], paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/", file_name))
        

      }
      
      updateProgressBar(
        session = session,
        id = "generate_image_or_slice_file_myprogress",
        value = i * 1/nrow(upload_data) * 100
        )
      
      } 
    
    }
  }
####################################################################################
  
  progressSweetAlert(
    session = session, id = "AI_pre_label_myprogress",
    title = "Start AI labeling",
    display_pct = TRUE, value = 0
  )
  
  if (input$AI_pre_label_train_or_validation_Run_AI == "By Validation Set") {
    model_location <- paste0("model/", input$AI_pre_label_model_group_Run_AI, "/", input$AI_pre_label_model_Run_AI, "/Model.h5")
  } else if (input$AI_pre_label_train_or_validation_Run_AI == "By Training Set") {
    model_location <- paste0("model/", input$AI_pre_label_model_group_Run_AI, "/", input$AI_pre_label_model_Run_AI, "/Model_over.h5")
  }
  
  if (file.exists(model_location)) {
    if (AI_pre_label$model_name != paste0(input$AI_pre_label_model_group_Run_AI, "/", input$AI_pre_label_model_Run_AI) | AI_pre_label$model_type != input$AI_pre_label_train_or_validation_Run_AI) {
      
      updateProgressBar(
        session = session,
        title = "Loading model...",
        id = "AI_pre_label_myprogress",
        value = 10
      )
      
      AI_pre_label$model_name <- paste0(input$AI_pre_label_model_group_Run_AI, "/", input$AI_pre_label_model_Run_AI)
      AI_pre_label$model_type <- input$AI_pre_label_train_or_validation_Run_AI
      AI_pre_label$model_detail <- readRDS(paste0("model/", input$AI_pre_label_model_group_Run_AI, "/", input$AI_pre_label_model_Run_AI, "/detail.rds"))
      
      source(paste0("model/", input$AI_pre_label_model_group_Run_AI, "/", input$AI_pre_label_model_Run_AI, "/run_use.R"), local = TRUE)
      AI_pre_label$model <- load_model_weights_hdf5(object = get_unet(input_shape = list(NULL, NULL, 3 * length(input$AI_pre_label_in_slide_layer_Run_AI)),
                                                                      num_classes = nrow(hot_to_r(input$AI_pre_label_table_Run_AI))), 
                                                    filepath = model_location)
    }
    
	
	train_length <- length(img_location_Upload) +  length(c(input$Choose_file_training_table_Run_AI)) 
	
	
    if (input$Slide_or_image == "Slide" & train_length > 0) {
	
      updateProgressBar(
        session = session,
        title = "Model start predicting...",
        id = "AI_pre_label_myprogress",
        value = 20
      )

	  
	 img_location_history <- c(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", input$Choose_file_training_table_Run_AI, "/"))
	  img_location <- NULL
      model_for_report_list <- NULL
	  model_for_report_list_history <- c(input$Choose_file_training_table_Run_AI)
	  
	  
      count <- 0
	  
	  if(length(img_location_Upload) >0 && length(model_for_report_list_history) == 0){
	         img_location <- img_location_Upload
		     model_for_report_list <- model_for_report_list_Upload
		 }
	  if(length(img_location_Upload) == 0 && length(model_for_report_list_history) > 0){
	     img_location <- img_location_history 
		 model_for_report_list <- model_for_report_list_history
	  }
	  if(length(img_location_Upload) > 0 && length(model_for_report_list_history) > 0){
	          
	     img_location <- union(img_location_history ,img_location_Upload)
		 model_for_report_list <- union(model_for_report_list_history ,model_for_report_list_Upload) 
	  }

	  
     for(i in 1:length(img_location)){
	 
	  keras$backend$clear_session()
      k_clear_session()
	 
	 
      count <- count + 1
	  
	  Points_data_current_on_map$df <- point_file_function(paste0(img_location[1], "/points.rds"))
	  
	  if(file.exists(paste0(img_location[1], "/points.rds"))==FALSE){
	  
	       Points_data_current_on_map$df <- data.frame(point_group="P_1",point_id=1,point_lon=1,point_lat=1,point_primary=1,point_secondary=1,point_tertiary=1)
		}
		
	  image_name <- list.files(img_location[1], pattern = ".ndpi", full.names = TRUE)
      slide_info <- NDPI_info(image_name)
      tile_size <- input$AI_pre_label_in_slide_tile_size_Run_AI %>% as.integer()
      batch_size <- input$AI_pre_label_in_slide_CPU_modify_batch_size_Run_AI %>% as.integer()
      sample_layer <- log2((input$AI_pre_label_in_slide_layer_Run_AI %>% str_replace("x", "") %>% as.numeric())/0.078125) + 3
      check_layer <- max(sample_layer)
      shifting_chunk <- (tile_size * 2^(12 - check_layer))/2
      center_move_pixel <- tile_size * 2^(12 - check_layer)
      tile_x_number <- ceiling(slide_info$Width_pixel/center_move_pixel)
      tile_y_number <- ceiling(slide_info$Height_pixel/center_move_pixel)
      #temp_folder_location <- paste0(img_location[1], "/AI_temp")
	  temp_folder_location <- paste0("www/Slide/RUN_AI/", model_for_report_list[count], "/AI_temp")
      modified_label_table <- hot_to_r(input$AI_pre_label_table_Run_AI)
 
	  
      run_gpu_batch <- input$AI_pre_label_in_slide_batch_size_Run_AI %>% as.integer()
      
      if (file.exists(temp_folder_location) == FALSE) {
        dir.create(temp_folder_location)
      } else {
        rm_file <- list.files(temp_folder_location, full.names = TRUE)
        file.remove(rm_file)
     }
      
      #polygon_threshold <- input$AI_pre_label_polygon_threshold
      #point_threshold <- input$AI_pre_label_point_threshold
      #polygon_simplify <- input$AI_pre_label_polygon_simplify
      overlap_pixel <- input$AI_pre_label_in_slide_overlap_pixel_Run_AI %>% as.integer()
      
      #saveRDS(modified_label_table, "modified_label_table.rds")
      
      future_table <- data.frame(
        center_x = integer(tile_x_number * tile_y_number),
        center_y = integer(tile_x_number * tile_y_number),
        base_layer = integer(tile_x_number * tile_y_number)
      )
      
      #modified_label_table <- hot_to_r(input$AI_pre_label_table_Run_AI)
      
      df_count <- 0
      for (x in 1:tile_x_number) {
        for (y in 1:tile_y_number) {
          df_count <- df_count + 1
          future_table[df_count, "center_x"] <- (x - 1) * center_move_pixel + center_move_pixel/2
          future_table[df_count, "center_y"] <- (y - 1) * center_move_pixel + center_move_pixel/2
          future_table[df_count, "base_layer"] <- check_layer
        }
      }
      
      if (input$Tissue_only_or_All_slide_switch_Run_AI & input$AI_labeled_restricted_area == "No") {
        thumb_image <- NDPI_center_RGB_batch_Num(file_name = image_name,
                                                 all_x_pixel = 1024 * 2^(12 - 5),
                                                 all_y_pixel = 1024 * 2^(12 - 5),
                                                 all_pyramid_layer = 5,
                                                 tilesize = 2048)
        if (AI_pre_label$tissue_model == "") {
          source("store_data_for_program/Tissue_Model.R", local = TRUE)
          AI_pre_label$tissue_model <- load_model_weights_hdf5(object = get_tissue(num_classes = 1), filepath = "store_data_for_program/Tissue_Model.h5")
        }
        thumb_image <- AI_pre_label$tissue_model$predict(thumb_image)[1,,,1]
        
        #saveRDS(thumb_image, "test1.rds")
        
        thumb_image <- replace(thumb_image, which(thumb_image >= input$Tissue_only_threshold_Run_AI), 1) %>% 
          replace(which(thumb_image < input$Tissue_only_threshold_Run_AI), 0) %>% 
          dilate(kern = makeBrush(size = input$Tissue_only_dilate_pixel_Run_AI, shape = "disc"))

        thumb_tile_number <- 2^(check_layer - 5) * 2 * (1024 / tile_size)
        tile_chunk <- untile(thumb_image, c(thumb_tile_number, thumb_tile_number),lwd = 0)
        thumb_image <- sapply(1:(thumb_tile_number^2), function(x){max(tile_chunk[,,x])}) %>% array(c(thumb_tile_number, thumb_tile_number))
        
        thumb_array <- which(thumb_image > 0, arr.ind = TRUE)
        #print(thumb_array)
        thumb_table <- data.frame(
          center_x = (thumb_array[,1] - 1) * center_move_pixel + center_move_pixel/2,
          center_y = (thumb_array[,2] - 1) * center_move_pixel + center_move_pixel/2,
          base_layer = check_layer
        )
        
        future_table <- inner_join(future_table, thumb_table)
      } else if (input$AI_labeled_restricted_area == "Yes only") {
        thumb_image <- thumb_restricted_region_function(slide_file = image_name,
                                                        polygon_layer_select = paste0(input$AI_pre_label_model_restricted_primary_layer_Run_AI, 
                                                                                      "_",
                                                                                      input$AI_pre_label_model_restricted_secondary_layer_Run_AI,
                                                                                      "_",
                                                                                      input$AI_pre_label_model_restricted_tertiary_layer_Run_AI))
        
        for (resize_number in 9:(check_layer - 5)) {
          thumb_image <- thumb_image %>% 
            resize(w = 2^(resize_number) * 2 * (1024 / tile_size),
                   h = 2^(resize_number) * 2 * (1024 / tile_size), filter = "bilinear")
        }
        
        thumb_array <- which(thumb_image > 0, arr.ind = TRUE)
        
        #print(thumb_array)
        
        thumb_table <- data.frame(
          center_x = (thumb_array[,1] - 1) * center_move_pixel + center_move_pixel/2,
          center_y = (thumb_array[,2] - 1) * center_move_pixel + center_move_pixel/2,
          base_layer = check_layer
        )
        
        
        future_table <- inner_join(future_table, thumb_table)
      } else if (input$AI_labeled_restricted_area == "Yes plus tissue") {
        thumb_image1 <- NDPI_center_RGB_batch_Num(file_name = image_name,
                                                  all_x_pixel = 1024 * 2^(12 - 5),
                                                  all_y_pixel = 1024 * 2^(12 - 5),
                                                  all_pyramid_layer = 5,
                                                  tilesize = 2048)
        if (AI_pre_label$tissue_model == "") {
          source("store_data_for_program/Tissue_Model.R", local = TRUE)
          AI_pre_label$tissue_model <- load_model_weights_hdf5(object = get_tissue(num_classes = 1), filepath = "store_data_for_program/Tissue_Model.h5")
        }
        thumb_image1 <- AI_pre_label$tissue_model$predict(thumb_image1)[1,,,1]
        
        #saveRDS(thumb_image1, "test1.rds")
        
        thumb_image1 <- replace(thumb_image1, which(thumb_image1 >= input$Tissue_only_threshold_Run_AI), 1) %>% 
          replace(which(thumb_image1 < input$Tissue_only_threshold_Run_AI), 0) %>% 
          dilate(kern = makeBrush(size = input$Tissue_only_dilate_pixel_Run_AI, shape = "disc")) %>% 
          resize(w = 2^(check_layer - 5) * 2 * (1024 / tile_size),
                 h = 2^(check_layer - 5) * 2 * (1024 / tile_size), filter = "bilinear")
        
        thumb_image2 <- thumb_restricted_region_function(slide_file = image_name,
                                                         polygon_layer_select = paste0(input$AI_pre_label_model_restricted_primary_layer_Run_AI, 
                                                                                       "_",
                                                                                       input$AI_pre_label_model_restricted_secondary_layer_Run_AI,
                                                                                       "_",
                                                                                       input$AI_pre_label_model_restricted_tertiary_layer_Run_AI))
        for (resize_number in 9:(check_layer - 5)) {
          thumb_image2 <- thumb_image2 %>% 
            resize(w = 2^(resize_number) * 2 * (1024 / tile_size),
                   h = 2^(resize_number) * 2 * (1024 / tile_size), filter = "bilinear")
        }
        
        thumb_image <- thumb_image1 + thumb_image2
        
        thumb_array <- which(thumb_image > 0, arr.ind = TRUE)
        #print(thumb_array)
        thumb_table <- data.frame(
          center_x = (thumb_array[,1] - 1) * center_move_pixel + center_move_pixel/2,
          center_y = (thumb_array[,2] - 1) * center_move_pixel + center_move_pixel/2,
          base_layer = check_layer
        )
        
        future_table <- inner_join(future_table, thumb_table)
      }
      

      total_check <- ceiling(nrow(future_table)/batch_size)
      #i in 1:ceiling(nrow(future_table)/batch_size)
      
      #check_future_table <- data.frame(
      #  x = (future_table$center_x - shifting_chunk)/(2^(12 - check_layer))/tile_size,
      #  y = (future_table$center_y - shifting_chunk)/(2^(12 - check_layer))/tile_size,
      #  base_layer = check_layer
      #)
      
      #saveRDS(check_future_table, paste0(temp_folder_location, "/future_table.rds"))
      
      for (i in 1:total_check) {
        
        if (i != total_check) {
          counting_up <- ((i - 1) * batch_size + 1):(i * batch_size)
        } else {
          counting_up <- ((i - 1) * batch_size + 1):nrow(future_table)
        }
        check_file_resolve <- paste0(temp_folder_location, "/check_", i, ".rds")
        saveRDS(i, check_file_resolve)
        
        if (length(sample_layer) == 1) {
          image_use <- lapply(counting_up, function(x) {
            NDPI_center_RGB_batch_Num(file_name = image_name,
                                      all_x_pixel = future_table$center_x[x],
                                      all_y_pixel = future_table$center_y[x],
                                      all_pyramid_layer = sample_layer,
                                      tilesize = tile_size + overlap_pixel*2)
          })
        } else {
          image_use <- lapply(counting_up, function(x) {
            NDPI_center_RGB_pyramid_batch_Num(file_name = image_name,
                                              x_pixel = future_table$center_x[x],
                                              y_pixel = future_table$center_y[x],
                                              all_pyramid_layer = sample_layer,
                                              tilesize = tile_size + overlap_pixel*2)
          })
        }
        
        predict_result <- lapply(image_use, function(x) {
          AI_pre_label$model$predict(x, batch_size = as.integer(input$AI_pre_label_in_slide_batch_size_Run_AI))
        })
        
        
        #if (i %in% c(4, 12, 18, 19)) {
        # saveRDS(list(batch_result = abind::abind(predict_result, along = 1),
        #               deviation_x = (future_table$center_x[counting_up] - shifting_chunk)/2^(12 - check_layer) - overlap_pixel,
        #               deviation_y = (future_table$center_y[counting_up] - shifting_chunk)/2^(12 - check_layer) - overlap_pixel,
        #               basal_layer = check_layer,
        #               overlap_pixel = overlap_pixel,
        #               #polygon_threshold = polygon_threshold,
        #               #point_threshold = point_threshold,
        #               #polygon_simplify = polygon_simplify,
        #               modified_label_table = modified_label_table,
        #               temp_AI = temp_folder_location,
        #               check_file_resolve = check_file_resolve),
        #          paste0("list_", i, ".rds"))
        #}
        
        future({
          slide_result_function(batch_result = abind::abind(predict_result, along = 1),
                                deviation_x = (future_table$center_x[counting_up] - shifting_chunk)/2^(12 - check_layer) - overlap_pixel,
                                deviation_y = (future_table$center_y[counting_up] - shifting_chunk)/2^(12 - check_layer) - overlap_pixel,
                                basal_layer = check_layer,
                                overlap_pixel = overlap_pixel,
                                #polygon_threshold = polygon_threshold,
                                #point_threshold = point_threshold,
                                #polygon_simplify = polygon_simplify,
                                modified_label_table = modified_label_table,
                                temp_AI = temp_folder_location,
                                check_file_resolve = check_file_resolve)
        }, seed = NULL)
        
        updateProgressBar(
          session = session,
          title = paste0("Model start predicting...(", i, "/", total_check, ")"),
          id = "AI_pre_label_myprogress",
          value = 20 + 60/total_check * i
        )
		
		  #---------#
	   keras$backend$clear_session()
       k_clear_session() 
	   clear_gpu()
       mem_release()
	   #---------#

		rm(counting_up,check_file_resolve,image_use,predict_result)
        gc(verbose = getOption("verbose"), reset = TRUE, full = TRUE)
		
      }
      
      updateProgressBar(
        session = session,
        title = paste0("Wait for all core/thread to resolve...(0/120[max check times])"),
        id = "AI_pre_label_myprogress",
        value = 90
      )
      
      check_time <- 0
      while((list.files(temp_folder_location, pattern = "check") %>% length() != 0) & check_time < 120) {
        Sys.sleep(5)
        check_time <- check_time + 1
        updateProgressBar(
          session = session,
          title = paste0("Wait for all core/thread to resolve...(", check_time, "/120[max check times])"),
          id = "AI_pre_label_myprogress",
          value = 90
        )

      }
      #resolve(last_future)
      
      for (i in 1:nrow(modified_label_table)) {
        updateProgressBar(
          session = session,
          title = paste0("Saving result...(", i, "/", nrow(modified_label_table), ")"),
          id = "AI_pre_label_myprogress",
          value = 90 + 10/(nrow(modified_label_table) + 1) * i
        )
		
        
        if (modified_label_table$Use_in_label[i] == "Y") {
		
          if (modified_label_table$Inference_G[i] != "-") {
            update_polygon_class <- modified_label_table$Inference_G[i]
            
            change_polygon_class <- (modified_label_table$Inference_G[i] %>% str_split("_") %>% unlist())[1:3] %>% paste0(collapse = "_")
            
            all_polygon_df_file_location <- list.files(temp_folder_location, 
                                                       pattern = paste0(update_polygon_class, "_"),
                                                       full.names = TRUE)
            if (length(all_polygon_df_file_location) > 0) {
              poly_location <- XY2LonLat(x = base::c(0,
                                                     slide_info$Width_pixel,
                                                     slide_info$Width_pixel,
                                                     0,
                                                     0),
                                         y = base::c(0,
                                                     0,
                                                     slide_info$Height_pixel,
                                                     slide_info$Height_pixel,
                                                     0),
                                         zoom = 12 + 1)
              
              chunk_polygon <- lawn_polygon(list(list(
                base::c(poly_location[1,1], poly_location[1,2]),
                base::c(poly_location[2,1], poly_location[2,2]),
                base::c(poly_location[3,1], poly_location[3,2]),
                base::c(poly_location[4,1], poly_location[4,2]),
                base::c(poly_location[5,1], poly_location[5,2])
              )))
              
              cut_polygon <- chunk_polygon 
              
              all_polygon_df <- lapply(all_polygon_df_file_location, function(x) {
                x %>% readRDS()
              })
              
              #saveRDS(all_polygon_df, "test.rds")
              
              outer_all_polygon_df <- lapply(all_polygon_df, function(x){
                x$outer
              })
              
              inner_all_polygon_df <- lapply(all_polygon_df, function(x){
                x$inner
              })
              
              outer_poly <- cluster_geojson_merge(outer_all_polygon_df)
              inner_poly <- cluster_geojson_merge(inner_all_polygon_df)
              
              merge_poly_result <- combine_outer_and_inner_poly_function(outer_all_polygon_df = outer_poly, 
                                                                         inner_all_polygon_df = inner_poly)
              
              
              #for (cut_poly in 1:length(all_polygon_df)) {
              #  cut_polygon <- lawn_difference(cut_polygon, all_polygon_df[[cut_poly]])
              #}
              
              for (ii in 1:length(merge_poly_result)) {
                if (ii == 1) {
                  merge_all_polygon <- merge_poly_result[[1]]
                  merge_all_polygon$geometry$type <- "MultiPolygon"
                  merge_all_polygon$geometry$coordinates <- list()
                  if (class(merge_poly_result[[ii]]$geometry$coordinates) == "array") {
                    merge_all_polygon$geometry$coordinates[[1]] <- merge_poly_result[[ii]]$geometry$coordinates
                  } else if (class(merge_poly_result[[ii]]$geometry$coordinates) == "list") {
                    merge_all_polygon$geometry$coordinates[[1]] <- merge_poly_result[[ii]]$geometry$coordinates
                  }
                } else {
                  if (class(merge_poly_result[[ii]]$geometry$coordinates) == "array") {
                    merge_all_polygon$geometry$coordinates[[length(merge_all_polygon$geometry$coordinates) + 1]] <- merge_poly_result[[ii]]$geometry$coordinates
                  } else if (class(merge_poly_result[[ii]]$geometry$coordinates) == "list") {
                    if (sum(class(merge_poly_result[[ii]]$geometry$coordinates[[1]]) == "matrix") > 0) {
                      merge_all_polygon$geometry$coordinates <- c(merge_all_polygon$geometry$coordinates, list(merge_poly_result[[ii]]$geometry$coordinates))
                    } else {
                      merge_all_polygon$geometry$coordinates <- c(merge_all_polygon$geometry$coordinates, merge_poly_result[[ii]]$geometry$coordinates)
                    }
                  }
                }
              }
              
              #print(modified_label_table$Replace_Add_Clip[i])
              
              if (modified_label_table$Replace_Add_Clip[i] == "R") {
                Polygons_data_current_on_map$list[[update_polygon_class]] <- geojson_split_function(merge_all_polygon) 
              } else if (modified_label_table$Replace_Add_Clip[i] == "A") {
                if (length(Polygons_data_current_on_map$list[[update_polygon_class]]) == 0) {
                  Polygons_data_current_on_map$list[[update_polygon_class]] <- geojson_split_function(merge_all_polygon)
                } else {
                  merge_all_current_polygon <- geojson_merge_function(Polygons_data_current_on_map$list[[update_polygon_class]],
                                                                      method = "exact")
                  
                  cut_polygon <- chunk_polygon 
                  cut_polygon <- cut_polygon %>% lawn_difference(merge_all_current_polygon) %>% lawn_difference(merge_all_polygon)
                  union_all_polygon <- lawn_difference(chunk_polygon, cut_polygon)
                  split_all_polygon <- geojson_split_function(union_all_polygon)
                  
                  Polygons_data_current_on_map$list[[update_polygon_class]] <- split_all_polygon
                  
                  original_bbox <- lapply(Polygons_data_current_on_map$list[[update_polygon_class]], function(x){
                    lawn_bbox(x)
                  })
                }
              } else if (modified_label_table$Replace_Add_Clip[i] == "C") {
                if (length(Polygons_data_current_on_map$list[[update_polygon_class]]) == 0) {
                } else {
                  merge_all_current_polygon <- geojson_merge_function(Polygons_data_current_on_map$list[[update_polygon_class]],
                                                                      method = "exact")
                  union_all_polygon <- lawn_difference(merge_all_current_polygon, merge_all_polygon)
                  split_all_polygon <- geojson_split_function(union_all_polygon)
                  
                  Polygons_data_current_on_map$list[[update_polygon_class]] <- split_all_polygon
                  
                  original_bbox <- lapply(Polygons_data_current_on_map$list[[update_polygon_class]], function(x){
                    lawn_bbox(x)
                  })
                }
              }
              
              file.remove(all_polygon_df_file_location)
            }
          } 
          
          if (modified_label_table$Inference_P[i] != "-") {
		  
            update_point_class <- paste0(modified_label_table$Inference_P[i], "_")
			
            all_point_df_file_location <- list.files(temp_folder_location, 
                                                     pattern = update_point_class,
                                                     full.names = TRUE)
													 
            if (length(all_point_df_file_location) > 0) {
              all_point_df <- lapply(all_point_df_file_location, readRDS)
              all_point_df <- all_point_df %>% abind::abind(along = 1)
		
			  
              if (nrow(all_point_df) > 0) {
                #i <- 999
				
                if (modified_label_table$Replace_Add_Clip[i] == "R") {
                  Points_data_current_on_map$df <- Points_data_current_on_map$df %>% 
                    filter(point_primary != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[1] | 
                             point_secondary != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[2] | 
                             point_tertiary != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[3] | 
                             point_group != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[4:5] %>% paste0(collapse = "_"))
                  Points_data_current_on_map$df <- rbind(Points_data_current_on_map$df, all_point_df)
				  
				  
                } else if (modified_label_table$Replace_Add_Clip[i] == "A") {
                  Points_data_current_on_map$df <- rbind(Points_data_current_on_map$df, all_point_df)
                } else if (modified_label_table$Replace_Add_Clip[i] == "C") {
                  Points_data_current_on_map$df <- Points_data_current_on_map$df %>% filter((point_id %in% all_point_df[, 2]) == FALSE)
				  
                }
              } else {
                if (modified_label_table$Replace_Add_Clip[i] == "R") {
                  Points_data_current_on_map$df <- Points_data_current_on_map$df %>% 
                    filter(point_primary != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[1] | 
                             point_secondary != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[2] | 
                             point_tertiary != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[3] | 
                             point_group != (modified_label_table$Inference_P[i] %>% str_split("_") %>% unlist())[4:5] %>% paste0(collapse = "_"))
					
                  				
                }
              }
			 
              file.remove(all_point_df_file_location)
			 
			  
            }
          }
   
          if (modified_label_table$Inference_H[i] != "-") {
		    
            update_heat_class <- paste0(modified_label_table$Inference_H[i], "_")
            all_heat_df_file_location <- list.files(temp_folder_location, 
                                                    pattern = update_heat_class,
                                                    full.names = TRUE)
					
            if(length(all_heat_df_file_location)>0){ 
			
            all_heat_df <- lapply(all_heat_df_file_location, readRDS)
            

            max_width <- max(sapply(all_heat_df, function(x){x[[2]]}))
            max_height <- max(sapply(all_heat_df, function(x){x[[3]]}))
            max_size <- max(max_width, max_height)
			
            blank_heat <- array(0, c(max_size, max_size,1))
			
			
			
            for (chunk_no in 1:length(all_heat_df)) {
              blank_heat[all_heat_df[[chunk_no]][[2]][1]:all_heat_df[[chunk_no]][[2]][2], 
                         all_heat_df[[chunk_no]][[3]][1]:all_heat_df[[chunk_no]][[3]][2],1] <- all_heat_df[[chunk_no]][[1]]
            }
           
            color_number <- (str_split(modified_label_table$Inference_H[i], "_H_") %>% unlist())[2]
            color_number <- input$Change_heat_color_order[as.integer(color_number)]
            
            base_heat_R <- array(color_name_value[[color_number]][1], c(max_size, max_size, 1))
            #saveRDS(base_heat_R, "base_heat_R.rds")
            base_heat_G <- array(color_name_value[[color_number]][2], c(max_size, max_size, 1))
            #saveRDS(base_heat_G, "base_heat_G.rds")
            base_heat_B <- array(color_name_value[[color_number]][3], c(max_size, max_size, 1))
            #saveRDS(base_heat_B, "base_heat_B.rds")
            #saveRDS(blank_heat, "blank_heat.rds")
          
            blank_heat <- abind::abind(base_heat_R, 
                                       base_heat_G, 
                                       base_heat_B, 
                                       blank_heat, along = 3)
           
            save_image <- Image(blank_heat, colormode = 'Color') 
            Heats_data_current_on_map$list[[modified_label_table$Inference_H[i]]] <- list(save_image, max_size)
			
            save_image %>% writeImage(paste0("www/Slide/RUN_AI/",
                                             model_for_report_list[count], "/", 
                                             "temp/",
                                             update_heat_class,
                                             "0_0.png"))

            saveRDS(Heats_data_current_on_map$list,paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","heats.rds"))											 
		
            file.remove(all_heat_df_file_location)
			}else{
			  Heats_data_current_on_map$list <- list()
			  saveRDS(Heats_data_current_on_map$list,paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","heats.rds"))
			}
          }
        }
      }
    
      current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
      modified_polygon_list <- Polygons_data_current_on_map$list[[current_polygon_class]]
      polygon_names <- Polygons_data_current_on_map$list[[current_polygon_class]] %>% names()
      if (length(modified_polygon_list) > 0) {
        Polygons_data_current_on_map$bbox <- lapply(modified_polygon_list, function(x){
          lawn_bbox(x)
        })
      } else {
        Polygons_data_current_on_map$bbox <- list()
      }
      
      show_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
      show_polygon_list <- Polygons_data_current_on_map$list[str_detect(names(Polygons_data_current_on_map$list), 
                                                                        paste0(show_polygon_class, "_G_"))]
																		

	   model_for_report <- c(paste0(input$Slide_or_image),paste0(input$Annotation_group))#file path
	   
		modified_point_table <- Points_data_current_on_map$df %>% 
        filter(point_primary == input$Point_class_1) %>% 
        filter(point_secondary == input$Point_class_2) %>% 
        filter(point_tertiary == input$Point_class_3)
		

		
		if(modified_point_table$point_id[1] == 1 & length(modified_point_table$point_id) > 1){

		   
	       modified_point_table <-  modified_point_table[-1,]
		   saveRDS(modified_point_table,paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","points.rds"))
	    
		}
		
		saveRDS(modified_point_table,paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","points.rds"))

	
	    saveRDS(Polygons_data_current_on_map$list,paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","polygons.rds"))
	    #saveRDS(modified_point_table,paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","points.rds"))
		
		Polygons_data_current_on_map$list <- list()
	    
		
       img_location <- img_location[-1]
	  
	  
      updateProgressBar(
        session = session,
        title =paste0("Updating result to image...","(",model_for_report_list[count],")"),
        id = "AI_pre_label_myprogress",
        value = 100
         )

	   }
    

    future:::ClusterRegistry(action = "stop")
	plan(future.callr::callr)
    #plan(list(multisession, sequential))
    #plan(multisession)
    #plan(callr)

	sess_close()
	accelerator()

    rm(list=ls(all=TRUE))
    gc(verbose = getOption("verbose"), reset = TRUE, full = TRUE)
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "Pre-label finish!!",
      text = "The labeled image have been updated!!",
      type = "success"
    )
	 
	 }else{
	     
		 sendSweetAlert(
              session = session,
              title = paste0("Please select your training file..." ),
              type = "error"
           )
	 
	 }
  }
  
   

})




	



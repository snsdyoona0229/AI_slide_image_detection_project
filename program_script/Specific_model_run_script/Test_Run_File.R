observeEvent(input$select_all_generative_file_Run_AI, {
  updateMultiInput(session, inputId = "Choose_file_training_table_Run_AI", selected = list.files(paste0("www/", input$Slide_or_image, "/", input$Annotation_group)) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE))
})

observeEvent(input$delete_all_generative_file_Run_AI, {
  updateMultiInput(session, inputId = "Choose_file_training_table_Run_AI", selected = character(0))
})

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
  
  
  #files_list <- NULL
  

  if (input$Fast_slide_proces_files_Run_AI_file[1] %>% class() == "list") {
    root_name <- getVolumes()()[input$Fast_slide_proces_files_Run_AI_file$root] %>% str_replace("\\/", "")
    choose_file <- sapply(input$Fast_slide_proces_files_Run_AI_file$files, function(x){
      paste0(root_name, unlist(x) %>% paste0(collapse = "/"))
    })
    
	files_list <- c()

	
   for(i in 1:length(choose_file)){  
     files_list[i] <- strsplit(choose_file[i], split = '/')[[1]][length(strsplit(choose_file[i], split = '/')[[1]])] %>% str_replace(" ", "")
	  
	  }
										   
	  Fast_slide_process_Run_AI$df <- data.frame("Slide" = files_list,	                                        
                                                 "new_name" = "",
                                                 "Group_of_Slide" = "RUN_AI",
												 "WSL_class" = input$Select_customuze_AI_to_use,
												 "datapath" = choose_file,
                                                  stringsAsFactors = FALSE)	


										
  }
  
  output$Fast_slide_file_table_Run_AI <- renderRHandsontable({
  if (nrow(Fast_slide_process_Run_AI$df) > 0) {
    rhandsontable(Fast_slide_process_Run_AI$df )%>% 
      hot_col(col = "Group_of_Slide", type = "dropdown", source = list.files("www/Slide/")) %>% 
      hot_col("Slide", readOnly = TRUE)
      }
  
  })

})

#Fast_slide_process_Run_AI$df <- data.frame()

observeEvent(input$Start_AI_prelabel_Run_AI_Simplify,{

Start <- Sys.time()


Default_parameters <- list(Colon_LN=c("MLN_Best","20x",2,2048,6,0.7,10),
                           Mitosis=c("Mitosis_Best","40x",2,2048,6,0.7,10),
                           Gastric_biopsy=c("GCA_Best","20x",2,2048,6,0.7,10),
                           Sentiinel_LN=c("SLN_Best","20x",2,2048,6,0.7,10), 
                           Glomeruli=c("GR_Best","10x",2,2048,6,0.7,10),
                           inter_face = c("Colon LN/cancer detection","Mitosis","Gastric_biopsy","Sentiinel_LN","Glomeruli"))



inter_face <- which(Default_parameters$inter_face==as.character(input$Select_customuze_AI_to_use))
Simplify_train_information <- Default_parameters[[inter_face]]

    model_detail <- readRDS(paste0("model/","1_Good", "/",Simplify_train_information[[1]], "/detail.rds"))

    split_word <- c(model_detail$layer_class_id_P, model_detail$layer_class_id_G) %>% str_split("_")
    
    Primary_layer_ID <- vapply(split_word, function(x){
      x[1] %>% as.integer()
    },integer(1))
    
    Secondary_layer_ID <- vapply(split_word, function(x){
      x[2] %>% as.integer()
    },integer(1))
    
    Tertiary_layer_ID <- vapply(split_word, function(x){
      x[3] %>% as.integer()
    },integer(1))
    
    option_word <- vapply(split_word, function(x){
      if (is.na(x[5])) {
        ""
      } else {
        paste0("_", x[5])
      }
    },character(1))
	
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
    AI_pre_label$output_table[, 9] <- as.numeric(Simplify_train_information[[6]])
    AI_pre_label$output_table[, 10] <- as.integer(Simplify_train_information[[7]])
    AI_pre_label$output_table[, 11] <- "-"
    AI_pre_label$output_table[, 12] <- "0.00005"
  
#######

 progressSweetAlert(
    session = session, id = "AI_pre_label_myprogress",
    title = "Upload_files",
    display_pct = TRUE, value = 5
     )  
  
  upload_data <- as.data.frame(hot_to_r(input$Fast_slide_file_table_Run_AI))
  slide_number <- str_detect(upload_data$Slide,".ndpi|.isyntax|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif") %>% sum()
  
  
  img_location_Upload <- c()
  model_for_report_list_Upload <- c()


  if(nrow(upload_data) > 0 ){
    
    if (input$Slide_or_image == "Slide" && nrow(upload_data) != slide_number && length(which(upload_data$Slide ==""))==0) {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Your upload files are not all slides which are accepted by our system!",
        type = "error"
      )
    }else{
#  model_location <- paste0("model/", "1_Good", "/",Simplify_train_information[1], "/Model.h5")
#  if (file.exists(model_location)) {


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
		

#          if (upload_data$new_name[i] == "") {
#            file_name <- upload_data$Slide[i]
#          } else {
#            file_name <- paste0(upload_data$new_name[i],".ndpi")
#          }
          file_name <- if(upload_data$new_name[i] == "") upload_data$Slide[i] else paste0(upload_data$new_name[i],".ndpi")


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
		
#          file.rename(upload_data$datapath[i], paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/", file_name))

           file.copy(paste0(upload_data$datapath[i]), 
                      paste0("www/Slide/", upload_data$Group_of_Slide[i], "/", file_name_no_ndpi, "/", file_name))	

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
         MAGNIFICATION <- "40x"
		 Lens <- 40
		 opened_slide <- openslide$OpenSlide(upload_data$datapath[i])
		 
		if(length(opened_slide$properties$get('openslide.objective-power')) > 0){
		  MAGNIFICATION <- paste0(opened_slide$properties$get('openslide.objective-power'),"x") %>% as.character()
		  Lens <- opened_slide$properties$get('openslide.objective-power') %>% as.integer()
		 }
        output_list <- list(
          "Width_pixel" = NA,
          "Height_pixel" = NA,
          "Physicalx_center" = NA,
          "Physicaly_cente" = NA,
          "Physicalwidth_nm" = NA,
          "Physicalheight_nm" = NA,
          "Lens" = Lens %>% as.integer(),             
          "Number_of_z_layers" = 1 %>% as.integer(),
          "Number_of_channels" = 3 %>% as.integer(),
          "Bits_per_channel" = 8 %>% as.integer(),
          "image_size" = NA,
          "source_lens" = MAGNIFICATION
        )
        
		###path_setting
		current_format <- gsub("/",".",current_format )
		current_file_name <- str_replace_all(upload_data$Slide[i],current_format,"")
		current_root <- str_replace_all(upload_data$datapath[i],upload_data$Slide[i],"")
		
		###path_setting
		
        #opened_slide <- openslide$OpenSlide(upload_data$datapath[i])
        tile_generator <- openslide$deepzoom$DeepZoomGenerator(osr = opened_slide,
                                                               tile_size = as.integer(256), 
                                                               overlap = as.integer(0))

       tile_table <- tile_generator$level_dimensions %>% unlist() %>% matrix(nrow = 2) 
	   tile_table <- as.data.frame(tile_table)%>% transpose()

        output_list$image_size <- tile_table[nrow(tile_table),] %>% as.integer()
        output_list$Width_pixel <- tile_table[nrow(tile_table),1] %>% as.integer()
        output_list$Height_pixel <- tile_table[nrow(tile_table),2] %>% as.integer()

#        if (upload_data$new_name[i] == "") {
#          file_name <- upload_data$Slide[i]
#        } else {
#          file_name <- paste0(upload_data$new_name[i], current_format)
#        }
         file_name <- if(upload_data$new_name[i] == "") upload_data$Slide[i] else  paste0(upload_data$new_name[i], current_format)
		 
        file_name_no_ndpi <- file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-")
        if (file_name_no_ndpi %in% exist_name) {
          file_name_no_ndpi <- paste0(file_name_no_ndpi, "_", time_name)
        }
		
		img_location_Upload[i]  <- paste0("www/Slide/",input$Annotation_group, "/", file_name_no_ndpi)
		model_for_report_list_Upload[i] <- paste0(file_name_no_ndpi)
##########
		if(str_detect(upload_data$Slide[i], ".svs|.vmu|.tif|.scn|.tiff|.svslide|.bif")){
           dir.create(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi))
		}
		if(str_detect(upload_data$Slide[i], ".vms")){
		  
          datapath_string_path_split <- strsplit(upload_data$datapath[i],"/")	
          file_root_localtion <- str_replace_all(upload_data$datapath[i],datapath_string_path_split[[1]][length(datapath_string_path_split[[1]])],"")		  
		  dir.create(paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi))
		  move_file_to_destination <- paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi)
#input$Slide_or_image, "/", input$Annotation_group
		  for(vms_file in dir(file_root_localtion)){
		      if(str_detect(vms_file,".vms")==FALSE){

			    file.copy(paste0(file_root_localtion,"/",vms_file),paste0(move_file_to_destination,"/",vms_file),overwrite = TRUE)
			 }
		   } 
		}
	    #---#
		if(str_detect(upload_data$Slide[i], ".mrxs")){
		   root_mrxs <- current_file_name %in% dir(current_root) %>% sum()

           if(root_mrxs == 1){
		      if(file.exists(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-") ))==TRUE){

                current_file_modify_duplicate <- paste0(current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-"),"_", time_name)			   
			  
                dir.create(paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/", current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-"), "_", time_name))
				
                dir.create(paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/",current_file_modify_duplicate,"/",current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-"),"_", time_name, "___", output_list$Width_pixel, "___", output_list$Height_pixel, "___", output_list$source_lens))
				
                select_folder <- paste0(current_root,"/",current_file_name)
				destination_folder <- paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/",current_file_modify_duplicate,"/",current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-"),"_", time_name, "___", output_list$Width_pixel, "___", output_list$Height_pixel, "___", output_list$source_lens)
				
				
				for(d in dir(select_folder)){
				   file.copy(paste0(select_folder,"/",d),paste0(destination_folder,"/",d),overwrite = TRUE)
				}
				
			  }else{
			    current_file_modify <- current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-")
				
			   	dir.create(paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/",current_file_modify ))
				dir.create(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/",current_file_modify,"/",current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-"), "___", output_list$Width_pixel, "___", output_list$Height_pixel, "___", output_list$source_lens))
				
                select_folder <- paste0(current_root,"/",current_file_name)
				destination_folder <- paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/",current_file_modify,"/",current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-"), "___", output_list$Width_pixel, "___", output_list$Height_pixel, "___", output_list$source_lens)
				
				for(d in dir(select_folder)){
				   file.copy(paste0(select_folder,"/",d),paste0(destination_folder,"/",d),overwrite = TRUE)
				}

             }			  
           }		   
		}
		###########

        dir.create(paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/info"))
        dir.create(paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/temp"))
		dir.create(paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/AI_temp"))
        
        saveRDS(output_list, paste0("www/",input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/info/info.rds"))
		
		
        try(
          opened_slide$associated_images['macro']$convert(mode = "RGB")$save(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/info/SlideImage.jpg"), format="JPEG"),
          silent = TRUE
        )
        
	   tile_table_sapply <- tile_generator$level_tiles %>% unlist() %>% matrix(nrow = 2)
	   tile_table_sapply <- as.data.frame(tile_table_sapply)%>% transpose()
	   
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
        
        file.copy(upload_data$datapath[i], paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/", file_name_no_ndpi, "/", file_name))
        

              }
      
            }
         }
	  
       }
 # }  
####### 
  model_location <- paste0("model/", "1_Good/",Simplify_train_information[[1]], "/Model.h5")
  if (file.exists(model_location)) {
  
    if (AI_pre_label$model_name != paste0("1_Good/",Simplify_train_information[[1]]) || AI_pre_label$model_type != "By Validation Set") {
	
      updateProgressBar(
        session = session,
        title = "Loading model...",
        id = "AI_pre_label_myprogress",
        value = 10
      )
      AI_pre_label$model_name <- paste0("1_Good/",Simplify_train_information[[1]])
      AI_pre_label$model_type <- "By Validation Set"
      AI_pre_label$model_detail <- readRDS(paste0("model/","1_Good/",Simplify_train_information[[1]], "/detail.rds"))
      source(paste0("model/","1_Good/",Simplify_train_information[[1]], "/run_use.R"), local = TRUE)
      AI_pre_label$model <- load_model_weights_hdf5(object = get_unet(input_shape = list(NULL, NULL, 3),
                                                                      num_classes =1), 
                                                    filepath = model_location)
																								
    }
	
	
	train_length <- length(img_location_Upload) +  length(c(input$Choose_file_training_table_Run_AI)) 
	

	
    if (input$Slide_or_image == "Slide" && train_length > 0) {
	
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
   
      count <- count + 1
	  
	  Points_data_current_on_map$df <- point_file_function(paste0(img_location[1], "/points.rds"))
	  
	  if(file.exists(paste0(img_location[1], "/points.rds"))==FALSE){
	  
	       Points_data_current_on_map$df <- data.frame(point_group="P_1",point_id=1,point_lon=1,point_lat=1,point_primary=1,point_secondary=1,point_tertiary=1)
		}
		
	 # image_name <- list.files(img_location[1], pattern = ".ndpi", full.names = TRUE)
     # slide_info <- NDPI_info(image_name)

	  ###
      image_name <- list.files(img_location[1], pattern = ".ndpi|.isyntax|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif", full.names = TRUE)
      
	  
	   
#	  if(str_detect(image_name, ".ndpi")==TRUE){
#	     slide_info <- NDPI_info(image_name)
#	  }else{
#	     slide_info <- readRDS(file = paste0(img_location[1],"/info/info.rds"))		 
#	  }
      slide_info <- if(str_detect(image_name, ".ndpi")) NDPI_info(image_name) else readRDS(file = paste0(img_location[1],"/info/info.rds"))	
	  
	  ###
      tile_size <-Simplify_train_information[[4]] %>% as.integer()
      batch_size <- 16
      sample_layer <- log2((Simplify_train_information[[2]] %>% str_replace("x", "") %>% as.numeric())/0.078125) + 3
      check_layer <- max(sample_layer)
      shifting_chunk <- (tile_size * 2^(12 - check_layer))/2
      center_move_pixel <- tile_size * 2^(12 - check_layer)
      tile_x_number <- ceiling(slide_info$Width_pixel/center_move_pixel)
      tile_y_number <- ceiling(slide_info$Height_pixel/center_move_pixel)
      #temp_folder_location <- paste0(img_location[1], "/AI_temp")
	  temp_folder_location <- paste0("www/Slide/RUN_AI/", model_for_report_list[count], "/AI_temp")
      modified_label_table <- AI_pre_label$output_table
	  #numba$vectorize(target="parallel", nopython=TRUE)
	  numba$vectorize(target="parallel", nopython=TRUE)
	  blas_set_num_threads(parallel::detectCores())
	  
	  

	  opened_slide <- openslide$OpenSlide(image_name)
	  
	  slide_info_file <- readRDS(file = paste0("www/Slide/RUN_AI/",model_for_report_list[count], "/info/info.rds"))
	   
      run_gpu_batch <-Simplify_train_information[[5]] %>% as.integer()
       
#       if (file.exists(temp_folder_location) == FALSE) {
#        dir.create(temp_folder_location)
#      } else {
        #rm_file <- list.files(temp_folder_location, full.names = TRUE)
#		rm_file <-.Internal(list.files(path = temp_folder_location, pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
#        file.remove(rm_file)
#     }
      ifelse(file.exists(temp_folder_location),file.remove(list.files(temp_folder_location, full.names = TRUE)),dir.create(temp_folder_location))
	 

      #polygon_threshold <- input$AI_pre_label_polygon_threshold
      #point_threshold <- input$AI_pre_label_point_threshold
      #polygon_simplify <- input$AI_pre_label_polygon_simplify
      overlap_pixel <- Simplify_train_information[[3]] %>% as.integer()
      
      #saveRDS(modified_label_table, "modified_label_table.rds")
	  
      future_table <- data.table(
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
      #----#
#	  if(str_detect(image_name, ".ndpi")==TRUE){
#         thumb_image <- NDPI_center_RGB_batch_Num(file_name = image_name,
#                                                  all_x_pixel = 1024 * 2^(12 - 5),
#                                                  all_y_pixel = 1024 * 2^(12 - 5),
#                                                  all_pyramid_layer = 5,
#                                                  tilesize = 2048)								  
#	  }else{
#		thumb_image <- NDPI_center_RGB_batch_Num_function_Background(opened_slide,image_name,img_location[1],131072L,131072L,5L,2048L,slide_info_file)
#	  }

	 thumb_image <- if(str_detect(image_name, ".ndpi")) NDPI_center_RGB_batch_Num(file_name = image_name,all_x_pixel = 1024 * 2^(12 - 5),all_y_pixel = 1024 * 2^(12 - 5),all_pyramid_layer = 5,tilesize = 2048) else  NDPI_center_RGB_batch_Num_function_Background(opened_slide,image_name,img_location[1],1024 * 2^(12 - 5),1024 * 2^(12 - 5),5L,2048L,slide_info_file)


	  #----#

        if (AI_pre_label$tissue_model == "") {
          source("store_data_for_program/Tissue_Model.R", local = TRUE)
          AI_pre_label$tissue_model <- load_model_weights_hdf5(object = get_tissue(num_classes = 1), filepath = "store_data_for_program/Tissue_Model.h5")
        }
        
        thumb_image <- AI_pre_label$tissue_model$predict(thumb_image)[1,,,1]
		
        
        thumb_image <- replace(thumb_image,which(thumb_image >= 0.8), 1) %>% 
          replace(which(thumb_image < 0.8), 0) %>% 
          dilate(kern = makeBrush(size = 5, shape = "disc"))

        thumb_tile_number <- 2^(check_layer - 5) * 2 * (1024 / tile_size)
        tile_chunk <- untile(thumb_image, c(thumb_tile_number, thumb_tile_number),lwd = 0)
        thumb_image <- sapply(1:(thumb_tile_number^2), function(x){max(tile_chunk[,,x])}) %>% array(c(thumb_tile_number, thumb_tile_number))


        thumb_array <- which(thumb_image > 0, arr.ind = TRUE)
        
		
         if(length(thumb_array)==0){
          thumb_array <- which(thumb_image >= 0, arr.ind = TRUE)
        }		
	    check_layer_genertor <- rep(check_layer, length(thumb_array[,1]))
		
        thumb_table <- data.table(
          center_x = (thumb_array[,1] - 1) * center_move_pixel + center_move_pixel/2,
          center_y = (thumb_array[,2] - 1) * center_move_pixel + center_move_pixel/2,
          base_layer = check_layer_genertor
        )
		
        future_table <- inner_join(future_table, thumb_table)
		#future_table <- rbind(future_table, thumb_table)
        total_check <- ceiling(nrow(future_table)/batch_size)
      
      #i in 1:ceiling(nrow(future_table)/batch_size)
      #check_future_table <- data.frame(
      #  x = (future_table$center_x - shifting_chunk)/(2^(12 - check_layer))/tile_size,
      #  y = (future_table$center_y - shifting_chunk)/(2^(12 - check_layer))/tile_size,
      #  base_layer = check_layer
      #)
      #saveRDS(check_future_table, paste0(temp_folder_location, "/future_table.rds"))
	  
      
      for (i in 1:total_check) {
	    

#------------------------------------------------------------------------------------------------------------------------------------------#
#        if (i != total_check) {
#          counting_up <- ((i - 1) * batch_size + 1):(i * batch_size)
#        } else {
#          counting_up <- ((i - 1) * batch_size + 1):nrow(future_table)
#        }
        counting_up <- if(i != total_check) ((i - 1) * batch_size + 1):(i * batch_size) else ((i - 1) * batch_size + 1):nrow(future_table)
		
        check_file_resolve <- paste0(temp_folder_location, "/check_", i, ".rds")
        saveRDS(i, check_file_resolve)
#------------------------------------------------------------------------------------------------------------------------------------------#
        if (length(sample_layer) == 1) {
		
#		if(str_detect(image_name, ".ndpi")==TRUE){
#		  image_use <- lapply(counting_up, function(x) {
#            NDPI_center_RGB_batch_Num(file_name = image_name,
#                                      all_x_pixel = future_table$center_x[x],
#                                      all_y_pixel = future_table$center_y[x],
#                                      all_pyramid_layer = sample_layer,
#                                      tilesize = tile_size + overlap_pixel*2)
#           })
#		 }else{ 
#          image_use <- lapply(counting_up, function(x) {
#            NDPI_center_RGB_batch_Num_function(opened_slide,image_name,img_location[1],future_table$center_x[x],future_table$center_y[x],sample_layer,tile_size,overlap_pixel,input$AI_pre_label_in_slide_layer,slide_info_file)
#		  })
#         } 

     image_use <- if(str_detect(image_name, ".ndpi")) lapply(counting_up, function(x) {NDPI_center_RGB_batch_Num(file_name = image_name,all_x_pixel = future_table$center_x[x],all_y_pixel = future_table$center_y[x],all_pyramid_layer = sample_layer,tilesize = tile_size + overlap_pixel*2)}) else lapply(counting_up, function(x) {NDPI_center_RGB_batch_Num_function(opened_slide,image_name,img_location[1],future_table$center_x[x],future_table$center_y[x],sample_layer,tile_size,overlap_pixel,input$AI_pre_label_in_slide_layer,slide_info_file)})


        } else {
          image_use <- lapply(counting_up, function(x) {
            NDPI_center_RGB_pyramid_batch_Num(file_name = image_name,
                                              x_pixel = future_table$center_x[x],
                                              y_pixel = future_table$center_y[x],
                                              all_pyramid_layer = sample_layer,
                                              tilesize = tile_size + overlap_pixel*2)
          })
        }
		
       #numba$vectorize(target="parallel", nopython=TRUE)
	   
        predict_result <-lapply(image_use, function(x) { 
          numba$vectorize(target="parallel", nopython=TRUE)
          AI_pre_label$model$predict(x, batch_size = as.integer(Simplify_train_information[[5]]))
	 	 })

		#input$AI_pre_label_in_slide_batch_size_Run_AI
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
		 
		# future_promise_queue()

	
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
	
         })
		 

		 
#------------------------------------------------------------------------------------------------------------------------------------------#
        updateProgressBar(
          session = session,
          title = paste0("Model start predicting...(", i, "/", total_check, ")"),
          id = "AI_pre_label_myprogress",
          value = 20 + 60/total_check * i
        )

	   mem_release()
	   sess_close()
		rm(counting_up,check_file_resolve,image_use,predict_result)
        .Internal(gc(verbose = getOption("verbose"), reset = TRUE, full = TRUE))

       }
      
	   

      updateProgressBar(
        session = session,
        title = paste0("Wait for all core/thread to resolve...(0/120[max check times])"),
        id = "AI_pre_label_myprogress",
        value = 90
      )
      
      check_time <- 0
      while((list.files(path = temp_folder_location, pattern = "check") %>% length() != 0) & check_time < 120) {
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
      all_polygon_df_file_location_number <- 0
	  
	  
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
            
            all_polygon_df_file_location <- .Internal(list.files(path = temp_folder_location, pattern = paste0(update_polygon_class, "_"), all.files = FALSE,full.names = TRUE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
													   
			
			all_polygon_df_file_location_number <- length(all_polygon_df_file_location)
			
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
              
              for (ii in 1:length(merge_poly_result)) {
			    merge_poly_result_simplify<- merge_poly_result[[ii]]$geometry$coordinates 
                if (ii == 1) {
                  merge_all_polygon <- merge_poly_result[[1]]
                  merge_all_polygon$geometry$type <- "MultiPolygon"
                  merge_all_polygon$geometry$coordinates <- list()
                  if (class(merge_poly_result_simplify) == "array") {
                    merge_all_polygon$geometry$coordinates[[1]] <- merge_poly_result_simplify
                  } else if (class(merge_poly_result_simplify) == "list") {
                    merge_all_polygon$geometry$coordinates[[1]] <- merge_poly_result_simplify
                  }
                } else {
                  if (class(merge_poly_result_simplify) == "array") {
                    merge_all_polygon$geometry$coordinates[[length(merge_all_polygon$geometry$coordinates) + 1]] <- merge_poly_result_simplify
                  } else if (class(merge_poly_result_simplify) == "list") {
                    if (sum(class(merge_poly_result_simplify[[1]]) == "matrix") > 0) {
                      merge_all_polygon$geometry$coordinates <- c(merge_all_polygon$geometry$coordinates, list(merge_poly_result_simplify))
                    } else {
                      merge_all_polygon$geometry$coordinates <- c(merge_all_polygon$geometry$coordinates, merge_poly_result_simplify)
                    }
                  }
                }
              }
              
              #print(modified_label_table$Replace_Add_Clip[i])
              Replace_Add_Clip_simplify <- modified_label_table$Replace_Add_Clip[i]
			  
              if (Replace_Add_Clip_simplify == "R") {
                Polygons_data_current_on_map$list[[update_polygon_class]] <- geojson_split_function(merge_all_polygon) 
              } else if (Replace_Add_Clip_simplify == "A") {
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
              } else if (Replace_Add_Clip_simplify == "C") {
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
          Inference_P_simplify <- modified_label_table$Inference_P[i] 
          if (Inference_P_simplify != "-") {
		  
            update_point_class <- paste0(Inference_P_simplify, "_")
			
            all_point_df_file_location <- .Internal(list.files(path = temp_folder_location, pattern = update_point_class, all.files = FALSE,full.names = TRUE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
			
													 
            if (length(all_point_df_file_location) > 0) {
              all_point_df <- lapply(all_point_df_file_location, readRDS)
              all_point_df <- all_point_df %>% abind::abind(along = 1)
		
			  
              if (nrow(all_point_df) > 0) {
                #i <- 999
				
                if (Replace_Add_Clip_simplify == "R") {
                  Points_data_current_on_map$df <- Points_data_current_on_map$df %>% 
                    filter(point_primary != (Inference_P_simplify %>% str_split("_") %>% unlist())[1] | 
                             point_secondary != (Inference_P_simplify %>% str_split("_") %>% unlist())[2] | 
                             point_tertiary != (Inference_P_simplify %>% str_split("_") %>% unlist())[3] | 
                             point_group != (Inference_P_simplify %>% str_split("_") %>% unlist())[4:5] %>% paste0(collapse = "_"))
                  Points_data_current_on_map$df <-bind_rows(Points_data_current_on_map$df, all_point_df)
				  
				  
                } else if (Replace_Add_Clip_simplify == "A") {
                  Points_data_current_on_map$df <- bind_rows(Points_data_current_on_map$df, all_point_df)#rbind
                } else if (Replace_Add_Clip_simplify == "C") {
                  Points_data_current_on_map$df <- Points_data_current_on_map$df %>% filter((any(all_point_df[, 2] ==point_id)) == FALSE)
				  
                }
              } else {
                if (Replace_Add_Clip_simplify == "R") {
                  Points_data_current_on_map$df <- Points_data_current_on_map$df %>% 
                    filter(point_primary != (Inference_P_simplify %>% str_split("_") %>% unlist())[1] | 
                             point_secondary != (Inference_P_simplify %>% str_split("_") %>% unlist())[2] | 
                             point_tertiary != (Inference_P_simplify %>% str_split("_") %>% unlist())[3] | 
                             point_group != (Inference_P_simplify %>% str_split("_") %>% unlist())[4:5] %>% paste0(collapse = "_"))
					
                  				
                }
              }
			 
              file.remove(all_point_df_file_location)			  
            }
          }
          Inference_H_simplify <- modified_label_table$Inference_H[i]
          if (Inference_H_simplify != "-") {
		    
            update_heat_class <- paste0(Inference_H_simplify, "_")
            all_heat_df_file_location <- .Internal(list.files(path = temp_folder_location, pattern = update_heat_class, all.files = FALSE,full.names = TRUE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
					
            if(length(all_heat_df_file_location)>0){

            all_heat_df <- lapply(all_heat_df_file_location, readRDS)
            
			#----#
			count_all_heat_df <- NULL
			
			for(ii in 1:length(all_heat_df)){
			 all_heat_df_simplify <- all_heat_df[[ii]]
			 if(length(all_heat_df_simplify) >= 3){
			   if(length(all_heat_df_simplify[[2]]) >0 && length(all_heat_df_simplify[[3]]) > 0 && (NA %in% c(all_heat_df_simplify[[2]],all_heat_df_simplify[[3]]))==FALSE){
			       count_all_heat_df[ii] <- ii
				   }
			   }
			}
			#----#
			if(length(count_all_heat_df) > 0){
			
            max_width <- max(sapply(all_heat_df[count_all_heat_df], function(x){x[[2]]}))
            max_height <- max(sapply(all_heat_df[count_all_heat_df], function(x){x[[3]]}))

            #max_width <- max(sapply(all_heat_df, function(x){x[[2]]}))
            #max_height <- max(sapply(all_heat_df, function(x){x[[3]]}))
            max_size <- max(max_width, max_height)

            blank_heat <- array(0, c(max_size, max_size,1))
			
			
            for (chunk_no in 1:length(all_heat_df)) {
              blank_heat[all_heat_df[[chunk_no]][[2]][1]:all_heat_df[[chunk_no]][[2]][2], 
                         all_heat_df[[chunk_no]][[3]][1]:all_heat_df[[chunk_no]][[3]][2],1] <- all_heat_df[[chunk_no]][[1]]
			}
           
            color_number <- (str_split(Inference_H_simplify, "_H_") %>% unlist())[2]
            color_number <- input$Change_heat_color_order[as.integer(color_number)]
            
            base_heat_R <- array(color_name_value[[color_number]][1], c(max_size, max_size, 1))
            base_heat_G <- array(color_name_value[[color_number]][2], c(max_size, max_size, 1))
            base_heat_B <- array(color_name_value[[color_number]][3], c(max_size, max_size, 1))

          
            blank_heat <- abind::abind(base_heat_R, 
                                       base_heat_G, 
                                       base_heat_B, 
                                       blank_heat, along = 3)
           
            save_image <- Image(blank_heat, colormode = 'Color') 
            Heats_data_current_on_map$list[[Inference_H_simplify]] <- list(save_image, max_size)
			
            save_image %>% writeImage(paste0("www/Slide/RUN_AI/",
                                             model_for_report_list[count], "/", 
                                             "temp/",
                                             update_heat_class,
                                             "0_0.png"))
            saveRDS(Heats_data_current_on_map$list,paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","heats.rds"))

			   }else{
	 
			     Heats_data_current_on_map$list <- list()
			     saveRDS(Heats_data_current_on_map$list,paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","heats.rds"))
			   }
			}else{
			  Heats_data_current_on_map$list <- list()
			  saveRDS(Heats_data_current_on_map$list,paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","heats.rds"))
			}
          }
        }
		
      }
	  

	  all_df_file_location <- list.files(paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","AI_temp"))
	  
	  if(length(all_df_file_location) > 0){
	     file.remove(paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","AI_temp/",all_df_file_location)) 
	  }
      current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
	  
	  if(all_polygon_df_file_location_number > 0){
        modified_polygon_list <- Polygons_data_current_on_map$list[[current_polygon_class]]
        polygon_names <- Polygons_data_current_on_map$list[[current_polygon_class]] %>% names()
	  }else{
	    modified_polygon_list <- list()
	    Polygons_data_current_on_map$list <- list()
	  }
	  
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
		
		
		if(modified_point_table$point_id[1] == 1 && length(modified_point_table$point_id) > 1){
	       modified_point_table <-  modified_point_table[-1,]
		   saveRDS(modified_point_table,paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","points.rds"))
		}else{
		  saveRDS(modified_point_table,paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","points.rds"))
		}
		
	    saveRDS(Polygons_data_current_on_map$list,paste0("www/Slide/RUN_AI/",model_for_report_list[count],"/","polygons.rds"))

		
		Polygons_data_current_on_map$list <- list()

        img_location <- img_location[-1]

        opened_slide$close()

      updateProgressBar(
        session = session,
        title =paste0("Updating result to image...","(",model_for_report_list[count],")"),
        id = "AI_pre_label_myprogress",
        value = 100
         )

	   }
   
   modified_label_table <- list()
   updateSelectInput(session = session, inputId = "Image_input", choices = model_for_report_list[count])

	#-----------------------------------------------

    future:::ClusterRegistry(action = "stop")
	plan(list(multisession, sequential))
	
	
	end <- Sys.time()
	result <- end - Start
	print(result)  
    
	mem_release()
	sess_close()  
    rm(list=ls(all=TRUE))
    .Internal(gc(verbose = getOption("verbose"), reset = TRUE, full = TRUE))
	

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

 #------------------------------------------------------------------------------------------------------------#
    Fast_slide_process_Run_AI <- reactiveValues(df = data.frame())
	Slide_temp_number <- ""
    datapath_temp_number <- ""
	
	Fast_slide_process_Run_AI$df <- data.frame("Slide" = Slide_temp_number,
	                                           "datapath" = datapath_temp_number,
                                              "new_name" = "",
                                              "Group_of_Slide" = "RUN_AI",
                                              stringsAsFactors = FALSE)		

    output$Fast_slide_file_table_Run_AI <- renderRHandsontable({
              rhandsontable(Fast_slide_process_Run_AI$df )%>% 
              hot_col(col = "Group_of_Slide", type = "dropdown", source = list.files("www/Slide/")) %>% 
              hot_col("Slide", readOnly = TRUE)

     })

 #------------------------------------------------------------------------------------------------------------#
 
}) 


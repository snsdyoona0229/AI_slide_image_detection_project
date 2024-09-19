observeEvent(input$upload_Slide_or_image, {
  updateSelectInput(session = session, inputId = "upload_Annotation_group", choices = list.files(paste0("www/", input$upload_Slide_or_image, "/")) %>% str_sort(numeric = TRUE))
})

#Upload_image_or_slide_file_ref <- reactiveValues(df = data.frame(),
#                                                 upload = 0)
#observeEvent(input$upload_image_or_slide_file, {

#  Upload_image_or_slide_file_ref$df <- input$upload_image_or_slide_file
#  Upload_image_or_slide_file_ref$df$new_name <- ""
#  Upload_image_or_slide_file_ref$upload <- 1 
#  as.data.frame(hot_to_r(input$hot))
#})
############
shinyFileChoose(input = input, 
                id = 'upload_image_or_slide_file', 
                roots = getVolumes()(),
                filetypes=c("png","jpeg","jpg","tif","tiff","bmp","ndpi","isyntax","svs","vms","vmu","scn","mrxs","tiff","svslide","bif")
)

Upload_image_or_slide_file_ref <- reactiveValues(df = data.table,
                                                 upload = 0)


observeEvent(input$upload_image_or_slide_file, {

  files_list <-NULL
  size <- NULL
  type <- NULL
  
  if (input$upload_image_or_slide_file[1] %>% class() == "list") {
    root_name <- getVolumes()()[input$upload_image_or_slide_file$root] %>% str_replace("\\/", "")
    choose_file <- sapply(input$upload_image_or_slide_file$files, function(x){
      paste0(root_name, unlist(x) %>% paste0(collapse = "/"))
    })

   for(ii in 1:length(choose_file)){  
     files_list[ii] <- strsplit(choose_file[ii], split = '/')[[1]][length(strsplit(choose_file[ii], split = '/')[[1]])] %>% str_replace(" ", "")
	 #size[ii] <- OS$path$getsize(choose_file[ii])
	 size[ii] <- file.info(choose_file[ii])$size
	 type[ii] <- input$upload_image_or_slide_file$files[[ii]][[6]]
	  }
	  
	  upload_image_or_slide_file <- data.table("name" = files_list,	                                        
                                              "size" = size,
											  "type" = type,
											  "datapath" = choose_file,
                                              stringsAsFactors = FALSE)
		
		
	    Upload_image_or_slide_file_ref$df <- upload_image_or_slide_file
        Upload_image_or_slide_file_ref$df$new_name <- ""
        Upload_image_or_slide_file_ref$upload <- 1
	  
										
  }
  
})

#############
observeEvent(input$covert_upload_file_to_location, {
  upload_data <- as.data.table(hot_to_r(input$Upload_file_table))
  image_number <- str_detect(upload_data$name, ".png|.jpeg|.jpg|.tif|.tiff|.PNG|.JPEG|.JPG|.TIF|.TIFF") %>% sum()
  slide_number <- str_detect(upload_data$name, ".ndpi|.isyntax|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif") %>% sum()
  if (input$upload_Slide_or_image == "Slide" & nrow(upload_data) != slide_number) {
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Your upload files are not all slides which are accepted by our system!",
      type = "error"
    )
  } else if (input$upload_Slide_or_image == "Image" & nrow(upload_data) != image_number) {
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Your upload files are not all images which are accepted by our system!",
      type = "error"
    )
  } else {
    
    progressSweetAlert(
      session = session, id = "generate_image_or_slice_file_myprogress",
      title = "Image or Slide preparation",
      display_pct = TRUE, value = 0
    )

    for (i in 1:nrow(upload_data)) {
      time_name <- Sys.time() %>% as.character() %>% str_replace_all(" ", "_") %>% str_replace_all(":", "-")
      file_name <- ""
      exist_name <- list.files(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group))
      if (str_detect(upload_data$name[i], ".jpg|.jpeg|.JPG|.JPEG")) {
        if (upload_data$new_name[i] == "") {
          file_name <- upload_data$name[i]
        } else {
          file_name <- paste0(upload_data$new_name[i], ".jpg")
        }
        file_name_no_jpg <- file_name %>% str_replace_all(".jpg|.jpeg|.JPG|.JPEG", "")
        if (file_name_no_jpg %in% exist_name) {
          file_name_no_jpg <- paste0(file_name_no_jpg, "_", time_name)
        }
        dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_jpg))
        image_raster <- raster(upload_data$datapath[i]) %>% dim()
        file_name <- paste0(file_name_no_jpg, "___", image_raster[2], "___", image_raster[1], ".jpg")
        #file.rename(upload_data$datapath[i], paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_jpg, "/", file_name))
		file.copy(upload_data$datapath[i],paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_jpg, "/", file_name), overwrite = TRUE)
		
      } else if (str_detect(upload_data$name[i], ".png|.tif|.tiff|.PNG|.TIF|.TIFF") & upload_data$size[i] < 25000000) {#25000000
        if (upload_data$new_name[i] == "") {
          file_name <- upload_data$name[i]
        } else {
          file_name <- paste0(upload_data$new_name[i], ".jpg")
        }
        file_name_no_jpg <- file_name %>% str_replace_all(".png|.tif|.tiff|.PNG|.TIF|.TIFF", "")
        if (file_name_no_jpg %in% exist_name) {
          file_name_no_jpg <- paste0(file_name_no_jpg, "_", time_name)
        }
        dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_jpg))
        image_raster <- raster(upload_data$datapath[i]) %>% dim()
        file_name <- paste0(file_name_no_jpg, "___", image_raster[2], "___", image_raster[1], ".jpg")
        image_data <- readImage(upload_data$datapath[i])
        writeImage(image_data, paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_jpg, "/", file_name))
      } else if (str_detect(upload_data$name[i], ".ndpi")) {#.vms|.vmu|
        output_list <- NDPI_info(file_name = upload_data$datapath[i])
        image_size <- c(output_list$Width_pixel, output_list$Height_pixel)
        source_lens <- paste0(output_list$Lens, "x")
        output_list[["image_size"]] <- image_size
        output_list[["source_lens"]] <- source_lens
        
        if (upload_data$new_name[i] == "") {
          file_name <- upload_data$name[i]
        } else {
          file_name <- paste0(upload_data$new_name[i], ".ndpi")
        }
#        file_name_no_ndpi <- file_name %>% str_replace_all(".ndpi", "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-")
        file_name_no_ndpi <- file_name %>% str_replace_all(".ndpi", "") %>% str_replace_all(" ", "-")
        if (file_name_no_ndpi %in% exist_name) {
          file_name_no_ndpi <- paste0(file_name_no_ndpi, "_", time_name)
        }
        
        dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi))
        dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/info"))
        dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/temp"))
        file_name <- paste0(file_name_no_ndpi, "___", image_size[1], "___", image_size[2], "___", source_lens, ".ndpi")
        #file.rename(upload_data$datapath[i], paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/", file_name))
		file.copy(upload_data$datapath[i], paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/", file_name), overwrite = TRUE)
		
        saveRDS(output_list, paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/info/info.rds"))
        
        NDPI_save_slide_preview(file_name = paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/", file_name),
                                output_name = paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/info/SlideImage.jpg"))
        
        slide_info <- readRDS(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/info/info.rds"))
        
        run_list <- list()
        
        #z 5-12
        for (z in 5:2) {
          x_size <- slide_info$image_size[1]/(256*(2^(12-z)))
          y_size <- slide_info$image_size[2]/(256*(2^(12-z)))
          for (x in 0:x_size) {
            for (y in 0:y_size) {
              run_list[[paste0(x, "-", y, "-", z)]] <- c(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/", file_name), 
                                                         paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/temp/", x, "_", y, "_", z, "minimap.jpg"), 
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
      } else if (str_detect(upload_data$name[i], ".svs|.vms|.vmu|.tif|.scn|.mrxs|.tiff|.svslide|.bif")) {

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
		current_file_name <- str_replace_all(upload_data$name[i],current_format,"")
		current_root <- str_replace_all(upload_data$datapath[i],upload_data$name[i],"")
		
		###path_setting
        opened_slide <- openslide$OpenSlide(upload_data$datapath[i])
        tile_generator <- openslide$deepzoom$DeepZoomGenerator(osr = opened_slide,
                                                               tile_size = as.integer(256), 
                                                               overlap = as.integer(0))											   

        

	   tile_table <- tile_generator$level_dimensions %>% unlist() %>% matrix(nrow = 2) 
	   tile_table <- as.data.table(tile_table)%>% transpose()

        output_list$image_size <- tile_table[nrow(tile_table),] %>% as.integer()
        output_list$Width_pixel <- tile_table[nrow(tile_table),1] %>% as.integer()
        output_list$Height_pixel <- tile_table[nrow(tile_table),2] %>% as.integer()
      
		 
        if (upload_data$new_name[i] == "") {
          file_name <- upload_data$name[i]
        } else {
          file_name <- paste0(upload_data$new_name[i], current_format)
        }
        file_name_no_ndpi <- file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-")
        if (file_name_no_ndpi %in% exist_name) {
          file_name_no_ndpi <- paste0(file_name_no_ndpi, "_", time_name)
        }
		
		###########
		if(str_detect(upload_data$name[i], ".svs|.vmu|.tif|.scn|.tiff|.svslide|.bif")){
           dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi))
		}
		if(str_detect(upload_data$name[i], ".vms")){
		  
          datapath_string_path_split <- strsplit(upload_data$datapath[i],"/")	
          file_root_localtion <- str_replace_all(upload_data$datapath[i],datapath_string_path_split[[1]][length(datapath_string_path_split[[1]])],"")		  
		  dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi))
		  move_file_to_destination <- paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi)

		  for(vms_file in dir(file_root_localtion)){
		      if(str_detect(vms_file,".vms")==FALSE){

			    file.copy(paste0(file_root_localtion,"/",vms_file),paste0(move_file_to_destination,"/",vms_file),overwrite = TRUE)
			 }
		   } 
		}
	    #---#
		if(str_detect(upload_data$name[i], ".mrxs")){
		   root_mrxs <- current_file_name %in% dir(current_root) %>% sum()

           if(root_mrxs == 1){
		      if(file.exists(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-") ))==TRUE){

                current_file_modify_duplicate <- paste0(current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-"),"_", time_name)			   
			  
                dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-"), "_", time_name))
				
                dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/",current_file_modify_duplicate,"/",current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-"),"_", time_name, "___", output_list$Width_pixel, "___", output_list$Height_pixel, "___", output_list$source_lens))
				
                select_folder <- paste0(current_root,"/",current_file_name)
				destination_folder <- paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/",current_file_modify_duplicate,"/",current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-"),"_", time_name, "___", output_list$Width_pixel, "___", output_list$Height_pixel, "___", output_list$source_lens)
				
				
				for(d in dir(select_folder)){
				   file.copy(paste0(select_folder,"/",d),paste0(destination_folder,"/",d),overwrite = TRUE)
				}
				
			  }else{
			    current_file_modify <- current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-")
				
			   	dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/",current_file_modify ))
				dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/",current_file_modify,"/",current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-"), "___", output_list$Width_pixel, "___", output_list$Height_pixel, "___", output_list$source_lens))
				
                select_folder <- paste0(current_root,"/",current_file_name)
				destination_folder <- paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/",current_file_modify,"/",current_file_name %>% str_replace_all(current_format, "") %>% str_replace_all("[[:punct:]]", "_") %>% str_replace_all(" ", "-"), "___", output_list$Width_pixel, "___", output_list$Height_pixel, "___", output_list$source_lens)
				
				for(d in dir(select_folder)){
				   file.copy(paste0(select_folder,"/",d),paste0(destination_folder,"/",d),overwrite = TRUE)
				}

             }			  
           }		   
		}
		###########

		
        dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/info"))
        dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/temp"))
		dir.create(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/AI_temp"))
        
        saveRDS(output_list, paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/info/info.rds"))

		
        try(
          opened_slide$associated_images['macro']$convert(mode = "RGB")$save(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/info/SlideImage.jpg"), format="JPEG"),
          silent = TRUE
        )
        
	   tile_table_sapply <- tile_generator$level_tiles %>% unlist() %>% matrix(nrow = 2)
	   tile_table_sapply <- as.data.table(tile_table_sapply)%>% transpose()

	   
        last_z_level <- nrow(tile_table_sapply) - 7
        
        run_list <- NULL
        
        file_name <- paste0(file_name_no_ndpi, "___", output_list$Width_pixel, "___", output_list$Height_pixel, "___", output_list$source_lens, current_format)
		
        for (z in last_z_level:(last_z_level-3)) {
          for (x in 1:tile_table_sapply[z,1]) {
            for (y in 1:tile_table_sapply[z,2]) {
              run_list[[paste0(x, "-", y, "-", z)]] <- c(paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/", file_name), 
                                                         paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/temp/", x - 1, "_", y - 1, "_", 5 - last_z_level + z, "minimap.jpg"), 
                                                         x - 1, 
                                                         y - 1, 
                                                         z - 1)
            }
          }
        }

        sapply(run_list, function(tiling){
          blank_image <- PIL$Image$new("RGB", size = c(256,256) %>% as.integer())
          tile_data <- tile_generator$get_tile(level = abs(as.integer(tiling[5])),
                                               address = as.integer(c(tiling[3], tiling[4])))

          
          blank_image$paste(im = tile_data)
          blank_image$save(tiling[2], format="JPEG")
        })
        #opened_slide$close()
        #file.rename(upload_data$datapath[i], paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/", file_name))
        file.copy(upload_data$datapath[i],paste0("www/", input$upload_Slide_or_image, "/", input$upload_Annotation_group, "/", file_name_no_ndpi, "/", file_name), overwrite = TRUE)
      }
      updateProgressBar(
        session = session,
        id = "generate_image_or_slice_file_myprogress",
        value = i * 1/nrow(upload_data) * 100
      )  
    } 
    
    closeSweetAlert(session = session)
    Upload_image_or_slide_file_ref$upload <- 0
    sendSweetAlert(
      session = session,
      title = "Upload complete!!",
      text = "The upload files are all in position and ready to use!",
      type = "success"
    )
  }
})

output$Upload_file_table <- renderRHandsontable({
  rhandsontable(Upload_image_or_slide_file_ref$df)
})

output$cover_file_to_location_button <- renderUI({
  if (Upload_image_or_slide_file_ref$upload == 1) {
    actionBttn(
      inputId = "covert_upload_file_to_location",
      label = "Covert file to location",
      style = "fill", 
      color = "primary"
    )
  }
})




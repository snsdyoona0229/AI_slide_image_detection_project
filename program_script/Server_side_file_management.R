observeEvent(input$file_management_group, {

  slide_file <- list.files(paste0("www/Slide/", input$file_management_group))
  slide_file <- slide_file[str_detect(slide_file, ".rds", negate = TRUE)]
  updateSelectInput(session = session, inputId = "file_management_group", choices = list.files("www/Slide"), selected = input$file_management_group)
  updateSelectInput(session = session, inputId = "file_management_slide", choices = slide_file)
})

observeEvent(input$customize_AI_model_viewer_switch, {

leafletProxy("image_viewer", session) %>% 
      clearMarkers()
leafletProxy("image_viewer", session) %>% 
    clearGeoJSON() 

   if(input$Slide_or_image == "Slide"){
     
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Slide")
    updateSelectInput(session, inputId = "Annotation_group", choices = "RUN_AI")
    updateSelectInput(session, inputId = "Image_input", choices = list.files(paste0("www/Slide/RUN_AI/")) %>% 
                       grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
                       str_sort(numeric = TRUE))
						

	if(length(list.files(paste0("www/Slide/RUN_AI/")) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE)) == 0){
	
	    source("program_script/Server_side_leaflet_slide_image_viewer.R", local = TRUE)

      }
    }
})

observeEvent(input$move, {
        
		if(input$move == TRUE){
		    updateCheckboxInput(session, "copy", value = FALSE)
        }
        if(input$move == FALSE){
		    updateCheckboxInput(session, "copy", value = TRUE)
        }
		
})

observeEvent(input$copy, {
        
		if(input$copy == TRUE){
		    updateCheckboxInput(session, "move", value = FALSE)
        }
        if(input$copy == FALSE){
		    updateCheckboxInput(session, "move", value = TRUE)
        }  	  		
})

observeEvent(input$file_management_start, {


    new_df <- as.data.frame(hot_to_r(input$file_management_slide_table))
	
	error_destination_address <- c()
	slide_file_ALL <- list.files(paste0("www/Slide/", input$file_management_group)) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE)
	slide_folder <- list.files(paste0("www/Slide/"))
	#-------------check out-------------#
	for( x in 1:length(new_df$file_name)){
     
	 if ((new_df$select_file_destination[x] == "NULL")){
	     error_destination_address[length(error_destination_address)+1] <- new_df$file_name[x]
      }
	    
	}

	
   if(length(setdiff(new_df$slide_name,input$file_management_group)) == 0 & length(setdiff(new_df$select_file_destination,slide_folder)) == 0 &  length(setdiff(new_df$file_name,slide_file_ALL)) == 0 & length(setdiff(new_df$slide_name,new_df$select_file_destination)) > 0 ){
	      
		  
    if(length(error_destination_address) > 0 ){

          sendSweetAlert(
              session = session,
              title = paste0("file address non-existent or not filled the file address : ",str_c(error_destination_address, collapse = ",") ),
              type = "error"
           )
		   
#		   new_df$other_file_destination_address[1:length(new_df$other_file_destination_address)] <- "NULL"
		   
		   #---------------
#		     rhandsontable(new_df, width = "100%", height = "100%")%>% 
#               hot_col(col = "select_file_destination", type = "dropdown", source = list.files("www/Slide/"))%>%	  
#	           hot_cols(renderer = "
#                   function (instance, td, row, col, prop, value, cellProperties) {
#                   Handsontable.renderers.NumericRenderer.apply(this, arguments);
#                   td.style.background = 'white';
#                   td.style.color = 'black';

#               }")
		   
		   
		   #---------------

    }else{	
	
	#-------------check out-------------#
	
    withProgress(message = 'Now processing', value = 0, {

	#-------------processing-------------#
	
	
	for( i in 1:length(new_df$file_name)){

      
	 incProgress(1/length(new_df$file_name), detail = paste("Now processing:", new_df$file_name[i]))

	     if(new_df$select_file_destination[i] != "NULL" && input$copy == TRUE ){
	 
#	         file.copy(paste0("www/Slide/",input$file_management_group,"/",new_df$file_name[i]),paste0("www/Slide/",new_df$select_file_destination[i]),recursive=TRUE)
			 
			select_folder <- paste0("./www/Slide/",input$file_management_group,"/",new_df$file_name[i])
			destination_folder <- paste0("./www/Slide/",new_df$select_file_destination[i],"/",new_df$file_name[i])
			
			py_run_string("import shutil")
			py_run_string("import os")

		   if(dir.exists(paste0("www/Slide/",new_df$select_file_destination[i],"/",new_df$file_name[i])) == TRUE){

			   #py_run_string(glue("new_path = shutil.copytree('{select_folder}','{destination_folder}',dirs_exist_ok=True)"))
		   }
		   
		   if(dir.exists(paste0("www/Slide/",new_df$select_file_destination[i],"/",new_df$file_name[i])) == FALSE){
			   py_run_string(glue("new_path = shutil.copytree('{select_folder}','{destination_folder}')"))

			}
			#py_run_string("new_path = shutil.copy('www/Slide/LN/CLN_001-','www/Slide/Demo')")
			#py_run_string("new_path") 
          }

		  if(input$move == TRUE){
		  
		  
		    select_folder <- paste0("./www/Slide/",input$file_management_group,"/",new_df$file_name[i])
			destination_folder <- paste0("./www/Slide/",new_df$select_file_destination[i])

			
			if(dir.exists(paste0("www/Slide/",new_df$select_file_destination[i],"/",new_df$file_name[i])) == TRUE){

			   #py_run_string(glue("new_path = shutil.copytree('{select_folder}','{destination_folder}',dirs_exist_ok=True)"))
		   
		   }
			
			if(dir.exists(paste0("www/Slide/",new_df$select_file_destination[i],"/",new_df$file_name[i])) == FALSE){
			
               py_run_string(glue("new_path = shutil.move('{select_folder}','{destination_folder}')"))
		    }

		      #print("move")
              #unlink(paste0("www/Slide/",input$file_management_group,"/",new_df$file_name[i]), recursive=TRUE)
          }	

	 }
	 

	#-------------processing-------------#
	
	})
	closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title =" Processing completed !",
      type = "success"
    )
  slide_file <- list.files(paste0("www/Slide/", input$file_management_group))
  slide_file <- slide_file[str_detect(slide_file, ".rds", negate = TRUE)]
  updateSelectInput(session = session, inputId = "file_management_group", choices = list.files("www/Slide"), selected = input$file_management_group)
  updateSelectInput(session = session, inputId = "file_management_slide", choices = slide_file)
  
  
    slide_updata <- list.files(paste0("www/Slide/", input$Annotation_group))
    slide_updata <- slide_updata[str_detect(slide_updata, ".rds", negate = TRUE)]
  
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Slide")
    updateSelectInput(session, inputId = "Annotation_group", choices = list.files("www/Slide"), selected = input$Annotation_group)
    updateSelectInput(session, inputId = "Image_input", choices =slide_updata)
	
    source("program_script/Server_side_leaflet_slide_image_viewer.R", local = TRUE)	
	
	if(length(list.files(paste0("www/Slide/RUN_AI/")) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE)) == 0){
	
	    source("program_script/Server_side_leaflet_slide_image_viewer.R", local = TRUE)

      }


  }
}else{

    sendSweetAlert(
              session = session,
              title = "You filled out the form incorrectly",
              type = "error"
           )
   }


})

observeEvent(input$file_management_remove, {


    delete <- as.data.frame(hot_to_r(input$remove_slide_table))
	slide_file_ALL <- list.files(paste0("www/Slide/", input$file_management_group)) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE)
  
    if(length(setdiff(delete$slide_name,input$file_management_group)) == 0 &  length(setdiff(delete$remove_file_name,slide_file_ALL)) == 0 ){
  
      withProgress(message = 'Now processing', value = 0, {
   
      for( j in 1:length(delete$remove_file_name)){
      
	     incProgress(1/length(delete$remove_file_name), detail = paste("Now processing:", delete$remove_file_name[j]))
         unlink(paste0("www/Slide/",input$file_management_group,"/",delete$remove_file_name[j]), recursive=TRUE)
	   }
	})

	   closeSweetAlert(session = session)
       sendSweetAlert(
       session = session,
       title =" Remove completed !",
       type = "success"
    )
	
     slide_file <- list.files(paste0("www/Slide/", input$file_management_group))
     slide_file <- slide_file[str_detect(slide_file, ".rds", negate = TRUE)]
     updateSelectInput(session = session, inputId = "file_management_group", choices = list.files("www/Slide"), selected = input$file_management_group)
     updateSelectInput(session = session, inputId = "file_management_slide", choices = slide_file)
	 
    slide_updata <- list.files(paste0("www/Slide/", input$Annotation_group))
    slide_updata <- slide_updata[str_detect(slide_updata, ".rds", negate = TRUE)]
  
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Slide")
    updateSelectInput(session, inputId = "Annotation_group", choices = list.files("www/Slide"), selected = input$Annotation_group)
    updateSelectInput(session, inputId = "Image_input", choices =slide_updata) 

if(length(list.files(paste0("www/Slide/RUN_AI/")) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE)) == 0){
	
	    source("program_script/Server_side_leaflet_slide_image_viewer.R", local = TRUE)

      }
	 
	
}else{

    sendSweetAlert(
              session = session,
              title = "You filled out the form incorrectly",
              type = "error"
           )
   }	
  
})

#other_file_destination_address

output$file_management_slide_table <- rhandsontable::renderRHandsontable({
  

  if(length(input$file_management_slide) == 0 & input$All_group_files == FALSE){ 

      file_management_slide_table =  data.frame(slide_name = input$file_management_group,
	                                           file_name = "NULL",
	                                           select_file_destination = "NULL"
                                             
	                                          )
    }
  if(length(input$file_management_slide) > 0 & input$All_group_files == FALSE){

     file_management_slide_table = data.frame(slide_name = input$file_management_group,
	                                          file_name = input$file_management_slide,
	                                          select_file_destination = "NULL"
                                         )
    }
   
  if(input$All_group_files == TRUE){
      
	  slide_file_ALL <- list.files(paste0("www/Slide/", input$file_management_group)) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE)
	  updateSelectInput(session = session, inputId = "file_management_slide", choices = slide_file_ALL, selected = slide_file_ALL)
	  
	  if(length(slide_file_ALL) > 0){
	  
	    file_management_slide_table =  data.frame(slide_name = input$file_management_group,
		                                      file_name = slide_file_ALL,
	                                          select_file_destination = "NULL"
										
											  )
	    }else{
		
		   file_management_slide_table = data.frame(slide_name = input$file_management_group,
		                                       file_name = "NULL",
	                                          select_file_destination = "NULL"
										
											  )
		
		}
   }

rhandsontable(file_management_slide_table, width = "100%", height = "100%")%>% 
      hot_col(col = "select_file_destination", type = "dropdown", source = list.files("www/Slide/"))%>%  
	  hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = 'white';
              td.style.color = 'black';

           }")
     

})

output$remove_slide_table<- rhandsontable::renderRHandsontable({

  if(length(input$file_management_slide) == 0 & input$All_group_files == FALSE){ 

      remove_slide_table = data.frame(slide_name = input$file_management_group,
	                                  remove_file_name = "NULL")                                       
    }
  if(length(input$file_management_slide) > 0 & input$All_group_files == FALSE){ 

      remove_slide_table = data.frame(slide_name = input$file_management_group,
	                                  remove_file_name = input$file_management_slide )                                          
    }
	if(input$All_group_files == TRUE){
      
	  slide_file_ALL <- list.files(paste0("www/Slide/", input$file_management_group)) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE)
	  updateSelectInput(session = session, inputId = "file_management_slide", choices = slide_file_ALL, selected = slide_file_ALL)
	  
	    if(length(slide_file_ALL) > 0){
		
	      remove_slide_table = data.frame(slide_name = input$file_management_group,
		                                  remove_file_name = slide_file_ALL)
	    }else{
		
		   remove_slide_table = data.frame(slide_name = input$file_management_group,
		                                   remove_file_name = "NULL")
		
		}                                       
   }  

rhandsontable(remove_slide_table,width = "100%", height = "100%")%>% 
    hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = 'white';
              td.style.color = 'black';
			  
           }")  
})


observeEvent(input$side_folder_managemen, {

  slide_file <- list.files(paste0("www/Slide/", input$file_management_group))
  slide_file <- slide_file[str_detect(slide_file, ".rds", negate = TRUE)]
  updateSelectInput(session = session, inputId = "file_management_group", choices = list.files("www/Slide"), selected = input$file_management_group)
  updateSelectInput(session = session, inputId = "file_management_slide", choices = slide_file)

})
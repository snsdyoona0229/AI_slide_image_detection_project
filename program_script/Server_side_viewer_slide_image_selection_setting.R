observeEvent(input$Slide_or_image, {


  if (input$Select_function_to_use != "Run AI") {

  
    if (current_user_autherization$admin) {
      updateSelectInput(session = session, inputId = "Annotation_group", choices = list.files(paste0("www/", input$Slide_or_image, "/")) %>% str_sort(numeric = TRUE))
      updateSelectInput(session = session, inputId = "Image_input", choices = list.files(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/")) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE))
    } else {
      updateSelectInput(session = session, inputId = "Annotation_group", choices = intersect(list.files(paste0("www/", input$Slide_or_image, "/")), current_user_autherization$load_list[[input$Slide_or_image]] %>% names()) %>% str_sort(numeric = TRUE))
      updateSelectInput(session = session, inputId = "Image_input", choices = intersect(list.files(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/")) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE),
                                                                                        current_user_autherization$load_list[[input$Slide_or_image]][[input$Annotation_group]]) %>% str_sort(numeric = TRUE))
    }
    
    if (input$Slide_or_image == "Slide") {
      if (file.exists(paste0("www/Slide/",
                             input$Annotation_group,
                             "/",
                             input$Image_input,
                             "/temp/0_0_5.jpg"))) {
        updateSelectInput(session = session, 
                          inputId = "Slide_viewer_option", 
                          selected = "Tile")
      } else {
        updateSelectInput(session = session, 
                          inputId = "Slide_viewer_option", 
                          selected = "JPG")
      }
    }
 }#
  
})

observeEvent(input$Annotation_group, {


 if (input$Select_function_to_use != "Run AI") {

    if (current_user_autherization$admin) {
      updateSelectInput(session = session, inputId = "Image_input", choices = list.files(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/")) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE))
    } else {
      updateSelectInput(session = session, inputId = "Image_input", choices = intersect(list.files(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/")) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE),
                                                                                        current_user_autherization$load_list[[input$Slide_or_image]][[input$Annotation_group]]) %>% str_sort(numeric = TRUE))
    }
    
    if (input$Slide_or_image == "Slide") {
      if (file.exists(paste0("www/Slide/",
                             input$Annotation_group,
                             "/",
                             input$Image_input,
                             "/temp/0_0_5.jpg"))) {
        updateSelectInput(session = session, 
                          inputId = "Slide_viewer_option", 
                          selected = "Tile")
      } else {
        updateSelectInput(session = session, 
                          inputId = "Slide_viewer_option", 
                          selected = "JPG")
      }
    }
  } #
  
})

observeEvent(input$Image_input, {

  if (input$Slide_or_image == "Slide") {
    if (file.exists(paste0("www/Slide/",
                           input$Annotation_group,
                           "/",
                           input$Image_input,
                           "/temp/0_0_5.jpg"))) {
      updateSelectInput(session = session, 
                        inputId = "Slide_viewer_option", 
                        selected = "Tile")
    } else {
      updateSelectInput(session = session, 
                        inputId = "Slide_viewer_option", 
                        selected = "JPG")
    }
  }

  
})

observeEvent(input$Select_function_to_use, {

 
leafletProxy("image_viewer", session) %>% 
      clearMarkers()
leafletProxy("image_viewer", session) %>% 
    clearGeoJSON() 
	
	
	
  if (input$Select_function_to_use == "Run AI" &  length(input$Select_customuze_AI_to_use) > 0  ) {
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Image")
    updateSelectInput(session, inputId = "Annotation_group", choices = "AI_FISH_HER2")
    updateSelectInput(session, inputId = "Image_input", choices = list.files(paste0("www/Image/AI_FISH_HER2/")) %>% 
                        grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
                       str_sort(numeric = TRUE)
    )


  }

  if (input$Select_function_to_use == "Viewer" | input$Select_function_to_use == "Annotation" ) {

    updateSelectInput(session, inputId = "Slide_or_image", choices = base::c("Slide", "Image"))
    if (current_user_autherization$admin) {
      updateSelectInput(session = session, inputId = "Annotation_group", choices = list.files(paste0("www/", input$Slide_or_image, "/")) %>% str_sort(numeric = TRUE))
      updateSelectInput(session = session, inputId = "Image_input", choices = list.files(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/")) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE))
    } else {
      updateSelectInput(session = session, inputId = "Annotation_group", choices = intersect(list.files(paste0("www/", input$Slide_or_image, "/")), current_user_autherization$load_list[[input$Slide_or_image]] %>% names()) %>% str_sort(numeric = TRUE))
      updateSelectInput(session = session, inputId = "Image_input", choices = intersect(list.files(paste0("www/", input$Slide_or_image, "/", input$Annotation_group, "/")) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE),
                                                                                       current_user_autherization$load_list[[input$Slide_or_image]][[input$Annotation_group]]) %>% str_sort(numeric = TRUE))
    }

  }
  
})


 


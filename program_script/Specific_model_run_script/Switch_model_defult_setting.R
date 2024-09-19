observeEvent(input$Select_customuze_AI_to_use, {
  Run_AI_model$model_result <- list()
  
  #print("good_morning01")

  saveRDS("good_morning01",file = "www/Select_customuze_AI_to_use.RDS")
 # interface_information <- readRDS("www/interface_information.RDS")

  
  if (input$Select_customuze_AI_to_use == 'FISH/HER2') {
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Image")
    updateSelectInput(session, inputId = "Annotation_group", choices = "AI_FISH_HER2")
    updateSelectInput(session, inputId = "Image_input", choices = list.files(paste0("www/Image/AI_FISH_HER2/")) %>% 
                        grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
                        str_sort(numeric = TRUE)
    )
    updateOrderInput(session = session,
                     inputId = "Change_point_color_order",
                     items = base::c("red", "green", "blue", "yellow", "black", "white", "purple", "cyan", "magenta", "orange"))
    updateSwitchInput(session = session, 
                      inputId = "Point_circle_switch", 
                      value = FALSE)
    updateSwitchInput(session = session, 
                      inputId = "Polygon_opacity_on_off", 
                      value = TRUE)
    updateSelectInput(session = session,
                      inputId = "Polygon_viewing_color", 
                      selected = "yellow")
  } else if (input$Select_customuze_AI_to_use == 'FISH/1p 19q') {
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Image")
    updateSelectInput(session, inputId = "Annotation_group", choices = "AI_FISH_1p_19q")
    updateSelectInput(session, inputId = "Image_input", choices = list.files(paste0("www/Image/AI_FISH_1p_19q/")) %>% 
                        grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
                        str_sort(numeric = TRUE)
    )
    updateOrderInput(session = session,
                     inputId = "Change_point_color_order",
                     items = base::c("red", "green", "blue", "yellow", "black", "white", "purple", "cyan", "magenta", "orange"))
    updateSwitchInput(session = session, 
                      inputId = "Point_circle_switch", 
                      value = FALSE)
    updateSwitchInput(session = session, 
                      inputId = "Polygon_opacity_on_off", 
                      value = TRUE)
    updateSelectInput(session = session,
                      inputId = "Polygon_viewing_color", 
                      selected = "yellow")
  } else if (input$Select_customuze_AI_to_use == 'FISH/CDK2 MDM4') {
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Image")
    updateSelectInput(session, inputId = "Annotation_group", choices = "AI_FISH_CDK2_MDM4")
    updateSelectInput(session, inputId = "Image_input", choices = list.files(paste0("www/Image/AI_FISH_CDK2_MDM4/")) %>% 
                        grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
                        str_sort(numeric = TRUE)
    )
    updateOrderInput(session = session,
                     inputId = "Change_point_color_order",
                     items = base::c("red", "green", "blue", "yellow", "black", "white", "purple", "cyan", "magenta", "orange"))
    updateSwitchInput(session = session, 
                      inputId = "Point_circle_switch", 
                      value = FALSE)
    updateSwitchInput(session = session, 
                      inputId = "Polygon_opacity_on_off", 
                      value = TRUE)
    updateSelectInput(session = session,
                      inputId = "Polygon_viewing_color", 
                      selected = "yellow")
  }else if (input$Select_customuze_AI_to_use == 'Colon LN/cancer detection') {
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Slide")
    updateSelectInput(session, inputId = "Annotation_group", choices = "RUN_AI")
    updateSelectInput(session, inputId = "Image_input", choices = list.files(paste0("www/Slide/RUN_AI/")) %>% 
                        grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
                        str_sort(numeric = TRUE)
    )
    updateOrderInput(session = session,
                     inputId = "Change_point_color_order",
                     items = base::c("red", "green", "blue", "yellow", "black", "white", "purple", "cyan", "magenta", "orange"))
    updateSwitchInput(session = session, 
                      inputId = "Point_circle_switch", 
                      value = FALSE)
    updateSwitchInput(session = session, 
                      inputId = "Polygon_opacity_on_off", 
                      value = TRUE)
    updateSelectInput(session = session,
                      inputId = "Polygon_viewing_color", 
                      selected = "blue")
	
  } else if (input$Select_customuze_AI_to_use == 'Gastric_biopsy') {
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Slide")
    updateSelectInput(session, inputId = "Annotation_group", choices = "RUN_AI")
    updateSelectInput(session, inputId = "Image_input", choices = list.files(paste0("www/Slide/RUN_AI/")) %>% 
                        grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
                        str_sort(numeric = TRUE)
    )
    updateOrderInput(session = session,
                     inputId = "Change_point_color_order",
                     items = base::c("red", "green", "blue", "yellow", "black", "white", "purple", "cyan", "magenta", "orange"))
    updateSwitchInput(session = session, 
                      inputId = "Point_circle_switch", 
                      value = FALSE)
    updateSwitchInput(session = session, 
                      inputId = "Polygon_opacity_on_off", 
                      value = TRUE)
    updateSelectInput(session = session,
                      inputId = "Polygon_viewing_color", 
                      selected = "blue")
  } else if (input$Select_customuze_AI_to_use == 'Gastric_biopsy') {
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Slide")
    updateSelectInput(session, inputId = "Annotation_group", choices = "RUN_AI")
    updateSelectInput(session, inputId = "Image_input", choices = list.files(paste0("www/Slide/RUN_AI/")) %>% 
                        grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
                        str_sort(numeric = TRUE)
    )
    updateOrderInput(session = session,
                     inputId = "Change_point_color_order",
                     items = base::c("red", "green", "blue", "yellow", "black", "white", "purple", "cyan", "magenta", "orange"))
    updateSwitchInput(session = session, 
                      inputId = "Point_circle_switch", 
                      value = FALSE)
    updateSwitchInput(session = session, 
                      inputId = "Polygon_opacity_on_off", 
                      value = TRUE)
    updateSelectInput(session = session,
                      inputId = "Polygon_viewing_color", 
                      selected = "blue")
  } else if (input$Select_customuze_AI_to_use == 'Mitosis') {
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Slide")
    updateSelectInput(session, inputId = "Annotation_group", choices = "RUN_AI")
    updateSelectInput(session, inputId = "Image_input", choices = list.files(paste0("www/Slide/RUN_AI/")) %>% 
                        grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
                        str_sort(numeric = TRUE)
    )
    updateOrderInput(session = session,
                     inputId = "Change_point_color_order",
                     items = base::c("red", "green", "blue", "yellow", "black", "white", "purple", "cyan", "magenta", "orange"))
    updateSwitchInput(session = session, 
                      inputId = "Point_circle_switch", 
                      value = FALSE)
    updateSwitchInput(session = session, 
                      inputId = "Polygon_opacity_on_off", 
                      value = TRUE)
    updateSelectInput(session = session,
                      inputId = "Polygon_viewing_color", 
                      selected = "blue")
  }
  #-----------------#
  
  else if (input$Select_customuze_AI_to_use == 'Sentiinel_LN') {
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Slide")
    updateSelectInput(session, inputId = "Annotation_group", choices = "RUN_AI")
    updateSelectInput(session, inputId = "Image_input", choices = list.files(paste0("www/Slide/RUN_AI/")) %>% 
                        grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
                        str_sort(numeric = TRUE)
    )
    updateOrderInput(session = session,
                     inputId = "Change_point_color_order",
                     items = base::c("red", "green", "blue", "yellow", "black", "white", "purple", "cyan", "magenta", "orange"))
    updateSwitchInput(session = session, 
                      inputId = "Point_circle_switch", 
                      value = FALSE)
    updateSwitchInput(session = session, 
                      inputId = "Polygon_opacity_on_off", 
                      value = TRUE)
    updateSelectInput(session = session,
                      inputId = "Polygon_viewing_color", 
                      selected = "blue")
  }
  
    else if (input$Select_customuze_AI_to_use == 'Glomeruli') {
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Slide")
    updateSelectInput(session, inputId = "Annotation_group", choices = "RUN_AI")
    updateSelectInput(session, inputId = "Image_input", choices = list.files(paste0("www/Slide/RUN_AI/")) %>% 
                        grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
                        str_sort(numeric = TRUE)
    )
    updateOrderInput(session = session,
                     inputId = "Change_point_color_order",
                     items = base::c("red", "green", "blue", "yellow", "black", "white", "purple", "cyan", "magenta", "orange"))
    updateSwitchInput(session = session, 
                      inputId = "Point_circle_switch", 
                      value = FALSE)
    updateSwitchInput(session = session, 
                      inputId = "Polygon_opacity_on_off", 
                      value = TRUE)
    updateSelectInput(session = session,
                      inputId = "Polygon_viewing_color", 
                      selected = "blue")
  }
  
   else if (input$Select_customuze_AI_to_use == 'Common') {
    updateSelectInput(session, inputId = "Slide_or_image", choices = "Slide")
    updateSelectInput(session, inputId = "Annotation_group", choices = "RUN_AI")
    updateSelectInput(session, inputId = "Image_input", choices = list.files(paste0("www/Slide/RUN_AI/")) %>% 
                        grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
                        str_sort(numeric = TRUE)
    )
    updateOrderInput(session = session,
                     inputId = "Change_point_color_order",
                     items = base::c("red", "green", "blue", "yellow", "black", "white", "purple", "cyan", "magenta", "orange"))
    updateSwitchInput(session = session, 
                      inputId = "Point_circle_switch", 
                      value = FALSE)
    updateSwitchInput(session = session, 
                      inputId = "Polygon_opacity_on_off", 
                      value = TRUE)
    updateSelectInput(session = session,
                      inputId = "Polygon_viewing_color", 
                      selected = "blue")
  }

    
  
})
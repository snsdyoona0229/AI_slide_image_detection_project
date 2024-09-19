observeEvent(input$Authorization_slide_group, {
  updateMultiInput(inputId = "Authorization_slide_for_user", 
                   session = session, 
                   choices = list.files(paste0("www/Slide/", input$Authorization_slide_group)) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE))
})

observeEvent(input$save_Authorization_slide, {
  
  slide_group <- input$Authorization_slide_group
  slide_list <- input$Authorization_slide_for_user
  user_name_list <- input$Authorization_slide_user
  
  
  if (input$view_all_slide_check) {
    for (i in 1:length(user_name_list)) {
      if (file.exists(paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))) {
        load_list <- readRDS(paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))
        
        all_list <- list.files("www/Slide") %>% lapply(function(x){
          paste0("www/Slide/", x) %>% list.files() %>% grep(pattern = '.rds', inv = TRUE, value = TRUE)
        })
        names(all_list) <- list.files("www/Slide")
        
        load_list$Slide <- all_list
        saveRDS(load_list, paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))
        sendSweetAlert(
          session = session,
          title = "Saved!!",
          text = "The authorization have been changed!",
          type = "success"
        )
      } else {
        load_list <- list("Slide" = list(),
                          "Image" = list())
        
        all_list <- list.files("www/Slide") %>% lapply(function(x){
          paste0("www/Slide/", x) %>% list.files() %>% grep(pattern = '.rds', inv = TRUE, value = TRUE)
        })
        names(all_list) <- list.files("www/Slide")
        
        load_list$Slide <- all_list
        
        saveRDS(load_list, paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))
        sendSweetAlert(
          session = session,
          title = "Saved!!",
          text = "The authorization have been changed!",
          type = "success"
        )
      }
    }
  } else {
    if (length(user_name_list) > 0) {
      for (i in 1:length(user_name_list)) {
        if (file.exists(paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))) {
          load_list <- readRDS(paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))
          load_list$Slide[[slide_group]] <- slide_list
          saveRDS(load_list, paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))
          sendSweetAlert(
            session = session,
            title = "Saved!!",
            text = "The authorization have been changed!",
            type = "success"
          )
        } else {
          load_list <- list("Slide" = list(),
                            "Image" = list())
          load_list$Slide[[slide_group]] <- slide_list
          saveRDS(load_list, paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))
          sendSweetAlert(
            session = session,
            title = "Saved!!",
            text = "The authorization have been changed!",
            type = "success"
          )
        }
      }
    }
  } 
  
})

observeEvent(input$Authorization_image_group, {
  updateMultiInput(inputId = "Authorization_image_for_user", 
                   session = session, 
                   choices = list.files(paste0("www/Image/", input$Authorization_image_group)) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE))
})

observeEvent(input$save_Authorization_image, {
  image_group <- input$Authorization_image_group
  image_list <- input$Authorization_image_for_user
  user_name_list <- input$Authorization_image_user
  
  if (input$view_all_image_check) {
    for (i in 1:length(user_name_list)) {
      if (file.exists(paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))) {
        load_list <- readRDS(paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))
        
        all_list <- list.files("www/Image") %>% lapply(function(x){
          paste0("www/Image/", x) %>% list.files() %>% grep(pattern = '.rds', inv = TRUE, value = TRUE)
        })
        names(all_list) <- list.files("www/Image")
        
        load_list$Image <- all_list
        saveRDS(load_list, paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))
        sendSweetAlert(
          session = session,
          title = "Saved!!",
          text = "The authorization have been changed!",
          type = "success"
        )
      } else {
        load_list <- list("Slide" = list(),
                          "Image" = list())
        
        all_list <- list.files("www/Image") %>% lapply(function(x){
          paste0("www/Image/", x) %>% list.files() %>% grep(pattern = '.rds', inv = TRUE, value = TRUE)
        })
        names(all_list) <- list.files("www/Image")
        
        load_list$Image <- all_list
        
        saveRDS(load_list, paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))
        sendSweetAlert(
          session = session,
          title = "Saved!!",
          text = "The authorization have been changed!",
          type = "success"
        )
      }
    }
  } else {
    if (length(user_name_list) > 0) {
      for (i in 1:length(user_name_list)) {
        if (file.exists(paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))) {
          load_list <- readRDS(paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))
          load_list$Image[[image_group]] <- image_list
          saveRDS(load_list, paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))
          sendSweetAlert(
            session = session,
            title = "Saved!!",
            text = "The authorization have been changed!",
            type = "success"
          )
        } else {
          load_list <- list("Slide" = list(),
                            "Image" = list())
          load_list$Image[[image_group]] <- image_list
          saveRDS(load_list, paste0("account_and_login_data/Slide_Image_list/", user_name_list[i], ".rds"))
          sendSweetAlert(
            session = session,
            title = "Saved!!",
            text = "The authorization have been changed!",
            type = "success"
          )
        }
      }
    }
  }
})



observeEvent(input$delete_authorization, {
  if (is.null(input$Authorization_delete_user) == FALSE) {
    for (i in input$Authorization_delete_user) {
      file.remove(paste0("account_and_login_data/Slide_Image_list/", i))
    }
    
    updateSelectInput(inputId = "Authorization_delete_user", choices = list.files("account_and_login_data/Slide_Image_list"))
    
    sendSweetAlert(
      session = session,
      title = "Delete!!!",
      text = "The authorization have been deleted!",
      type = "success"
    )
  }
})
observeEvent(input$Defult_AI_prelabel_condition, {
  if (file.exists(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/detail.rds"))) {
    model_detail <- readRDS(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/detail.rds"))
    
    if (model_detail$image_or_slide == "Slide") {
      if (model_detail$slide_input_type == "Split train") {
        updateSelectizeInput(session, 
                             inputId = "AI_pre_label_in_slide_layer", 
                             choices = model_detail$slide_input_layer,
                             selected = model_detail$slide_input_layer[1],
                             options = list(maxItems = 1))
      } else {
        updateSelectizeInput(session, 
                             inputId = "AI_pre_label_in_slide_layer", 
                             choices = model_detail$slide_input_layer,
                             selected = model_detail$slide_input_layer,
                             options = list(maxItems = length(model_detail$slide_input_layer)))
      }
    } else {
      updateSelectizeInput(session, 
                           inputId = "AI_pre_label_in_slide_layer", 
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
    
    AI_pre_label$model_slide_image_condition <- model_detail$image_or_slide
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
  
  updateRadioGroupButtons(session, 
                          inputId = "AI_pre_label_train_or_validation",
                          selected = "By Validation Set")
  
  updateSwitchInput(session,
                    inputId = "Tissue_only_or_All_slide_switch",
                    value = TRUE)
  
  updateSliderInput(session,
                    inputId = "Tissue_only_dilate_pixel",
                    value = 5)
  
  updateSliderInput(session,
                    inputId = "Tissue_only_threshold",
                    value = 0.8)
  
  updateSelectInput(session,
                    inputId = "AI_pre_label_in_slide_tile_size",
                    selected = 1024)
  
  updateSelectInput(session,
                    inputId = "AI_pre_label_in_slide_overlap_pixel",
                    selected = 0)
  
  updateSelectInput(session,
                    inputId = "AI_pre_label_in_slide_batch_size",
                    selected = 1)
  
  updateSelectInput(session,
                    inputId = "AI_pre_label_in_slide_CPU_modify_batch_size",
                    selected = 16)
  
})



observeEvent(input$Save_AI_prelabel_condition, {
  showModal(modalDialog(
    title = "Save model condition settings!",
    size = "l",
    textInput("Save_model_conditon_settings_name",
              label = "Condition name",
              width = "100%"),
    actionBttn("Save_model_conditon_settings", 
               color = "success",
               label = "Save!")
  ))
})

observeEvent(input$Save_model_conditon_settings, {
  if (str_replace_all(input$Save_model_conditon_settings_name, "[^[:alnum:]]", "_") == "") {
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Condition name not accetable",
      type = "error"
    )
  } else {
    if (file.exists(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition")) == FALSE) {
      dir.create(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition"))
    }
    
    
    if (file.exists(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition/",
                           str_replace_all(input$Save_model_conditon_settings_name, "[^[:alnum:]]", "_"),
                           ".rds"))) {
      confirmSweetAlert(
        session = session,
        inputId = "Check_save_model_condition",
        title = "Save model condition and re-write previous condition?",
        btn_labels =  c("Cancel", "Save anyway!!"),
        btn_colors = c("#FE642E", "#04B404")
      )
    } else {
      confirmSweetAlert(
        session = session,
        inputId = "Check_save_model_condition",
        title = "Save model condition?",
        btn_labels =  c("Cancel", "Save anyway!!"),
        btn_colors = c("#FE642E", "#04B404")
      )
    }
  }
  
  
  #print(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition"))
  
  #print(input$AI_pre_label_train_or_validation)
  #print(input$AI_pre_label_in_slide_layer)
  #print(input$Tissue_only_or_All_slide_switch)
  #print(input$Tissue_only_dilate_pixel)
  #print(input$Tissue_only_threshold)
  #print(input$AI_pre_label_in_slide_tile_size)
  #print(input$AI_pre_label_in_slide_overlap_pixel)
  #print(input$AI_pre_label_in_slide_batch_size)
  #print(input$AI_pre_label_in_slide_CPU_modify_batch_size)
  #print(hot_to_r(input$AI_pre_label_table))
})

observeEvent(input$Check_save_model_condition, {
  if (input$Check_save_model_condition) {
    condition_list <- list(
      AI_pre_label_train_or_validation = input$AI_pre_label_train_or_validation,
      AI_pre_label_in_slide_layer = input$AI_pre_label_in_slide_layer,
      Tissue_only_or_All_slide_switch = input$Tissue_only_or_All_slide_switch,
      Tissue_only_dilate_pixel = input$Tissue_only_dilate_pixel,
      Tissue_only_threshold = input$Tissue_only_threshold,
      AI_pre_label_in_slide_tile_size = input$AI_pre_label_in_slide_tile_size,
      AI_pre_label_in_slide_overlap_pixel = input$AI_pre_label_in_slide_overlap_pixel,
      AI_pre_label_in_slide_batch_size = input$AI_pre_label_in_slide_batch_size,
      AI_pre_label_in_slide_CPU_modify_batch_size = input$AI_pre_label_in_slide_CPU_modify_batch_size,
      AI_pre_label_table = hot_to_r(input$AI_pre_label_table)
    )
    
    saveRDS(condition_list, paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition/",
                                   str_replace_all(input$Save_model_conditon_settings_name, "[^[:alnum:]]", "_"),
                                   ".rds"))
  }
})

observeEvent(input$Load_AI_prelabel_condition, {
  if (file.exists(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition")) == FALSE) {
    dir.create(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition"))
  }
  showModal(modalDialog(
    title = "Load model condition settings!",
    size = "l",
    selectInput("Load_model_conditon_settings_name",
                label = "Condition name",
                choices = list.files(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition")),
                width = "100%"),
    actionBttn("Load_model_conditon_settings", 
               color = "warning",
               label = "Load!")
  ))
})


observeEvent(input$Load_model_conditon_settings, {
  if (str_detect(input$Load_model_conditon_settings_name, ".rds", negate = TRUE)) {
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Condition name not found",
      type = "error"
    )
  } else {
    condition_list <- readRDS(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition/",
                                     input$Load_model_conditon_settings_name))
    
    
    updateRadioGroupButtons(session, 
                            inputId = "AI_pre_label_train_or_validation",
                            selected = condition_list$AI_pre_label_train_or_validation)
    
    updateSwitchInput(session,
                      inputId = "Tissue_only_or_All_slide_switch",
                      value = condition_list$Tissue_only_or_All_slide_switch)
    
    updateSliderInput(session,
                      inputId = "Tissue_only_dilate_pixel",
                      value = condition_list$Tissue_only_dilate_pixel)
    
    updateSliderInput(session,
                      inputId = "Tissue_only_threshold",
                      value = condition_list$Tissue_only_threshold)
    
    updateSelectInput(session,
                      inputId = "AI_pre_label_in_slide_tile_size",
                      selected = condition_list$AI_pre_label_in_slide_tile_size)
    
    updateSelectInput(session,
                      inputId = "AI_pre_label_in_slide_overlap_pixel",
                      selected = condition_list$AI_pre_label_in_slide_overlap_pixel)
    
    updateSelectInput(session,
                      inputId = "AI_pre_label_in_slide_batch_size",
                      selected = condition_list$AI_pre_label_in_slide_batch_size)
    
    updateSelectInput(session,
                      inputId = "AI_pre_label_in_slide_CPU_modify_batch_size",
                      selected = condition_list$AI_pre_label_in_slide_CPU_modify_batch_size)
    
    AI_pre_label$output_table <- condition_list$AI_pre_label_table
    
    updateSelectizeInput(session, 
                         inputId = "AI_pre_label_in_slide_layer", 
                         selected = condition_list$AI_pre_label_in_slide_layer)
    
    sendSweetAlert(
      session = session,
      title = "Load conditon successfully!",
      text = "The condition has been loaded.",
      type = "success"
    )
    
    #print(condition_list)
  }
})

observeEvent(input$Delete_AI_prelabel_condition, {
  if (file.exists(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition")) == FALSE) {
    dir.create(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition"))
  }
  showModal(modalDialog(
    title = "Delete model condition settings!",
    size = "l",
    selectInput("Delete_model_conditon_settings_name",
                label = "Condition name",
                choices = list.files(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition")),
                width = "100%"),
    actionBttn("Delete_model_conditon_settings", 
               color = "danger",
               label = "Delete!")
  ))
})


observeEvent(input$Delete_model_conditon_settings, {
  if (str_detect(input$Delete_model_conditon_settings_name, ".rds", negate = TRUE)) {
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Condition name not found",
      type = "error"
    )
  } else {
    confirmSweetAlert(
      session = session,
      inputId = "Check_delete_model_condition",
      title = "Delete model condition?",
      btn_labels =  c("Cancel", "Delete anyway!!"),
      btn_colors = c("#FE642E", "#04B404")
    )
  }
})


observeEvent(input$Check_delete_model_condition, {
  if (input$Check_delete_model_condition) {
    file.remove(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition/",
                       input$Delete_model_conditon_settings_name))
    
    updateSelectInput(session,
                      inputId = "Delete_model_conditon_settings_name",
                      choices = list.files(paste0("model/", input$AI_pre_label_model_group, "/", input$AI_pre_label_model, "/model_condition")))
    
    sendSweetAlert(
      session = session,
      title = "Delete conditon successfully!",
      text = "The condition has been deleted.",
      type = "success"
    )
  }
})
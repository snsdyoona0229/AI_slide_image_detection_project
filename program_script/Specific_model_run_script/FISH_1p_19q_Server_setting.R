output$AI_FISH_1p_files <- renderTable(input$AI_FISH_1p_upload[,1:3])
output$AI_FISH_19q_files <- renderTable(input$AI_FISH_19q_upload[,1:3])


observeEvent(input$AI_FISH_1p_19q_select_finished_result, {
  all_1p_19q_FISH_file <- list.files(paste0("www/Image/AI_FISH_1p_19q/")) %>% 
    grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
    str_sort(numeric = TRUE)
  all_1p_19q_FISH_file <- all_1p_19q_FISH_file[str_detect(all_1p_19q_FISH_file, input$AI_FISH_1p_19q_select_finished_result)]
  updateSelectInput(session, inputId = "Image_input", choices = all_1p_19q_FISH_file)
})


observeEvent(input$AI_FISH_1p_19q_start_analysis, {
  if (input$AI_FISH_1p_19q_case_name == "") {
    sendSweetAlert(
      session = session,
      title = "No case name or number is found!",
      text = "Please Enter Case name or number",
      type = "error"
    )
  } else {
    progressSweetAlert(
      session = session, id = "AI_FISH_1p_19q_myprogress",
      title = "Start AI analysis",
      display_pct = TRUE, value = 0
    )
    if (Run_AI_model$model_tag != "AI_FISH_1p_19q") {
      updateProgressBar(
        session = session,
        title = "Loading model...",
        id = "AI_FISH_1p_19q_myprogress",
        value = 10
      )
      Run_AI_model$model_option <- ""
      Run_AI_model$model_result <- list()
      source(file = "model_for_report/FISH_1p_19q/1p_19q_model/model_function.R", local = TRUE)
      Run_AI_model$model_tag <- "AI_FISH_1p_19q"
      Run_AI_model$model <- list()
      
      Run_AI_model$model$seg_model <- load_model_weights_hdf5(get_unet(num_classes = 1), 
                                                              filepath = "model_for_report/FISH_1p_19q/1p_19q_model/cell_border.h5")
      
      Run_AI_model$model$point_model <- load_model_weights_hdf5(get_unet(num_classes = 2), 
                                                                filepath = "model_for_report/FISH_1p_19q/1p_19q_model/point_model.h5")
    }
    
    source(file = "program_script/Specific_model_run_script/FISH_1p_19q_Function_setting.R", local = TRUE)
    
    number_check <- c(rep(1, 17), 
                      rep(1, 10),
                      rep(1, 10),
                      rep(1, 10),
                      rep(1, 11),
                      rep(1, 11),
                      rep(1, 10),
                      rep(1, 10),
                      rep(1, 10),
                      rep(1, 10),
                      rep(1, 10),
                      rep(1, 10),
                      rep(1, 10))
    
    updateProgressBar(
      session = session,
      title = "Environment initiation...",
      id = "AI_FISH_1p_19q_myprogress",
      value = 20
    )
    file_folder <- paste0("model_for_report/FISH_1p_19q/Report_history/", input$AI_FISH_1p_19q_case_name)
    
    dir.create(file_folder)
    
    
    
    
    #1p
    
    all_display_df <- display_df <- data.frame(
      red_number = 0,
      red_count = 0,
      green_count = 0,
      cell_area = 0,
      use_or_not = FALSE,
      stringsAsFactors = FALSE
    )
    
    all_display_df <- all_display_df[-1,]
    
    file_name <- input$AI_FISH_1p_upload$datapath
    #print(input$AI_FISH_HER2_upload$name)
    
    move_final_folder_name <- paste0("www/Image/AI_FISH_1p_19q/", input$AI_FISH_1p_19q_case_name, "_", input$AI_FISH_1p_upload$name %>% str_replace_all(".jpg|.png", ""), "_1p")
    
    if (file.exists("www/Image/AI_FISH_1p_19q") == FALSE) {
      dir.create("www/Image/AI_FISH_1p_19q")
    }
    #move_final_image_name <- paste0("www/Image/AI_FISH/", input$AI_FISH_HER2_case_name, "_", input$AI_FISH_HER2_upload$name %>% str_replace_all(".jpg|.png", ""), "/", input$AI_FISH_HER2_upload$name)
    
    #print(move_final_folder_name)
    #print(move_final_image_name)
    
    for (k in 1:length(file_name)) {
      updateProgressBar(
        session = session,
        title = paste0("Analysis image (1p): ", input$AI_FISH_1p_upload$name[k]),
        id = "AI_FISH_1p_19q_myprogress",
        value = round(20 + 60/length(file_name) * k)
      )
      
      image <- readImage(file_name[k])
      image_1 <- image %>% array_reshape(dim = c(1, dim(image)))
      point_result <- Run_AI_model$model$point_model$predict(image_1)
      seg_result <- Run_AI_model$model$seg_model$predict(image_1)
      pred_result_all <- abind(point_result, seg_result)
      pred_result_all <- (pred_result_all >= 0.5) * 1
      pred_result_all[1,,,3] <- pred_result_all[1,,,3] %>% fillHull()
      
      pred_result_all[1,,,1] <- pred_result_all[1,,,1] * pred_result_all[1,,,3]
      pred_result_all[1,,,2] <- pred_result_all[1,,,2] * pred_result_all[1,,,3]
      
      dir.create(move_final_folder_name[k])
      
      move_final_image_name <- paste0(move_final_folder_name[k],
                                      "/",
                                      basename(move_final_folder_name[k]),
                                      "___",
                                      dim(pred_result_all)[2],
                                      "___",
                                      dim(pred_result_all)[3],
                                      ".jpg")
      
      file.rename(input$AI_FISH_1p_upload$datapath[k],
                  move_final_image_name)
      future({
        FISH_1p_19q_AI_run_function(image_result = pred_result_all,
                                    basal_layer = 11,
                                    polygon_simplify = 0.00005,
                                    result_type = c("P_1", "P_2", "G"),
                                    temp_AI = move_final_folder_name[k])
      }, seed = NULL, gc = TRUE)
      
      #saveRDS(pred_result_all, "test.rds")
      
      red_size <- pred_result_all[1,,,1] %>% bwlabel() %>% computeFeatures.shape()
      red_dot <- pred_result_all[1,,,1] %>% bwlabel() %>% computeFeatures.moment()
      if (is.null(red_dot) == FALSE) {
        red_dot <- red_dot[,1:2] %>% round()
        if (length(red_dot) == 2) {
          red_dot <- matrix(red_dot, c(1,2)) %>% round()
        }
      }
      green_dot <- pred_result_all[1,,,2] %>% bwlabel() %>% computeFeatures.moment()
      if (is.null(green_dot) == FALSE) {
        green_dot <- green_dot[,1:2] %>% round()
        if (length(green_dot) == 2) {
          green_dot <- matrix(green_dot, c(1,2)) %>% round()
        }
      }
      
      masking_img <- pred_result_all[1,,,3] %>% bwlabel()
      cell_feature <- masking_img %>% computeFeatures.shape()
      if (is.null(cell_feature) == FALSE) {
        cell_feature <- cell_feature[,1] %>% as.data.frame()
        colnames(cell_feature) <- "Nuclear_size"
        cell_feature$red_dot_number <- NA
        cell_feature$green_dot_count <- 0
        cell_feature$red_dot_count <- 0
        
        HER2_temp_pre_table <- masking_img[red_dot[,1:2]]
        HER2_temp_table <- HER2_temp_pre_table %>% table()
        CEP_temp_pre_table <- masking_img[green_dot[,1:2]]
        CEP_temp_table <- CEP_temp_pre_table %>% table()
        
        if (length(HER2_temp_table) > 0) {
          for (k in 1:length(HER2_temp_table)) {
            cell_feature$red_dot_number[names(HER2_temp_table)[k] %>% as.integer()] <- HER2_temp_table[k]
          }  
        }
        
        if (length(HER2_temp_pre_table)) {
          for (k in 1:length(HER2_temp_pre_table)) {
            if (HER2_temp_pre_table[k] != 0) {
              if (k <= nrow(red_size)) {
                add_number <- HER2_temp_pre_table[k]
                cell_feature$red_dot_count[add_number] <- cell_feature$red_dot_count[add_number] + number_check[red_size[k,1]]
              }
            }
          }
        }
        
        if (length(CEP_temp_table)) {
          for (k in 1:length(CEP_temp_table)) {
            cell_feature$green_dot_count[names(CEP_temp_table)[k] %>% as.integer()] <- CEP_temp_table[k]
          }  
        }
        
        
        if (nrow(cell_feature) > 0) {
          display_df <- data.frame(
            red_number = cell_feature$red_dot_number,
            red_count = cell_feature$red_dot_count,
            green_count = cell_feature$green_dot_count,
            cell_area = cell_feature$Nuclear_size,
            use_or_not = cell_feature$red_dot_count > 0 & cell_feature$green_dot_count > 0,
            stringsAsFactors = FALSE
          )
          
          all_display_df <- rbind(all_display_df, display_df)
        }
      }
      #print(k)
    }
    
    updateProgressBar(
      session = session,
      title = paste0("Regression analysis for tumor papulation"),
      id = "AI_FISH_HER2_myprogress",
      value = 85
    )
    
    saveRDS(all_display_df, paste0(file_folder, "/all_display_df_1p.rds"))
    
    Run_AI_model$model_result <- list()
    Run_AI_model$model_result$file_folder <- file_folder
    all_display_df_1p <- all_display_df
    Run_AI_model$model_result$all_display_df_1p <- all_display_df_1p
    Run_AI_model$model_result$available_cell_1p <- all_display_df_1p %>% filter(use_or_not)
    Run_AI_model$model_result$green_red_table_1p <- xyTable(Run_AI_model$model_result$available_cell_1p[,2], 
                                                            Run_AI_model$model_result$available_cell_1p[,3])
    Run_AI_model$model_result$red_green_df_1p <- data.frame(
      number_count = c(rep(0, 900))
    )
    
    for (i in 1:30) {
      for (j in 1:30) {
        rownames(Run_AI_model$model_result$red_green_df_1p)[(i-1)*30 + j] <- paste0("CEP", i, "_HER", j)
      }
    }
    
    for (i in 1:nrow(Run_AI_model$model_result$available_cell_1p)) {
      add_number <- which(rownames(Run_AI_model$model_result$red_green_df_1p) == paste0("CEP", Run_AI_model$model_result$available_cell_1p$green_count[i], "_HER", Run_AI_model$model_result$available_cell_1p$red_count[i]))
      Run_AI_model$model_result$red_green_df_1p$number_count[add_number] <- Run_AI_model$model_result$red_green_df_1p$number_count[add_number] + 1
    }
    
    #Run_AI_model$model_result$red_green_df$number_count <- Run_AI_model$model_result$red_green_df$number_count/sum(Run_AI_model$model_result$red_green_df$number_count)
    
    #saveRDS()
    
    loss_df <- readRDS(paste0("model_for_report/FISH_1p_19q/1p_19q_fraction_list/df_", input$AI_FISH_1p_19q_fraction_list * 100,".rds"))
    
    run_table <- cbind(Run_AI_model$model_result$red_green_df_1p, loss_df)
    penalized_raw <- penalized(number_count ~ .-log2_ratio, positive = TRUE, data = run_table)
    use_name <- which((coef(penalized_raw)[-1]/coef(penalized_raw)[-1] %>% sum()) >= 0.01) %>% names()
    
    run_table2 <- cbind(Run_AI_model$model_result$red_green_df_1p, run_table[,use_name])
    penalized_raw2 <- penalized(number_count ~ ., positive = TRUE, data = run_table2)
    
    pre_draw <- (coef(penalized_raw2)[-1]/sum(coef(penalized_raw2)[-1])*100) %>% sort(decreasing = TRUE)
    pre_draw <- data.frame(
      Red = names(pre_draw) %>% str_split("_HER") %>% sapply(function(x){x[2]}) %>% as.integer(),
      Green = names(pre_draw) %>% str_split("CEP|_HER") %>% sapply(function(x){x[2]}) %>% as.integer(),
      weight = pre_draw,
      stringsAsFactors = FALSE
    )
    
    
    #saveRDS(pre_draw, paste0(file_folder, "/pre_draw.rds"))
    
    re_draw <- data.frame(
      HER2 = c(Run_AI_model$model_result$available_cell_1p$red_count %>% mean(),
               sum(pre_draw$Red * pre_draw$weight)/100),
      CEP = c(Run_AI_model$model_result$available_cell_1p$green_count %>% mean(),
              sum(pre_draw$Green * pre_draw$weight)/100),
      weight = c(1,1),
      Type = c(paste0("All(", Run_AI_model$model_result$available_cell_1p$red_count %>% mean() %>% round(digits = 2), "/", Run_AI_model$model_result$available_cell_1p$green_count %>% mean() %>% round(digits = 2), ")"), 
               paste0("Regression(", (sum(pre_draw$Red * pre_draw$weight)/100) %>% round(digits = 2), "/", (sum(pre_draw$Green * pre_draw$weight)/100) %>% round(digits = 2), ")")),
      stringsAsFactors = FALSE
    )
    
    clone_cluster <- pre_draw
    clone_cluster$clone <- NA
    for (ii in 1:nrow(clone_cluster)) {
      if (ii == 1) {
        clone_cluster$clone[ii] <- 1
      } else {
        distance <- sapply(1:(ii - 1), function(x) {
          abs(clone_cluster$Red[ii] - clone_cluster$Red[x]) + abs(clone_cluster$Green[ii] - clone_cluster$Green[x])
        })
        distance_1 <- which(distance == 1)
        if (length(distance_1) > 0) {
          clone_cluster$clone[ii] <- clone_cluster$clone[min(distance_1, na.rm = TRUE)]
        } else {
          distance_2 <- which(distance == 2)
          if (length(distance_2) > 0) {
            clone_cluster$clone[ii] <- clone_cluster$clone[min(distance_2, na.rm = TRUE)]
          } else {
            clone_cluster$clone[ii] <- max(clone_cluster$clone, na.rm = TRUE) + 1
          }
        }
      }
    }
    
    for (ii in 1:max(clone_cluster$clone)) {
      use_clone <- clone_cluster %>% filter(clone == ii)
      current_raw <- nrow(re_draw)
      re_draw[current_raw + 1, 1] <- (sum(use_clone$Red * use_clone$weight)/sum(use_clone$weight)) %>% round(digits = 2)
      re_draw[current_raw + 1, 2] <- (sum(use_clone$Green * use_clone$weight)/sum(use_clone$weight)) %>% round(digits = 2)
      re_draw[current_raw + 1, 3] <- sum(use_clone$weight)
      re_draw[current_raw + 1, 4] <- "Papulation"
    }
    
    
    updateProgressBar(
      session = session,
      title = "Drawing picture",
      id = "AI_FISH_HER2_myprogress",
      value = 95
    )
    
    pre_draw_1p <- pre_draw
    Run_AI_model$model_result$cluster_data_plot_1p <- pre_draw_1p
    
    re_draw_1p <- re_draw
    Run_AI_model$model_result$group_cluster_final_1p <- re_draw_1p
    #Run_AI_model$model_result$basal_plot10 <- basal_plot10
    
    
    all_display_df <- display_df <- data.frame(
      red_number = 0,
      red_count = 0,
      green_count = 0,
      cell_area = 0,
      use_or_not = FALSE,
      stringsAsFactors = FALSE
    )
    
    all_display_df <- all_display_df[-1,]
    
    file_name <- input$AI_FISH_19q_upload$datapath
    #print(input$AI_FISH_HER2_upload$name)
    
    move_final_folder_name <- paste0("www/Image/AI_FISH_1p_19q/", input$AI_FISH_1p_19q_case_name, "_", input$AI_FISH_1p_upload$name %>% str_replace_all(".jpg|.png", ""), "_19q")
    
    
    for (k in 1:length(file_name)) {
      updateProgressBar(
        session = session,
        title = paste0("Analysis image (19q): ", input$AI_FISH_19q_upload$name[k]),
        id = "AI_FISH_1p_19q_myprogress",
        value = round(20 + 60/length(file_name) * k)
      )
      
      image <- readImage(file_name[k])
      image_1 <- image %>% array_reshape(dim = c(1, dim(image)))
      point_result <- Run_AI_model$model$point_model$predict(image_1)
      seg_result <- Run_AI_model$model$seg_model$predict(image_1)
      pred_result_all <- abind(point_result, seg_result)
      pred_result_all <- (pred_result_all >= 0.5) * 1
      pred_result_all[1,,,3] <- pred_result_all[1,,,3] %>% fillHull()
      
      pred_result_all[1,,,1] <- pred_result_all[1,,,1] * pred_result_all[1,,,3]
      pred_result_all[1,,,2] <- pred_result_all[1,,,2] * pred_result_all[1,,,3]
      
      dir.create(move_final_folder_name[k])
      
      move_final_image_name <- paste0(move_final_folder_name[k],
                                      "/",
                                      basename(move_final_folder_name[k]),
                                      "___",
                                      dim(pred_result_all)[2],
                                      "___",
                                      dim(pred_result_all)[3],
                                      ".jpg")
      
      file.rename(input$AI_FISH_19q_upload$datapath[k],
                  move_final_image_name)
      future({
        FISH_1p_19q_AI_run_function(image_result = pred_result_all,
                                    basal_layer = 11,
                                    polygon_simplify = 0.00005,
                                    result_type = c("P_1", "P_2", "G"),
                                    temp_AI = move_final_folder_name[k])
      }, seed = NULL, gc = TRUE)
      
      #saveRDS(pred_result_all, "test.rds")
      
      red_size <- pred_result_all[1,,,1] %>% bwlabel() %>% computeFeatures.shape()
      red_dot <- pred_result_all[1,,,1] %>% bwlabel() %>% computeFeatures.moment()
      if (is.null(red_dot) == FALSE) {
        red_dot <- red_dot[,1:2] %>% round()
        if (length(red_dot) == 2) {
          red_dot <- matrix(red_dot, c(1,2)) %>% round()
        }
      }
      green_dot <- pred_result_all[1,,,2] %>% bwlabel() %>% computeFeatures.moment()
      if (is.null(green_dot) == FALSE) {
        green_dot <- green_dot[,1:2] %>% round()
        if (length(green_dot) == 2) {
          green_dot <- matrix(green_dot, c(1,2)) %>% round()
        }
      }
      
      masking_img <- pred_result_all[1,,,3] %>% bwlabel()
      cell_feature <- masking_img %>% computeFeatures.shape()
      if (is.null(cell_feature) == FALSE) {
        cell_feature <- cell_feature[,1] %>% as.data.frame()
        colnames(cell_feature) <- "Nuclear_size"
        cell_feature$red_dot_number <- NA
        cell_feature$green_dot_count <- 0
        cell_feature$red_dot_count <- 0
        
        HER2_temp_pre_table <- masking_img[red_dot[,1:2]]
        HER2_temp_table <- HER2_temp_pre_table %>% table()
        CEP_temp_pre_table <- masking_img[green_dot[,1:2]]
        CEP_temp_table <- CEP_temp_pre_table %>% table()
        
        if (length(HER2_temp_table) > 0) {
          for (k in 1:length(HER2_temp_table)) {
            cell_feature$red_dot_number[names(HER2_temp_table)[k] %>% as.integer()] <- HER2_temp_table[k]
          }  
        }
        
        if (length(HER2_temp_pre_table)) {
          for (k in 1:length(HER2_temp_pre_table)) {
            if (HER2_temp_pre_table[k] != 0) {
              if (k <= nrow(red_size)) {
                add_number <- HER2_temp_pre_table[k]
                cell_feature$red_dot_count[add_number] <- cell_feature$red_dot_count[add_number] + number_check[red_size[k,1]]
              }
            }
          }
        }
        
        if (length(CEP_temp_table)) {
          for (k in 1:length(CEP_temp_table)) {
            cell_feature$green_dot_count[names(CEP_temp_table)[k] %>% as.integer()] <- CEP_temp_table[k]
          }  
        }
        
        
        if (nrow(cell_feature) > 0) {
          display_df <- data.frame(
            red_number = cell_feature$red_dot_number,
            red_count = cell_feature$red_dot_count,
            green_count = cell_feature$green_dot_count,
            cell_area = cell_feature$Nuclear_size,
            use_or_not = cell_feature$red_dot_count > 0 & cell_feature$green_dot_count > 0,
            stringsAsFactors = FALSE
          )
          
          all_display_df <- rbind(all_display_df, display_df)
        }
      }
      #print(k)
    }
    
    updateProgressBar(
      session = session,
      title = paste0("Regression analysis for tumor papulation"),
      id = "AI_FISH_HER2_myprogress",
      value = 85
    )
    
    saveRDS(all_display_df, paste0(file_folder, "/all_display_df_19q.rds"))
    
    #Run_AI_model$model_result <- list()
    #Run_AI_model$model_result$file_folder <- file_folder
    
    all_display_df_19q <- all_display_df
    Run_AI_model$model_result$all_display_df_19q <- all_display_df_19q
    Run_AI_model$model_result$available_cell_19q <- all_display_df_19q %>% filter(use_or_not)
    Run_AI_model$model_result$green_red_table_19q <- xyTable(Run_AI_model$model_result$available_cell_19q[,2], 
                                                             Run_AI_model$model_result$available_cell_19q[,3])
    Run_AI_model$model_result$red_green_df_19q <- data.frame(
      number_count = c(rep(0, 900))
    )
    
    for (i in 1:30) {
      for (j in 1:30) {
        rownames(Run_AI_model$model_result$red_green_df_19q)[(i-1)*30 + j] <- paste0("CEP", i, "_HER", j)
      }
    }
    
    for (i in 1:nrow(Run_AI_model$model_result$available_cell_19q)) {
      add_number <- which(rownames(Run_AI_model$model_result$red_green_df_19q) == paste0("CEP", Run_AI_model$model_result$available_cell_19q$green_count[i], "_HER", Run_AI_model$model_result$available_cell_19q$red_count[i]))
      Run_AI_model$model_result$red_green_df_19q$number_count[add_number] <- Run_AI_model$model_result$red_green_df_19q$number_count[add_number] + 1
    }
    
    #Run_AI_model$model_result$red_green_df$number_count <- Run_AI_model$model_result$red_green_df$number_count/sum(Run_AI_model$model_result$red_green_df$number_count)
    
    #saveRDS()
    
    loss_df <- readRDS(paste0("model_for_report/FISH_1p_19q/1p_19q_fraction_list/df_", input$AI_FISH_1p_19q_fraction_list * 100,".rds"))
    
    run_table <- cbind(Run_AI_model$model_result$red_green_df_19q, loss_df)
    penalized_raw <- penalized(number_count ~ .-log2_ratio, positive = TRUE, data = run_table)
    use_name <- which((coef(penalized_raw)[-1]/coef(penalized_raw)[-1] %>% sum()) >= 0.01) %>% names()
    
    run_table2 <- cbind(Run_AI_model$model_result$red_green_df_19q, run_table[,use_name])
    penalized_raw2 <- penalized(number_count ~ ., positive = TRUE, data = run_table2)
    
    pre_draw <- (coef(penalized_raw2)[-1]/sum(coef(penalized_raw2)[-1])*100) %>% sort(decreasing = TRUE)
    pre_draw <- data.frame(
      Red = names(pre_draw) %>% str_split("_HER") %>% sapply(function(x){x[2]}) %>% as.integer(),
      Green = names(pre_draw) %>% str_split("CEP|_HER") %>% sapply(function(x){x[2]}) %>% as.integer(),
      weight = pre_draw,
      stringsAsFactors = FALSE
    )
    
    
    #saveRDS(pre_draw, paste0(file_folder, "/pre_draw.rds"))
    
    re_draw <- data.frame(
      HER2 = c(Run_AI_model$model_result$available_cell_19q$red_count %>% mean(),
               sum(pre_draw$Red * pre_draw$weight)/100),
      CEP = c(Run_AI_model$model_result$available_cell_19q$green_count %>% mean(),
              sum(pre_draw$Green * pre_draw$weight)/100),
      weight = c(1,1),
      Type = c(paste0("All(", Run_AI_model$model_result$available_cell_19q$red_count %>% mean() %>% round(digits = 2), "/", Run_AI_model$model_result$available_cell_19q$green_count %>% mean() %>% round(digits = 2), ")"), 
               paste0("Regression(", (sum(pre_draw$Red * pre_draw$weight)/100) %>% round(digits = 2), "/", (sum(pre_draw$Green * pre_draw$weight)/100) %>% round(digits = 2), ")")),
      stringsAsFactors = FALSE
    )
    
    clone_cluster <- pre_draw
    clone_cluster$clone <- NA
    for (ii in 1:nrow(clone_cluster)) {
      if (ii == 1) {
        clone_cluster$clone[ii] <- 1
      } else {
        distance <- sapply(1:(ii - 1), function(x) {
          abs(clone_cluster$Red[ii] - clone_cluster$Red[x]) + abs(clone_cluster$Green[ii] - clone_cluster$Green[x])
        })
        distance_1 <- which(distance == 1)
        if (length(distance_1) > 0) {
          clone_cluster$clone[ii] <- clone_cluster$clone[min(distance_1, na.rm = TRUE)]
        } else {
          distance_2 <- which(distance == 2)
          if (length(distance_2) > 0) {
            clone_cluster$clone[ii] <- clone_cluster$clone[min(distance_2, na.rm = TRUE)]
          } else {
            clone_cluster$clone[ii] <- max(clone_cluster$clone, na.rm = TRUE) + 1
          }
        }
      }
    }
    
    for (ii in 1:max(clone_cluster$clone)) {
      use_clone <- clone_cluster %>% filter(clone == ii)
      current_raw <- nrow(re_draw)
      re_draw[current_raw + 1, 1] <- (sum(use_clone$Red * use_clone$weight)/sum(use_clone$weight)) %>% round(digits = 2)
      re_draw[current_raw + 1, 2] <- (sum(use_clone$Green * use_clone$weight)/sum(use_clone$weight)) %>% round(digits = 2)
      re_draw[current_raw + 1, 3] <- sum(use_clone$weight)
      re_draw[current_raw + 1, 4] <- "Papulation"
    }
    
    
    updateProgressBar(
      session = session,
      title = "Drawing picture",
      id = "AI_FISH_HER2_myprogress",
      value = 95
    )
    
    pre_draw_19q <- pre_draw
    Run_AI_model$model_result$cluster_data_plot_19q <- pre_draw_19q
    re_draw_19q <- re_draw
    Run_AI_model$model_result$group_cluster_final_19q <- re_draw_19q
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "Analysis complete!!",
      text = "The result is saved!!",
      type = "success"
    )
  }
  
   rm(list=ls(all=TRUE))
   gc(verbose = getOption("verbose"), reset = TRUE, full = TRUE)
})

output$AI_FISH_1p_Ratio_log2_frequency_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    Run_AI_model$model_result$available_cell_1p$slope <- Run_AI_model$model_result$available_cell_1p$red_count/Run_AI_model$model_result$available_cell_1p$green_count
    hist(log2(Run_AI_model$model_result$available_cell_1p$slope), 
         breaks = 1200, 
         xlim = c(-input$AI_FISH_1p_Ratio_log2_frequency_map_width, input$AI_FISH_1p_Ratio_log2_frequency_map_width), 
         las = 1, 
         main = Run_AI_model$model_result$file_folder %>% basename(), 
         xlab = "1p/1q log2 ratio")
  }
})

output$AI_FISH_1p_Size_modified_flow_distribution_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    average_area <- mean(Run_AI_model$model_result$available_cell_1p$cell_area)
    
    ggplot(Run_AI_model$model_result$available_cell_1p, aes(x = red_count/cell_area * average_area, y = green_count/cell_area * average_area)) + 
      geom_point() +
      xlim(0, input$AI_FISH_HER2_Size_modified_flow_distribution_map_width) + 
      ylim(0, input$AI_FISH_HER2_Size_modified_flow_distribution_map_width) +
      labs(y = "1q", x = "1p")
  }
})

output$AI_FISH_1p_Density_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    plot(Run_AI_model$model_result$green_red_table_1p$x , 
         Run_AI_model$model_result$green_red_table_1p$y , 
         cex = Run_AI_model$model_result$green_red_table_1p$number ^ 0.5 , 
         pch=16 , col=rgb(0,0,1,0.5) , 
         xlab= "1p" , 
         ylab="1q" , 
         xlim=c(0, input$AI_FISH_1p_Density_map_width) , 
         ylim=c(0, input$AI_FISH_1p_Density_map_width))
    if (length(Run_AI_model$model_result$green_red_table_1p$x) > 0 & length(Run_AI_model$model_result$green_red_table_1p$y) > 0 & length(Run_AI_model$model_result$green_red_table_1p$number) > 0) {
      text(Run_AI_model$model_result$green_red_table_1p$x , 
           Run_AI_model$model_result$green_red_table_1p$y , 
           Run_AI_model$model_result$green_red_table_1p$number)
    }
  }
})

output$AI_FISH_1p_Regression_density_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    group_plot_data10 <- data.frame(
      HER2 = sapply(1:input$AI_FISH_1p_Regression_density_map_width, function(x){1:input$AI_FISH_1p_Regression_density_map_width}) %>% as.integer(),
      CEP = sapply(1:input$AI_FISH_1p_Regression_density_map_width, function(x){rep(x, input$AI_FISH_1p_Regression_density_map_width)}) %>% as.integer(),
      Group = NA
    )
    
    for (i in 1:nrow(group_plot_data10)) {
      ratio <- group_plot_data10$HER2[i]/group_plot_data10$CEP[i]
      mean_red <- group_plot_data10$HER2[i]
      
      if (ratio <= 0.8) {
        group_plot_data10$Group[i] <- 1
      } else {
        group_plot_data10$Group[i] <- 2
      }
    }
    
    basal_plot10 <- ggplot(group_plot_data10, aes(HER2, CEP)) +
      geom_raster(aes(fill = Group), show.legend = TRUE, alpha = 0.5) +
      scale_fill_gradientn(colours=c("red","white"), guide = "legend") +
      labs(title = "Group distribution") +
      scale_x_continuous(limit = c(0,(input$AI_FISH_1p_Regression_density_map_width + 1)), 
                         breaks = c(0:(input$AI_FISH_1p_Regression_density_map_width + 1)), 
                         labels = c(0:(input$AI_FISH_1p_Regression_density_map_width + 1))) +
      scale_y_continuous(limit = c(0,(input$AI_FISH_1p_Regression_density_map_width + 1)), 
                         breaks = c(0:(input$AI_FISH_1p_Regression_density_map_width + 1)), 
                         labels = c(0:(input$AI_FISH_1p_Regression_density_map_width + 1)))
    
    basal_plot10 <- basal_plot10 +
      geom_point(data = Run_AI_model$model_result$cluster_data_plot_1p, 
                 mapping = aes(x = Red, y = Green), 
                 size = Run_AI_model$model_result$cluster_data_plot_1p$weight, color = 8)
    
    for (i in 1:nrow(Run_AI_model$model_result$cluster_data_plot_1p)) {
      basal_plot10 <- basal_plot10 +
        geom_text(x = Run_AI_model$model_result$cluster_data_plot_1p$Red[i], 
                  y = Run_AI_model$model_result$cluster_data_plot_1p$Green[i], 
                  label = paste0(Run_AI_model$model_result$cluster_data_plot_1p$weight[i] %>% round(digits = 1), "%")) 
    }
    
    basal_plot10 <- basal_plot10 +
      labs(y = "1q", x = "1p")
    basal_plot10
  }
})

output$AI_FISH_1p_Papulation_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    group_plot_data10 <- data.frame(
      HER2 = sapply(1:input$AI_FISH_1p_Papulation_map_width, function(x){1:input$AI_FISH_1p_Papulation_map_width}) %>% as.integer(),
      CEP = sapply(1:input$AI_FISH_1p_Papulation_map_width, function(x){rep(x, input$AI_FISH_1p_Papulation_map_width)}) %>% as.integer(),
      Group = NA
    )
    
    for (i in 1:nrow(group_plot_data10)) {
      ratio <- group_plot_data10$HER2[i]/group_plot_data10$CEP[i]
      mean_red <- group_plot_data10$HER2[i]
      
      if (ratio <= 0.8) {
        group_plot_data10$Group[i] <- 1
      } else {
        group_plot_data10$Group[i] <- 2
      }
    }
    
    basal_plot10 <- ggplot(group_plot_data10, aes(HER2, CEP)) +
      geom_raster(aes(fill = Group), show.legend = TRUE, alpha = 0.5) +
      scale_fill_gradientn(colours=c("red","white"), guide = "legend") +
      labs(title = "Group distribution") +
      scale_x_continuous(limit = c(0,(input$AI_FISH_1p_Papulation_map_width + 1)), 
                         breaks = c(0:(input$AI_FISH_1p_Papulation_map_width + 1)), 
                         labels = c(0:(input$AI_FISH_1p_Papulation_map_width + 1))) +
      scale_y_continuous(limit = c(0,(input$AI_FISH_1p_Papulation_map_width + 1)), 
                         breaks = c(0:(input$AI_FISH_1p_Papulation_map_width + 1)), 
                         labels = c(0:(input$AI_FISH_1p_Papulation_map_width + 1)))
    
    basal_plot10 <- basal_plot10 + 
      geom_point(aes(x = HER2, y = CEP, shape = Type), color = "white", size = 8, data = Run_AI_model$model_result$group_cluster_final_1p) +
      geom_point(aes(x = HER2, y = CEP, shape = Type, colour = Type), size = 5, data = Run_AI_model$model_result$group_cluster_final_1p) +
      theme(legend.key = element_rect(fill = "white", colour = "black"))
    
    
    for (ii in 3:nrow(Run_AI_model$model_result$group_cluster_final_1p)) {
      basal_plot10 <- basal_plot10 +
        geom_text(x = Run_AI_model$model_result$group_cluster_final_1p$HER2[ii], 
                  y = Run_AI_model$model_result$group_cluster_final_1p$CEP[ii] - 0.25 * input$AI_FISH_HER2_Papulation_map_width/10, 
                  label = paste0(Run_AI_model$model_result$group_cluster_final_1p$weight[ii] %>% round(), "%")) +
        geom_text(x = Run_AI_model$model_result$group_cluster_final_1p$HER2[ii], 
                  y = Run_AI_model$model_result$group_cluster_final_1p$CEP[ii] - 0.5 * input$AI_FISH_HER2_Papulation_map_width/10, 
                  label = paste0("(", 
                                 Run_AI_model$model_result$group_cluster_final_1p$HER2[ii] %>% round(digits = 2), 
                                 "/",
                                 Run_AI_model$model_result$group_cluster_final_1p$CEP[ii] %>% round(digits = 2),
                                 ")"))
    }
    
    basal_plot10 <- basal_plot10 +
      labs(y = "1q", x = "1p")
    basal_plot10
  }
})


output$Regression_density_map_copy_1p = renderDT({
  print_table <- Run_AI_model$model_result$cluster_data_plot_1p
  rownames(print_table) <- 1:nrow(print_table)
  print_table
})



output$Papulation_map_copy_1p = renderDT({
  print_table <- Run_AI_model$model_result$group_cluster_final_1p
  colnames(print_table) <- base::c("1p", "1q", "weight", "Type")
  print_table
})


output$AI_FISH_19q_Ratio_log2_frequency_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    Run_AI_model$model_result$available_cell_19q$slope <- Run_AI_model$model_result$available_cell_19q$red_count/Run_AI_model$model_result$available_cell_19q$green_count
    hist(log2(Run_AI_model$model_result$available_cell_19q$slope), 
         breaks = 1200, 
         xlim = c(-input$AI_FISH_19q_Ratio_log2_frequency_map_width, input$AI_FISH_19q_Ratio_log2_frequency_map_width), 
         las = 1, 
         main = Run_AI_model$model_result$file_folder %>% basename(), 
         xlab = "19q/19p log2 ratio")
  }
})

output$AI_FISH_19q_Size_modified_flow_distribution_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    average_area <- mean(Run_AI_model$model_result$available_cell_19q$cell_area)
    
    ggplot(Run_AI_model$model_result$available_cell_19q, aes(x = red_count/cell_area * average_area, y = green_count/cell_area * average_area)) + 
      geom_point() +
      xlim(0, input$AI_FISH_HER2_Size_modified_flow_distribution_map_width) + 
      ylim(0, input$AI_FISH_HER2_Size_modified_flow_distribution_map_width) +
      labs(y = "19p", x = "19q")
  }
})

output$AI_FISH_19q_Density_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    plot(Run_AI_model$model_result$green_red_table_19q$x , 
         Run_AI_model$model_result$green_red_table_19q$y , 
         cex = Run_AI_model$model_result$green_red_table_19q$number ^ 0.5 , 
         pch=16 , col=rgb(0,0,1,0.5) , 
         xlab= "19q" , 
         ylab="19p" , 
         xlim=c(0, input$AI_FISH_19q_Density_map_width) , 
         ylim=c(0, input$AI_FISH_19q_Density_map_width))
    if (length(Run_AI_model$model_result$green_red_table_19q$x) > 0 & length(Run_AI_model$model_result$green_red_table_19q$y) > 0 & length(Run_AI_model$model_result$green_red_table_19q$number) > 0) {
      text(Run_AI_model$model_result$green_red_table_19q$x , 
           Run_AI_model$model_result$green_red_table_19q$y , 
           Run_AI_model$model_result$green_red_table_19q$number)
    }
  }
})

output$AI_FISH_19q_Regression_density_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    group_plot_data10 <- data.frame(
      HER2 = sapply(1:input$AI_FISH_19q_Regression_density_map_width, function(x){1:input$AI_FISH_19q_Regression_density_map_width}) %>% as.integer(),
      CEP = sapply(1:input$AI_FISH_19q_Regression_density_map_width, function(x){rep(x, input$AI_FISH_19q_Regression_density_map_width)}) %>% as.integer(),
      Group = NA
    )
    
    for (i in 1:nrow(group_plot_data10)) {
      ratio <- group_plot_data10$HER2[i]/group_plot_data10$CEP[i]
      mean_red <- group_plot_data10$HER2[i]
      
      if (ratio <= 0.8) {
        group_plot_data10$Group[i] <- 1
      } else {
        group_plot_data10$Group[i] <- 2
      }
    }
    
    basal_plot10 <- ggplot(group_plot_data10, aes(HER2, CEP)) +
      geom_raster(aes(fill = Group), show.legend = TRUE, alpha = 0.5) +
      scale_fill_gradientn(colours=c("red","white"), guide = "legend") +
      labs(title = "Group distribution") +
      scale_x_continuous(limit = c(0,(input$AI_FISH_19q_Regression_density_map_width + 1)), 
                         breaks = c(0:(input$AI_FISH_19q_Regression_density_map_width + 1)), 
                         labels = c(0:(input$AI_FISH_19q_Regression_density_map_width + 1))) +
      scale_y_continuous(limit = c(0,(input$AI_FISH_19q_Regression_density_map_width + 1)), 
                         breaks = c(0:(input$AI_FISH_19q_Regression_density_map_width + 1)), 
                         labels = c(0:(input$AI_FISH_19q_Regression_density_map_width + 1)))
    
    basal_plot10 <- basal_plot10 +
      geom_point(data = Run_AI_model$model_result$cluster_data_plot_19q, 
                 mapping = aes(x = Red, y = Green), 
                 size = Run_AI_model$model_result$cluster_data_plot_19q$weight, color = 8)
    
    for (i in 1:nrow(Run_AI_model$model_result$cluster_data_plot_19q)) {
      basal_plot10 <- basal_plot10 +
        geom_text(x = Run_AI_model$model_result$cluster_data_plot_19q$Red[i], 
                  y = Run_AI_model$model_result$cluster_data_plot_19q$Green[i], 
                  label = paste0(Run_AI_model$model_result$cluster_data_plot_19q$weight[i] %>% round(digits = 1), "%")) 
    }
    
    basal_plot10 <- basal_plot10 +
      labs(y = "19p", x = "19q")
    basal_plot10
  }
})

output$AI_FISH_19q_Papulation_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    group_plot_data10 <- data.frame(
      HER2 = sapply(1:input$AI_FISH_19q_Papulation_map_width, function(x){1:input$AI_FISH_19q_Papulation_map_width}) %>% as.integer(),
      CEP = sapply(1:input$AI_FISH_19q_Papulation_map_width, function(x){rep(x, input$AI_FISH_19q_Papulation_map_width)}) %>% as.integer(),
      Group = NA
    )
    
    for (i in 1:nrow(group_plot_data10)) {
      ratio <- group_plot_data10$HER2[i]/group_plot_data10$CEP[i]
      mean_red <- group_plot_data10$HER2[i]
      
      if (ratio <= 0.8) {
        group_plot_data10$Group[i] <- 1
      } else {
        group_plot_data10$Group[i] <- 2
      }
    }
    
    basal_plot10 <- ggplot(group_plot_data10, aes(HER2, CEP)) +
      geom_raster(aes(fill = Group), show.legend = TRUE, alpha = 0.5) +
      scale_fill_gradientn(colours=c("red","white"), guide = "legend") +
      labs(title = "Group distribution") +
      scale_x_continuous(limit = c(0,(input$AI_FISH_19q_Papulation_map_width + 1)), 
                         breaks = c(0:(input$AI_FISH_19q_Papulation_map_width + 1)), 
                         labels = c(0:(input$AI_FISH_19q_Papulation_map_width + 1))) +
      scale_y_continuous(limit = c(0,(input$AI_FISH_19q_Papulation_map_width + 1)), 
                         breaks = c(0:(input$AI_FISH_19q_Papulation_map_width + 1)), 
                         labels = c(0:(input$AI_FISH_19q_Papulation_map_width + 1)))
    
    basal_plot10 <- basal_plot10 + 
      geom_point(aes(x = HER2, y = CEP, shape = Type), color = "white", size = 8, data = Run_AI_model$model_result$group_cluster_final_19q) +
      geom_point(aes(x = HER2, y = CEP, shape = Type, colour = Type), size = 5, data = Run_AI_model$model_result$group_cluster_final_19q) +
      theme(legend.key = element_rect(fill = "white", colour = "black"))
    
    
    for (ii in 3:nrow(Run_AI_model$model_result$group_cluster_final_19q)) {
      basal_plot10 <- basal_plot10 +
        geom_text(x = Run_AI_model$model_result$group_cluster_final_19q$HER2[ii], 
                  y = Run_AI_model$model_result$group_cluster_final_19q$CEP[ii] - 0.25 * input$AI_FISH_HER2_Papulation_map_width/10, 
                  label = paste0(Run_AI_model$model_result$group_cluster_final_19q$weight[ii] %>% round(), "%")) +
        geom_text(x = Run_AI_model$model_result$group_cluster_final_19q$HER2[ii], 
                  y = Run_AI_model$model_result$group_cluster_final_19q$CEP[ii] - 0.5 * input$AI_FISH_HER2_Papulation_map_width/10, 
                  label = paste0("(", 
                                 Run_AI_model$model_result$group_cluster_final_19q$HER2[ii] %>% round(digits = 2), 
                                 "/",
                                 Run_AI_model$model_result$group_cluster_final_19q$CEP[ii] %>% round(digits = 2),
                                 ")"))
    }
    
    basal_plot10 <- basal_plot10 +
      labs(y = "19p", x = "19q")
    basal_plot10
  }
})


observeEvent(input$AI_FISH_1p_19q_Load_FISH_image_result, {
  progressSweetAlert(
    session = session, id = "AI_FISH_1p_19q_myprogress",
    title = "Start AI analysis",
    display_pct = TRUE, value = 0
  )
  if (Run_AI_model$model_tag != "AI_FISH_1p_19q") {
    updateProgressBar(
      session = session,
      title = "Loading model...",
      id = "AI_FISH_1p_19q_myprogress",
      value = 10
    )
    Run_AI_model$model_option <- ""
    Run_AI_model$model_result <- list()
    source(file = "model_for_report/FISH_1p_19q/1p_19q_model/model_function.R", local = TRUE)
    Run_AI_model$model_tag <- "AI_FISH_1p_19q"
    Run_AI_model$model <- list()
    
    Run_AI_model$model$seg_model <- load_model_weights_hdf5(get_unet(num_classes = 1), 
                                                            filepath = "model_for_report/FISH_1p_19q/1p_19q_model/cell_border.h5")
    
    Run_AI_model$model$point_model <- load_model_weights_hdf5(get_unet(num_classes = 2), 
                                                              filepath = "model_for_report/FISH_1p_19q/1p_19q_model/point_model.h5")
  }
  
  updateProgressBar(
    session = session,
    title = paste0("Regression analysis for tumor papulation"),
    id = "AI_FISH_HER2_myprogress",
    value = 50
  )
  
  all_display_df <- readRDS(paste0("model_for_report/FISH_1p_19q/Report_history/", 
                                   input$AI_FISH_1p_19q_select_finished_result, 
                                   "/all_display_df_1p.rds"))
  
  Run_AI_model$model_result <- list()
  Run_AI_model$model_result$file_folder <- paste0("model_for_report/FISH_1p_19q/Report_history/", input$AI_FISH_1p_19q_select_finished_result)
  all_display_df_1p <- all_display_df
  Run_AI_model$model_result$all_display_df_1p <- all_display_df_1p
  Run_AI_model$model_result$available_cell_1p <- all_display_df_1p %>% filter(use_or_not)
  Run_AI_model$model_result$green_red_table_1p <- xyTable(Run_AI_model$model_result$available_cell_1p[,2], 
                                                          Run_AI_model$model_result$available_cell_1p[,3])
  Run_AI_model$model_result$red_green_df_1p <- data.frame(
    number_count = c(rep(0, 900))
  )
  
  for (i in 1:30) {
    for (j in 1:30) {
      rownames(Run_AI_model$model_result$red_green_df_1p)[(i-1)*30 + j] <- paste0("CEP", i, "_HER", j)
    }
  }
  
  for (i in 1:nrow(Run_AI_model$model_result$available_cell_1p)) {
    add_number <- which(rownames(Run_AI_model$model_result$red_green_df_1p) == paste0("CEP", Run_AI_model$model_result$available_cell_1p$green_count[i], "_HER", Run_AI_model$model_result$available_cell_1p$red_count[i]))
    Run_AI_model$model_result$red_green_df_1p$number_count[add_number] <- Run_AI_model$model_result$red_green_df_1p$number_count[add_number] + 1
  }
  
  #Run_AI_model$model_result$red_green_df$number_count <- Run_AI_model$model_result$red_green_df$number_count/sum(Run_AI_model$model_result$red_green_df$number_count)
  
  #saveRDS()
  
  loss_df <- readRDS(paste0("model_for_report/FISH_1p_19q/1p_19q_fraction_list/df_", input$AI_FISH_1p_19q_fraction_list * 100,".rds"))
  
  run_table <- cbind(Run_AI_model$model_result$red_green_df_1p, loss_df)
  penalized_raw <- penalized(number_count ~ .-log2_ratio, positive = TRUE, data = run_table)
  use_name <- which((coef(penalized_raw)[-1]/coef(penalized_raw)[-1] %>% sum()) >= 0.01) %>% names()
  
  run_table2 <- cbind(Run_AI_model$model_result$red_green_df_1p, run_table[,use_name])
  penalized_raw2 <- penalized(number_count ~ ., positive = TRUE, data = run_table2)
  
  pre_draw <- (coef(penalized_raw2)[-1]/sum(coef(penalized_raw2)[-1])*100) %>% sort(decreasing = TRUE)
  pre_draw <- data.frame(
    Red = names(pre_draw) %>% str_split("_HER") %>% sapply(function(x){x[2]}) %>% as.integer(),
    Green = names(pre_draw) %>% str_split("CEP|_HER") %>% sapply(function(x){x[2]}) %>% as.integer(),
    weight = pre_draw,
    stringsAsFactors = FALSE
  )
  
  
  #saveRDS(pre_draw, paste0(file_folder, "/pre_draw.rds"))
  
  re_draw <- data.frame(
    HER2 = c(Run_AI_model$model_result$available_cell_1p$red_count %>% mean(),
             sum(pre_draw$Red * pre_draw$weight)/100),
    CEP = c(Run_AI_model$model_result$available_cell_1p$green_count %>% mean(),
            sum(pre_draw$Green * pre_draw$weight)/100),
    weight = c(1,1),
    Type = c(paste0("All(", Run_AI_model$model_result$available_cell_1p$red_count %>% mean() %>% round(digits = 2), "/", Run_AI_model$model_result$available_cell_1p$green_count %>% mean() %>% round(digits = 2), ")"), 
             paste0("Regression(", (sum(pre_draw$Red * pre_draw$weight)/100) %>% round(digits = 2), "/", (sum(pre_draw$Green * pre_draw$weight)/100) %>% round(digits = 2), ")")),
    stringsAsFactors = FALSE
  )
  
  clone_cluster <- pre_draw
  clone_cluster$clone <- NA
  for (ii in 1:nrow(clone_cluster)) {
    if (ii == 1) {
      clone_cluster$clone[ii] <- 1
    } else {
      distance <- sapply(1:(ii - 1), function(x) {
        abs(clone_cluster$Red[ii] - clone_cluster$Red[x]) + abs(clone_cluster$Green[ii] - clone_cluster$Green[x])
      })
      distance_1 <- which(distance == 1)
      if (length(distance_1) > 0) {
        clone_cluster$clone[ii] <- clone_cluster$clone[min(distance_1, na.rm = TRUE)]
      } else {
        distance_2 <- which(distance == 2)
        if (length(distance_2) > 0) {
          clone_cluster$clone[ii] <- clone_cluster$clone[min(distance_2, na.rm = TRUE)]
        } else {
          clone_cluster$clone[ii] <- max(clone_cluster$clone, na.rm = TRUE) + 1
        }
      }
    }
  }
  
  for (ii in 1:max(clone_cluster$clone)) {
    use_clone <- clone_cluster %>% filter(clone == ii)
    current_raw <- nrow(re_draw)
    re_draw[current_raw + 1, 1] <- (sum(use_clone$Red * use_clone$weight)/sum(use_clone$weight)) %>% round(digits = 2)
    re_draw[current_raw + 1, 2] <- (sum(use_clone$Green * use_clone$weight)/sum(use_clone$weight)) %>% round(digits = 2)
    re_draw[current_raw + 1, 3] <- sum(use_clone$weight)
    re_draw[current_raw + 1, 4] <- "Papulation"
  }
  
  
  updateProgressBar(
    session = session,
    title = "Drawing picture",
    id = "AI_FISH_HER2_myprogress",
    value = 95
  )
  
  pre_draw_1p <- pre_draw
  Run_AI_model$model_result$cluster_data_plot_1p <- pre_draw_1p
  
  re_draw_1p <- re_draw
  Run_AI_model$model_result$group_cluster_final_1p <- re_draw_1p
  
  
  all_display_df <- readRDS(paste0("model_for_report/FISH_1p_19q/Report_history/", 
                                   input$AI_FISH_1p_19q_select_finished_result, 
                                   "/all_display_df_19q.rds"))
  
  all_display_df_19q <- all_display_df
  Run_AI_model$model_result$all_display_df_19q <- all_display_df_19q
  Run_AI_model$model_result$available_cell_19q <- all_display_df_19q %>% filter(use_or_not)
  Run_AI_model$model_result$green_red_table_19q <- xyTable(Run_AI_model$model_result$available_cell_19q[,2], 
                                                           Run_AI_model$model_result$available_cell_19q[,3])
  Run_AI_model$model_result$red_green_df_19q <- data.frame(
    number_count = c(rep(0, 900))
  )
  
  for (i in 1:30) {
    for (j in 1:30) {
      rownames(Run_AI_model$model_result$red_green_df_19q)[(i-1)*30 + j] <- paste0("CEP", i, "_HER", j)
    }
  }
  
  for (i in 1:nrow(Run_AI_model$model_result$available_cell_19q)) {
    add_number <- which(rownames(Run_AI_model$model_result$red_green_df_19q) == paste0("CEP", Run_AI_model$model_result$available_cell_19q$green_count[i], "_HER", Run_AI_model$model_result$available_cell_19q$red_count[i]))
    Run_AI_model$model_result$red_green_df_19q$number_count[add_number] <- Run_AI_model$model_result$red_green_df_19q$number_count[add_number] + 1
  }
  
  #Run_AI_model$model_result$red_green_df$number_count <- Run_AI_model$model_result$red_green_df$number_count/sum(Run_AI_model$model_result$red_green_df$number_count)
  
  #saveRDS()
  
  loss_df <- readRDS(paste0("model_for_report/FISH_1p_19q/1p_19q_fraction_list/df_", input$AI_FISH_1p_19q_fraction_list * 100,".rds"))
  
  run_table <- cbind(Run_AI_model$model_result$red_green_df_19q, loss_df)
  penalized_raw <- penalized(number_count ~ .-log2_ratio, positive = TRUE, data = run_table)
  use_name <- which((coef(penalized_raw)[-1]/coef(penalized_raw)[-1] %>% sum()) >= 0.01) %>% names()
  
  run_table2 <- cbind(Run_AI_model$model_result$red_green_df_19q, run_table[,use_name])
  penalized_raw2 <- penalized(number_count ~ ., positive = TRUE, data = run_table2)
  
  pre_draw <- (coef(penalized_raw2)[-1]/sum(coef(penalized_raw2)[-1])*100) %>% sort(decreasing = TRUE)
  pre_draw <- data.frame(
    Red = names(pre_draw) %>% str_split("_HER") %>% sapply(function(x){x[2]}) %>% as.integer(),
    Green = names(pre_draw) %>% str_split("CEP|_HER") %>% sapply(function(x){x[2]}) %>% as.integer(),
    weight = pre_draw,
    stringsAsFactors = FALSE
  )
  
  
  #saveRDS(pre_draw, paste0(file_folder, "/pre_draw.rds"))
  
  re_draw <- data.frame(
    HER2 = c(Run_AI_model$model_result$available_cell_19q$red_count %>% mean(),
             sum(pre_draw$Red * pre_draw$weight)/100),
    CEP = c(Run_AI_model$model_result$available_cell_19q$green_count %>% mean(),
            sum(pre_draw$Green * pre_draw$weight)/100),
    weight = c(1,1),
    Type = c(paste0("All(", Run_AI_model$model_result$available_cell_19q$red_count %>% mean() %>% round(digits = 2), "/", Run_AI_model$model_result$available_cell_19q$green_count %>% mean() %>% round(digits = 2), ")"), 
             paste0("Regression(", (sum(pre_draw$Red * pre_draw$weight)/100) %>% round(digits = 2), "/", (sum(pre_draw$Green * pre_draw$weight)/100) %>% round(digits = 2), ")")),
    stringsAsFactors = FALSE
  )
  
  clone_cluster <- pre_draw
  clone_cluster$clone <- NA
  for (ii in 1:nrow(clone_cluster)) {
    if (ii == 1) {
      clone_cluster$clone[ii] <- 1
    } else {
      distance <- sapply(1:(ii - 1), function(x) {
        abs(clone_cluster$Red[ii] - clone_cluster$Red[x]) + abs(clone_cluster$Green[ii] - clone_cluster$Green[x])
      })
      distance_1 <- which(distance == 1)
      if (length(distance_1) > 0) {
        clone_cluster$clone[ii] <- clone_cluster$clone[min(distance_1, na.rm = TRUE)]
      } else {
        distance_2 <- which(distance == 2)
        if (length(distance_2) > 0) {
          clone_cluster$clone[ii] <- clone_cluster$clone[min(distance_2, na.rm = TRUE)]
        } else {
          clone_cluster$clone[ii] <- max(clone_cluster$clone, na.rm = TRUE) + 1
        }
      }
    }
  }
  
  for (ii in 1:max(clone_cluster$clone)) {
    use_clone <- clone_cluster %>% filter(clone == ii)
    current_raw <- nrow(re_draw)
    re_draw[current_raw + 1, 1] <- (sum(use_clone$Red * use_clone$weight)/sum(use_clone$weight)) %>% round(digits = 2)
    re_draw[current_raw + 1, 2] <- (sum(use_clone$Green * use_clone$weight)/sum(use_clone$weight)) %>% round(digits = 2)
    re_draw[current_raw + 1, 3] <- sum(use_clone$weight)
    re_draw[current_raw + 1, 4] <- "Papulation"
  }
  
  
  updateProgressBar(
    session = session,
    title = "Drawing picture",
    id = "AI_FISH_HER2_myprogress",
    value = 95
  )
  
  pre_draw_19q <- pre_draw
  Run_AI_model$model_result$cluster_data_plot_19q <- pre_draw_19q
  re_draw_19q <- re_draw
  Run_AI_model$model_result$group_cluster_final_19q <- re_draw_19q
  
  closeSweetAlert(session = session)
  sendSweetAlert(
    session = session,
    title = "Analysis complete!!",
    text = "The result is saved!!",
    type = "success"
  )
})

output$Regression_density_map_copy_19q = renderDT({
  print_table <- Run_AI_model$model_result$cluster_data_plot_19q
  rownames(print_table) <- 1:nrow(print_table)
  print_table
})



output$Papulation_map_copy_19q = renderDT({
  print_table <- Run_AI_model$model_result$group_cluster_final_19q
  colnames(print_table) <- base::c("19q", "19p", "weight", "Type")
  print_table
})

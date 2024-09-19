output$AI_FISH_HER2_files <- renderTable(input$AI_FISH_HER2_upload[,1:3])

observeEvent(input$AI_FISH_HER2_select_finished_result, {
  all_HER2_FISH_file <- list.files(paste0("www/Image/AI_FISH_HER2/")) %>% 
    grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% 
    str_sort(numeric = TRUE)
  all_HER2_FISH_file <- all_HER2_FISH_file[str_detect(all_HER2_FISH_file, input$AI_FISH_HER2_select_finished_result)]
  updateSelectInput(session, inputId = "Image_input", choices = all_HER2_FISH_file)
})


observeEvent(input$AI_FISH_HER2_start_analysis, {
  if (input$AI_FISH_HER2_case_name == "") {
    sendSweetAlert(
      session = session,
      title = "No case name or number is found!",
      text = "Please Enter Case name or number",
      type = "error"
    )
  } else {
    progressSweetAlert(
      session = session, id = "AI_FISH_HER2_myprogress",
      title = "Start AI analysis",
      display_pct = TRUE, value = 0
    )
    if (Run_AI_model$model_tag != "AI_FISH_HER2") {
      updateProgressBar(
        session = session,
        title = "Loading model...",
        id = "AI_FISH_HER2_myprogress",
        value = 10
      )
      Run_AI_model$model_option <- ""
      Run_AI_model$model_result <- list()
      source(file = "model_for_report/FISH_HER2/HER2_model/model_function.R", local = TRUE)
      Run_AI_model$model_tag <- "AI_FISH_HER2"
      Run_AI_model$model <- list()
      
      Run_AI_model$model$seg_model <- load_model_weights_hdf5(get_unet(num_classes = 1), 
                                                              filepath = "model_for_report/FISH_HER2/HER2_model/cell_border.h5")
      
      Run_AI_model$model$point_model <- load_model_weights_hdf5(get_unet(num_classes = 2), 
                                                                filepath = "model_for_report/FISH_HER2/HER2_model/point_model.h5")
    }
    
    source(file = "program_script/Specific_model_run_script/FISH_HER2_Function_setting.R", local = TRUE)
    
    number_check <- c(rep(1, 17), 
                      rep(2, 10),
                      rep(3, 10),
                      rep(4, 10),
                      rep(5, 11),
                      rep(6, 11),
                      rep(7, 10),
                      rep(8, 10),
                      rep(9, 10),
                      rep(10, 10),
                      rep(11, 10),
                      rep(12, 10),
                      rep(13, 10))
    
    updateProgressBar(
      session = session,
      title = "Environment initiation...",
      id = "AI_FISH_HER2_myprogress",
      value = 20
    )
    file_folder <- paste0("model_for_report/FISH_HER2/Report_history/", input$AI_FISH_HER2_case_name)
    
    dir.create(file_folder)
    
    all_display_df <- display_df <- data.frame(
      red_number = 0,
      red_count = 0,
      green_count = 0,
      cell_area = 0,
      use_or_not = FALSE,
      stringsAsFactors = FALSE
    )
    
    all_display_df <- all_display_df[-1,]
    
    file_name <- input$AI_FISH_HER2_upload$datapath
    #print(input$AI_FISH_HER2_upload$name)
    
    if (file.exists("www/Image/AI_FISH_HER2") == FALSE) {
      dir.create("www/Image/AI_FISH_HER2")
    }
    
    move_final_folder_name <- paste0("www/Image/AI_FISH_HER2/", input$AI_FISH_HER2_case_name, "_", input$AI_FISH_HER2_upload$name %>% str_replace_all(".jpg|.png", ""))
    #move_final_image_name <- paste0("www/Image/AI_FISH/", input$AI_FISH_HER2_case_name, "_", input$AI_FISH_HER2_upload$name %>% str_replace_all(".jpg|.png", ""), "/", input$AI_FISH_HER2_upload$name)
    
    #print(move_final_folder_name)
    #print(move_final_image_name)
    
    for (k in 1:length(file_name)) {
      updateProgressBar(
        session = session,
        title = paste0("Analysis image: ", input$AI_FISH_HER2_upload$name[k]),
        id = "AI_FISH_HER2_myprogress",
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
      
      file.rename(input$AI_FISH_HER2_upload$datapath[k],
                  move_final_image_name)
      future({
        HER2_AI_run_function(image_result = pred_result_all,
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
      title = paste0("Regression analysis for tumor population"),
      id = "AI_FISH_HER2_myprogress",
      value = 85
    )
    
    saveRDS(all_display_df, paste0(file_folder, "/all_display_df.rds"))
    
    Run_AI_model$model_result <- list()
    Run_AI_model$model_result$file_folder <- file_folder
    Run_AI_model$model_result$all_display_df <- all_display_df
    Run_AI_model$model_result$available_cell <- all_display_df %>% filter(use_or_not)
    Run_AI_model$model_result$green_red_table <- xyTable(Run_AI_model$model_result$available_cell[,2], 
                                                         Run_AI_model$model_result$available_cell[,3])
    Run_AI_model$model_result$red_green_df <- data.frame(
      number_count = c(rep(0, 900))
    )
    
    for (i in 1:30) {
      for (j in 1:30) {
        rownames(Run_AI_model$model_result$red_green_df)[(i-1)*30 + j] <- paste0("CEP", i, "_HER", j)
      }
    }
    
    for (i in 1:nrow(Run_AI_model$model_result$available_cell)) {
      add_number <- which(rownames(Run_AI_model$model_result$red_green_df) == paste0("CEP", Run_AI_model$model_result$available_cell$green_count[i], "_HER", Run_AI_model$model_result$available_cell$red_count[i]))
      Run_AI_model$model_result$red_green_df$number_count[add_number] <- Run_AI_model$model_result$red_green_df$number_count[add_number] + 1
    }
    
    #Run_AI_model$model_result$red_green_df$number_count <- Run_AI_model$model_result$red_green_df$number_count/sum(Run_AI_model$model_result$red_green_df$number_count)
    
    #saveRDS()
    
    loss_df <- readRDS(paste0("model_for_report/FISH_HER2/HER2_fraction_list/df_", input$AI_FISH_HER2_fraction_list * 100,".rds"))
    
    run_table <- cbind(Run_AI_model$model_result$red_green_df, loss_df)
    penalized_raw <- penalized(number_count ~ .-log2_ratio, positive = TRUE, data = run_table)
    use_name <- which((coef(penalized_raw)[-1]/coef(penalized_raw)[-1] %>% sum()) >= 0.01) %>% names()
    
    run_table2 <- cbind(Run_AI_model$model_result$red_green_df, run_table[,use_name])
    penalized_raw2 <- penalized(number_count ~ ., positive = TRUE, data = run_table2)
    
    pre_draw <- (coef(penalized_raw2)[-1]/sum(coef(penalized_raw2)[-1])*100) %>% sort(decreasing = TRUE)
    pre_draw <- data.frame(
      HER2 = names(pre_draw) %>% str_split("_HER") %>% sapply(function(x){x[2]}) %>% as.integer(),
      CEP = names(pre_draw) %>% str_split("CEP|_HER") %>% sapply(function(x){x[2]}) %>% as.integer(),
      weight = pre_draw,
      group = NA,
      P_N = NA,
      stringsAsFactors = FALSE
    )
    
    
    for (i in 1:nrow(pre_draw)) {
      ratio <- pre_draw$HER2[i]/pre_draw$CEP[i]
      mean_red <- pre_draw$HER2[i]
      
      if (ratio >=2 & mean_red >= 4) {
        pre_draw$group[i] <- 1
        pre_draw$P_N[i] <- "P"
      } else if (ratio >= 2 & mean_red < 4) {
        pre_draw$group[i] <- 2
        pre_draw$P_N[i] <- "N"
      } else if (ratio < 2 & mean_red >= 6) {
        pre_draw$group[i] <- 3
        pre_draw$P_N[i] <- "P"
      } else if (ratio < 2 & mean_red >= 4 & mean_red < 6) {
        pre_draw$group[i] <- 4
        pre_draw$P_N[i] <- "N"
      } else if (ratio < 2 & mean_red < 4) {
        pre_draw$group[i] <- 5
        pre_draw$P_N[i] <- "N"
      }
    }
    
    run_table3 <- run_table2[,rownames(pre_draw)]
    for (i in 1:nrow(pre_draw)) {
      run_table3[,rownames(pre_draw)[i]] <- run_table3[,rownames(pre_draw)[i]] * pre_draw$weight[i]
    }
    for (i in 1:nrow(run_table3)) {
      run_table3[i,] <- run_table3[i,]/sum(run_table3[i,]) * 100
    }
    run_table3 <- run_table3 %>% filter(is.na(run_table3[,1]) == FALSE)
    
    Run_AI_model$model_result$likelihood_ratio <- run_table3
    
    saveRDS(run_table3, paste0(file_folder, "/", input$AI_FISH_HER2_fraction_list * 100, "_run_table3.rds"))
    
    updateSelectInput(session, 
                      inputId = "HER2_Regression_likelihood_ratio", 
                      choices = rownames(run_table3))
    #saveRDS(pre_draw, paste0(file_folder, "/pre_draw.rds"))
    
    re_draw <- data.frame(
      HER2 = c(Run_AI_model$model_result$available_cell$red_count %>% mean(),
               sum(pre_draw$HER2 * pre_draw$weight)/100),
      CEP = c(Run_AI_model$model_result$available_cell$green_count %>% mean(),
              sum(pre_draw$CEP * pre_draw$weight)/100),
      weight = c(1,1),
      Type = c(paste0("All(", Run_AI_model$model_result$available_cell$red_count %>% mean() %>% round(digits = 2), "/", Run_AI_model$model_result$available_cell$green_count %>% mean() %>% round(digits = 2), ")"), 
               paste0("Regression(", (sum(pre_draw$HER2 * pre_draw$weight)/100) %>% round(digits = 2), "/", (sum(pre_draw$CEP * pre_draw$weight)/100) %>% round(digits = 2), ")")),
      stringsAsFactors = FALSE
    )
    
    clone_cluster <- pre_draw
    clone_cluster$clone <- NA
    for (ii in 1:nrow(clone_cluster)) {
      if (ii == 1) {
        clone_cluster$clone[ii] <- 1
      } else {
        distance <- sapply(1:(ii - 1), function(x) {
          hor_d <- abs(clone_cluster$HER2[ii] - clone_cluster$HER2[x])
          ver_d <- abs(clone_cluster$CEP[ii] - clone_cluster$CEP[x])
          if (hor_d == 1 & ver_d == 1) {
            1
          } else {
            abs(clone_cluster$HER2[ii] - clone_cluster$HER2[x]) + abs(clone_cluster$CEP[ii] - clone_cluster$CEP[x])
          }
        })
        distance_1 <- which(distance == 1)
        if (length(distance_1) > 0) {
          clone_cluster$clone[ii] <- clone_cluster$clone[min(distance_1, na.rm = TRUE)]
        } else {
          clone_cluster$clone[ii] <- max(clone_cluster$clone, na.rm = TRUE) + 1
        }
      }
    }
    
    for (ii in 1:max(clone_cluster$clone)) {
      use_clone <- clone_cluster %>% filter(clone == ii)
      current_raw <- nrow(re_draw)
      re_draw[current_raw + 1, 1] <- (sum(use_clone$HER2 * use_clone$weight)/sum(use_clone$weight)) %>% round(digits = 2)
      re_draw[current_raw + 1, 2] <- (sum(use_clone$CEP * use_clone$weight)/sum(use_clone$weight)) %>% round(digits = 2)
      re_draw[current_raw + 1, 3] <- sum(use_clone$weight)
      re_draw[current_raw + 1, 4] <- "Population"
    }
    
    
    updateProgressBar(
      session = session,
      title = "Drawing picture",
      id = "AI_FISH_HER2_myprogress",
      value = 95
    )
    
    Run_AI_model$model_result$cluster_data_plot <- pre_draw
    Run_AI_model$model_result$group_cluster_final <- re_draw
    #Run_AI_model$model_result$basal_plot10 <- basal_plot10
    saveRDS(pre_draw, paste0(file_folder, "/", input$AI_FISH_HER2_fraction_list * 100, "_pre_draw.rds"))
    saveRDS(re_draw, paste0(file_folder, "/", input$AI_FISH_HER2_fraction_list * 100, "_re_draw.rds"))
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "Analysis complete!!",
      text = "The result is saved!!",
      type = "success"
    )
  }
})

output$AI_FISH_HER2_Ratio_log2_frequency_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    available_cell <- Run_AI_model$model_result$available_cell
    available_cell$slope <- log2(available_cell$red_count/available_cell$green_count)
    available_cell$Group <- NA
    for (i in 1:nrow(available_cell)) {
      ratio <- available_cell$red_count[i]/available_cell$green_count[i]
      mean_red <- available_cell$red_count[i]
      
      if (ratio >=2 & mean_red >= 4) {
        available_cell$Group[i] <- 1
      } else if (ratio >= 2 & mean_red < 4) {
        available_cell$Group[i] <- 2
      } else if (ratio < 2 & mean_red >= 6) {
        available_cell$Group[i] <- 3
      } else if (ratio < 2 & mean_red >= 4 & mean_red < 6) {
        available_cell$Group[i] <- 4
      } else if (ratio < 2 & mean_red < 4) {
        available_cell$Group[i] <- 5
      }
    }
    
    pyramid_df <- data.frame(
      Slope = 0,
      Group = 0,
      Number = 0,
      stringsAsFactors = FALSE
    )
    
    for (i in 1:nrow(available_cell)) {
      if (i == 1) {
        pyramid_df$Slope[1] <- available_cell$slope[i]
        pyramid_df$Group[1] <- available_cell$Group[i]
        pyramid_df$Number[1] <- 1
      } else {
        if ((pyramid_df$Slope == available_cell$slope[i]) %>% sum() > 0) {
          group_type <- available_cell$Group[i]
          same_group_number <- which(pyramid_df$Group == group_type)
          same_slope_number <- which(pyramid_df$Slope == available_cell$slope[i])
          intersect_number <- same_group_number[same_group_number %in% same_slope_number]
          if (length(intersect_number) > 0) {
            row_length <- intersect_number
            pyramid_df$Number[row_length] <- pyramid_df$Number[row_length] + 1
          } else {
            row_length <- nrow(pyramid_df) + 1
            pyramid_df[row_length,1] <- available_cell$slope[i]
            pyramid_df[row_length,2] <- available_cell$Group[i]
            pyramid_df[row_length,3] <- 1
          }
        } else {
          row_length <- nrow(pyramid_df) + 1
          pyramid_df[row_length,1] <- available_cell$slope[i]
          pyramid_df[row_length,2] <- available_cell$Group[i]
          pyramid_df[row_length,3] <- 1
        }
      }
    }
    
    
    pyramid_df$Slope_abs <- pyramid_df$Slope %>% abs()
    pyramid_df$Group <- pyramid_df$Group %>% factor(levels = 1:5)
    
    pyramid_df$Slope %>% unique()
    
    max_scale <- sapply(pyramid_df$Slope %>% unique(), function(x){
      pyramid_df$Number[which(pyramid_df$Slope == x)] %>% sum()
    }) %>% max()
    
    scale_size <- max_scale %/% 6
    
    max_scale <- ((max_scale %/% scale_size) + 1) * scale_size
    
    ggplot(pyramid_df, aes(x = Slope_abs)) +
      geom_bar(data = pyramid_df[pyramid_df$Slope >= 0,], aes(y = Number, fill = Group), stat="identity",width = 0.08, alpha = 0.5, colour="black") +
      geom_bar(data = pyramid_df[pyramid_df$Slope <= 0,], aes(y = -Number, fill = Group), stat="identity",width = 0.08, alpha = 0.5, colour="black") +
      scale_fill_manual("Group", values=c("red","white", "blue", "green", "orange")[pyramid_df$Group %>% unique() %>% sort()]) +
      coord_flip() +
      labs(y = "Counted number", x = "Absolute log2 ratio value") +
      scale_y_continuous(breaks = seq(-max_scale,max_scale,scale_size), 
                         labels = seq(-max_scale,max_scale,scale_size) %>% abs(),
                         limit = c(-max_scale,max_scale,scale_size)) +
      scale_x_continuous(limit = c(-0.1,input$AI_FISH_HER2_Ratio_log2_frequency_map_width),
                         breaks = (0:(input$AI_FISH_HER2_Ratio_log2_frequency_map_width*4))/4) +
      ggtitle("Negative region        Positive region") + 
      theme(legend.position = "right", 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 18),
            title = element_text(size = 18),
            plot.title = element_text(hjust = 0.5),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 18), axis.line.x = element_line())
    
    
#    Run_AI_model$model_result$available_cell$slope <- Run_AI_model$model_result$available_cell$red_count/Run_AI_model$model_result$available_cell$green_count
#    hist(log2(Run_AI_model$model_result$available_cell$slope), 
#         breaks = 1200, 
#         xlim = c(-input$AI_FISH_HER2_Ratio_log2_frequency_map_width, input$AI_FISH_HER2_Ratio_log2_frequency_map_width), 
#         las = 1, 
#         main = Run_AI_model$model_result$file_folder %>% basename(), 
#         xlab = "HER2/CEP17 log2 ratio")
  }
})

output$AI_FISH_HER2_Size_modified_flow_distribution_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    available_cell <- Run_AI_model$model_result$available_cell
    available_cell$Group <- NA
    
    average_area <- mean(available_cell$cell_area)
    
    for (i in 1:nrow(available_cell)) {
      ratio <- available_cell$red_count[i]/available_cell$green_count[i]
      mean_red <- available_cell$red_count[i]
      
      if (ratio >=2 & mean_red >= 4) {
        available_cell$Group[i] <- 1
      } else if (ratio >= 2 & mean_red < 4) {
        available_cell$Group[i] <- 2
      } else if (ratio < 2 & mean_red >= 6) {
        available_cell$Group[i] <- 3
      } else if (ratio < 2 & mean_red >= 4 & mean_red < 6) {
        available_cell$Group[i] <- 4
      } else if (ratio < 2 & mean_red < 4) {
        available_cell$Group[i] <- 5
      }
    }
    
    ggplot(available_cell, aes(x = red_count/cell_area * average_area, y = green_count/cell_area * average_area)) + 
      geom_point(aes(fill=Group %>% factor(levels = 1:5)), color = "black", size = 5, alpha = 0.5, shape = 21) +
      scale_fill_manual("Group",values=c("red","white", "blue", "green", "orange")[available_cell$Group %>% as.factor() %>% levels() %>% as.integer()]) +
      labs(y = "CEP17", x = "HER2") +
      scale_x_continuous(limit = c(0,(input$AI_FISH_HER2_Size_modified_flow_distribution_map_width)), 
                         breaks = c(0:(input$AI_FISH_HER2_Size_modified_flow_distribution_map_width)), 
                         labels = c(0:(input$AI_FISH_HER2_Size_modified_flow_distribution_map_width))) +
      scale_y_continuous(limit = c(0,(input$AI_FISH_HER2_Size_modified_flow_distribution_map_width)), 
                         breaks = c(0:(input$AI_FISH_HER2_Size_modified_flow_distribution_map_width)), 
                         labels = c(0:(input$AI_FISH_HER2_Size_modified_flow_distribution_map_width))) +
      guides(fill = guide_legend(override.aes = list(size=10))) +
      theme(legend.position = "right", 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 18))
  }
})

output$AI_FISH_HER2_Density_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    available_cell <- Run_AI_model$model_result$available_cell
    green_red_table <- xyTable(available_cell[,2], 
                               available_cell[,3])
    green_red_table <- green_red_table %>% as.data.frame()
    green_red_table$Group <- NA
    names(green_red_table)[3] <- "Number"
    for (i in 1:nrow(green_red_table)) {
      ratio <- green_red_table$x[i]/green_red_table$y[i]
      mean_red <- green_red_table$x[i]
      
      if (ratio >=2 & mean_red >= 4) {
        green_red_table$Group[i] <- 1
      } else if (ratio >= 2 & mean_red < 4) {
        green_red_table$Group[i] <- 2
      } else if (ratio < 2 & mean_red >= 6) {
        green_red_table$Group[i] <- 3
      } else if (ratio < 2 & mean_red >= 4 & mean_red < 6) {
        green_red_table$Group[i] <- 4
      } else if (ratio < 2 & mean_red < 4) {
        green_red_table$Group[i] <- 5
      }
    }
    use_plot <- ggplot(green_red_table, aes(x = x, y = y)) + 
      geom_point(aes(size = Number, fill=Group %>% factor(levels = 1:5)), color = "black", alpha = 0.5, shape = 21) + 
      scale_size_continuous(limits = c(0, 256), range = c(1,32), breaks = c(1,4,16,64,256,1024))+
      scale_fill_manual("Group",values=c("red","white", "blue", "green", "orange")[green_red_table$Group %>% as.factor() %>% levels() %>% as.integer()]) +
      scale_x_continuous(limit = c(0,(input$AI_FISH_HER2_Density_map_width)), 
                         breaks = c(0:(input$AI_FISH_HER2_Density_map_width)), 
                         labels = c(0:(input$AI_FISH_HER2_Density_map_width))) +
      scale_y_continuous(limit = c(0,(input$AI_FISH_HER2_Density_map_width)), 
                         breaks = c(0:(input$AI_FISH_HER2_Density_map_width)), 
                         labels = c(0:(input$AI_FISH_HER2_Density_map_width))) +
      labs(y = "CEP17", x = "HER2") +
      guides(fill = guide_legend(override.aes = list(size=10))) +
      theme(legend.position = "right", 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 18)) 
    use_plot_leg <- use_plot + guides(size = "none")
    guide_color <- get_legend(use_plot_leg)
    use_plot <- use_plot + guides(fill = "none") + theme(legend.position = "top")
    plot_grid(use_plot + 
                guides(fill = "none") + 
                theme(legend.direction = "horizontal", legend.position = c(0.5,0.90)), 
              guide_color, 
              ncol = 2, rel_widths = c(.90, .10))
  }
})

output$AI_FISH_HER2_Desnsity_map_summary <- renderUI({
  if (length(Run_AI_model$model_result) > 0) {
    available_cell <- Run_AI_model$model_result$available_cell
    available_cell$Group <- NA
    for (i in 1:nrow(available_cell)) {
      ratio <- available_cell$red_count[i]/available_cell$green_count[i]
      mean_red <- available_cell$red_count[i]
      
      if (ratio >=2 & mean_red >= 4) {
        available_cell$Group[i] <- 1
      } else if (ratio >= 2 & mean_red < 4) {
        available_cell$Group[i] <- 2
      } else if (ratio < 2 & mean_red >= 6) {
        available_cell$Group[i] <- 3
      } else if (ratio < 2 & mean_red >= 4 & mean_red < 6) {
        available_cell$Group[i] <- 4
      } else if (ratio < 2 & mean_red < 4) {
        available_cell$Group[i] <- 5
      }
    }
  }
  if (nrow(available_cell) > 0) {
    paste0("Counting nuclei: ", nrow(available_cell), "<br/><br/>",
           "Group 1: ", (((available_cell %>% filter(Group == 1)) %>% nrow())/nrow(available_cell) * 100) %>% round(digits = 2), "%<br/>",
           "Group 2: ", (((available_cell %>% filter(Group == 2)) %>% nrow())/nrow(available_cell) * 100) %>% round(digits = 2), "%<br/>",
           "Group 3: ", (((available_cell %>% filter(Group == 3)) %>% nrow())/nrow(available_cell) * 100) %>% round(digits = 2), "%<br/>",
           "Group 4: ", (((available_cell %>% filter(Group == 4)) %>% nrow())/nrow(available_cell) * 100) %>% round(digits = 2), "%<br/>",
           "Group 5: ", (((available_cell %>% filter(Group == 5)) %>% nrow())/nrow(available_cell) * 100) %>% round(digits = 2), "%<br/><br/>",
           "Positive: ", (((available_cell %>% filter(Group %in% c(1,3))) %>% nrow())/nrow(available_cell) * 100) %>% round(digits = 2), "%<br/>",
           "Negative: ", (((available_cell %>% filter(Group %in% c(2,4,5))) %>% nrow())/nrow(available_cell) * 100) %>% round(digits = 2), "%<br/><br/>") %>% HTML()
  }
})


output$AI_FISH_HER2_Regression_density_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    group_plot_data10 <- data.frame(
      HER2 = sapply(1:input$AI_FISH_HER2_Regression_density_map_width, function(x){1:input$AI_FISH_HER2_Regression_density_map_width}) %>% as.integer(),
      CEP = sapply(1:input$AI_FISH_HER2_Regression_density_map_width, function(x){rep(x, input$AI_FISH_HER2_Regression_density_map_width)}) %>% as.integer(),
      Group = NA
    )
    
    for (i in 1:nrow(group_plot_data10)) {
      ratio <- group_plot_data10$HER2[i]/group_plot_data10$CEP[i]
      mean_red <- group_plot_data10$HER2[i]
      
      if (ratio >=2 & mean_red >= 4) {
        group_plot_data10$Group[i] <- 1
      } else if (ratio >= 2 & mean_red < 4) {
        group_plot_data10$Group[i] <- 2
      } else if (ratio < 2 & mean_red >= 6) {
        group_plot_data10$Group[i] <- 3
      } else if (ratio < 2 & mean_red >= 4 & mean_red < 6) {
        group_plot_data10$Group[i] <- 4
      } else if (ratio < 2 & mean_red < 4) {
        group_plot_data10$Group[i] <- 5
      }
    }
    
    basal_plot10 <- ggplot(group_plot_data10, aes(HER2, CEP)) +
      geom_raster(aes(fill = Group), show.legend = TRUE, alpha = 0.5) +
      scale_fill_gradientn(colours=c("red","white", "blue", "green", "orange"), guide = "legend") +
      scale_x_continuous(limit = c(0,(input$AI_FISH_HER2_Regression_density_map_width + 1)), 
                         breaks = c(0:(input$AI_FISH_HER2_Regression_density_map_width + 1)), 
                         labels = c(0:(input$AI_FISH_HER2_Regression_density_map_width + 1))) +
      scale_y_continuous(limit = c(0,(input$AI_FISH_HER2_Regression_density_map_width + 1)), 
                         breaks = c(0:(input$AI_FISH_HER2_Regression_density_map_width + 1)), 
                         labels = c(0:(input$AI_FISH_HER2_Regression_density_map_width + 1))) +
      theme(legend.position = "right", 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 18))
    
    for (i in 1:nrow(Run_AI_model$model_result$cluster_data_plot)) {
      if (Run_AI_model$model_result$cluster_data_plot$group[i] == 1) {
        basal_plot10 <- basal_plot10 +
          geom_point(data = Run_AI_model$model_result$cluster_data_plot[i,], 
                     mapping = aes(x = HER2, y = CEP), 
                     size = Run_AI_model$model_result$cluster_data_plot$weight[i], fill = "deeppink", color = "black", shape = 21, alpha = 0.5)
      } else if (Run_AI_model$model_result$cluster_data_plot$group[i] == 2) {
        basal_plot10 <- basal_plot10 +
          geom_point(data = Run_AI_model$model_result$cluster_data_plot[i,], 
                     mapping = aes(x = HER2, y = CEP), 
                     size = Run_AI_model$model_result$cluster_data_plot$weight[i], fill = "darkgray", color = "black", shape = 21, alpha = 0.5)
      } else if (Run_AI_model$model_result$cluster_data_plot$group[i] == 3) {
        basal_plot10 <- basal_plot10 +
          geom_point(data = Run_AI_model$model_result$cluster_data_plot[i,], 
                     mapping = aes(x = HER2, y = CEP), 
                     size = Run_AI_model$model_result$cluster_data_plot$weight[i], fill = "cyan", color = "black", shape = 21, alpha = 0.5)
      } else if (Run_AI_model$model_result$cluster_data_plot$group[i] == 4) {
        basal_plot10 <- basal_plot10 +
          geom_point(data = Run_AI_model$model_result$cluster_data_plot[i,], 
                     mapping = aes(x = HER2, y = CEP), 
                     size = Run_AI_model$model_result$cluster_data_plot$weight[i], fill = "chartreuse", color = "black", shape = 21, alpha = 0.5)
      } else if (Run_AI_model$model_result$cluster_data_plot$group[i] == 5) {
        basal_plot10 <- basal_plot10 +
          geom_point(data = Run_AI_model$model_result$cluster_data_plot[i,], 
                     mapping = aes(x = HER2, y = CEP), 
                     size = Run_AI_model$model_result$cluster_data_plot$weight[i], fill = "yellow", color = "black", shape = 21, alpha = 0.5)
      }
    }
    
    #print(Run_AI_model$model_result$cluster_data_plot)
    
    
    for (i in 1:nrow(Run_AI_model$model_result$cluster_data_plot)) {
      basal_plot10 <- basal_plot10 +
        geom_text(x = Run_AI_model$model_result$cluster_data_plot$HER2[i], 
                  y = Run_AI_model$model_result$cluster_data_plot$CEP[i], 
                  label = paste0(Run_AI_model$model_result$cluster_data_plot$weight[i] %>% round(digits = 1), "%")) 
    }
    
    basal_plot10 <- basal_plot10 +
      labs(y = "CEP17", x = "HER2")
    basal_plot10
  }
})

output$AI_FISH_HER2_Papulation_map <- renderPlot({
  if (length(Run_AI_model$model_result) > 0) {
    
    group_plot_data10 <- data.frame(
      id = c(1,1,1,1,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5),
      HER2 = c(4,4,input$AI_FISH_HER2_Papulation_map_width,input$AI_FISH_HER2_Papulation_map_width,0,4,4,6,input$AI_FISH_HER2_Papulation_map_width,input$AI_FISH_HER2_Papulation_map_width,6,4,6,6,4,0,4,4,0),
      CEP17 = c(0,2,input$AI_FISH_HER2_Papulation_map_width/2,0,0,0,2,3,input$AI_FISH_HER2_Papulation_map_width/2,input$AI_FISH_HER2_Papulation_map_width,input$AI_FISH_HER2_Papulation_map_width,2,3,input$AI_FISH_HER2_Papulation_map_width,input$AI_FISH_HER2_Papulation_map_width,0,2,input$AI_FISH_HER2_Papulation_map_width,input$AI_FISH_HER2_Papulation_map_width),
      Group = c(1,1,1,1,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5)
    )
    
        
    basal_plot10 <- ggplot(group_plot_data10, aes(x = HER2, y = CEP17)) +
      geom_polygon(aes(fill = Group, group = id), alpha = 0.5) +
      scale_fill_gradientn(colours=c("red","white", "blue", "green", "orange"), guide = "legend") +
      scale_x_continuous(limit = c(0,(input$AI_FISH_HER2_Papulation_map_width)), 
                         breaks = c(0:(input$AI_FISH_HER2_Papulation_map_width)), 
                         labels = c(0:(input$AI_FISH_HER2_Papulation_map_width))) +
      scale_y_continuous(limit = c(0,(input$AI_FISH_HER2_Papulation_map_width)), 
                         breaks = c(0:(input$AI_FISH_HER2_Papulation_map_width)), 
                         labels = c(0:(input$AI_FISH_HER2_Papulation_map_width))) +
      theme(legend.position = "right", 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 18))
    
    guide_color <- get_legend(basal_plot10)
    
    basal_plot10 <- basal_plot10 + guides(fill="none")
    
    if (length(input$HER2_Papulation_map_copy_row_last_clicked) > 0) {
      if (is.null(input$HER2_Papulation_map_copy_rows_selected) == FALSE) {
        circle_table <- Run_AI_model$model_result$group_cluster_final[input$HER2_Papulation_map_copy_rows_selected,]
        for (i in 1:nrow(circle_table)) {
          basal_plot10 <- basal_plot10 + 
            geom_point(x = circle_table$HER2[i], y = circle_table$CEP[i], shape = 17, color = "yellow", size = 16)
        }
      }
    }
    
    basal_plot10 <- basal_plot10 + 
      geom_point(aes(x = HER2, y = CEP, shape = Type), color = "white", size = 8, data = Run_AI_model$model_result$group_cluster_final) +
      geom_point(aes(x = HER2, y = CEP, shape = Type, colour = Type), size = 5, data = Run_AI_model$model_result$group_cluster_final) +
      theme(legend.key = element_rect(fill = "white", colour = "black"))
    
    
    for (ii in 3:nrow(Run_AI_model$model_result$group_cluster_final)) {
      basal_plot10 <- basal_plot10 +
        geom_text(x = Run_AI_model$model_result$group_cluster_final$HER2[ii], 
                  y = Run_AI_model$model_result$group_cluster_final$CEP[ii] - 0.25 * input$AI_FISH_HER2_Papulation_map_width/10, 
                  label = paste0(Run_AI_model$model_result$group_cluster_final$weight[ii] %>% round(digits = 1), "%")) +
        geom_text(x = Run_AI_model$model_result$group_cluster_final$HER2[ii], 
                  y = Run_AI_model$model_result$group_cluster_final$CEP[ii] - 0.5 * input$AI_FISH_HER2_Papulation_map_width/10, 
                  label = paste0("(", 
                                 Run_AI_model$model_result$group_cluster_final$HER2[ii] %>% round(digits = 2), 
                                 "/",
                                 Run_AI_model$model_result$group_cluster_final$CEP[ii] %>% round(digits = 2),
                                 ")"))
    }
    
    if (length(input$HER2_Papulation_map_copy_row_last_clicked) > 0) {
      if (is.null(input$HER2_Papulation_map_copy_rows_selected) == FALSE) {
        circle_table <- Run_AI_model$model_result$group_cluster_final[input$HER2_Papulation_map_copy_rows_selected,]
        for (i in 1:nrow(circle_table)) {
          basal_plot10 <- basal_plot10 + 
            geom_point(x = circle_table$HER2[i], y = circle_table$CEP[i], shape = 2, color = "black", size = 5)
        }
      }
    }
    
    basal_plot10 <- plot_grid(basal_plot10 + 
                                labs(y = "CEP17", x = "HER2") +
                                theme(legend.position = "top", legend.text = element_text(size = 20), legend.title = element_blank()), 
                              guide_color, 
                              ncol = 2, rel_widths = c(.90, .10))
    basal_plot10
  }
})


observeEvent(input$AI_FISH_HER2_Load_FISH_image_result, {
  progressSweetAlert(
    session = session, id = "AI_FISH_HER2_myprogress",
    title = "Start AI analysis",
    display_pct = TRUE, value = 0
  )
  if (Run_AI_model$model_tag != "AI_FISH_HER2") {
    updateProgressBar(
      session = session,
      title = "Loading model...",
      id = "AI_FISH_HER2_myprogress",
      value = 10
    )
    Run_AI_model$model_option <- ""
    Run_AI_model$model_result <- list()
    source(file = "model_for_report/FISH_HER2/HER2_model/model_function.R", local = TRUE)
    Run_AI_model$model_tag <- "AI_FISH_HER2"
    Run_AI_model$model <- list()
    
    Run_AI_model$model$seg_model <- load_model_weights_hdf5(get_unet(num_classes = 1), 
                                                            filepath = "model_for_report/FISH_HER2/HER2_model/cell_border.h5")
    
    Run_AI_model$model$point_model <- load_model_weights_hdf5(get_unet(num_classes = 2), 
                                                              filepath = "model_for_report/FISH_HER2/HER2_model/point_model.h5")
  }
  
  updateProgressBar(
    session = session,
    title = paste0("Regression analysis for tumor population"),
    id = "AI_FISH_HER2_myprogress",
    value = 50
  )
  
  all_display_df <- readRDS(paste0("model_for_report/FISH_HER2/Report_history/", 
                                   input$AI_FISH_HER2_select_finished_result, 
                                   "/all_display_df.rds"))
  
  
  Run_AI_model$model_result <- list()
  file_folder <- paste0("model_for_report/FISH_HER2/Report_history/", input$AI_FISH_HER2_select_finished_result)
  Run_AI_model$model_result$file_folder <- file_folder
  Run_AI_model$model_result$all_display_df <- all_display_df
  Run_AI_model$model_result$available_cell <- all_display_df %>% filter(use_or_not)
  Run_AI_model$model_result$green_red_table <- xyTable(Run_AI_model$model_result$available_cell[,2], 
                                                       Run_AI_model$model_result$available_cell[,3])
  Run_AI_model$model_result$red_green_df <- data.frame(
    number_count = c(rep(0, 900))
  )
  
  for (i in 1:30) {
    for (j in 1:30) {
      rownames(Run_AI_model$model_result$red_green_df)[(i-1)*30 + j] <- paste0("CEP", i, "_HER", j)
    }
  }
  
  for (i in 1:nrow(Run_AI_model$model_result$available_cell)) {
    add_number <- which(rownames(Run_AI_model$model_result$red_green_df) == paste0("CEP", Run_AI_model$model_result$available_cell$green_count[i], "_HER", Run_AI_model$model_result$available_cell$red_count[i]))
    Run_AI_model$model_result$red_green_df$number_count[add_number] <- Run_AI_model$model_result$red_green_df$number_count[add_number] + 1
  }
  
  #Run_AI_model$model_result$red_green_df$number_count <- Run_AI_model$model_result$red_green_df$number_count/sum(Run_AI_model$model_result$red_green_df$number_count)
  
  #saveRDS()
  
  loss_df <- readRDS(paste0("model_for_report/FISH_HER2/HER2_fraction_list/df_", input$AI_FISH_HER2_fraction_list * 100,".rds"))
  
  if (file.exists(paste0(file_folder, "/", input$AI_FISH_HER2_fraction_list * 100, "_re_draw.rds"))) {
    updateProgressBar(
      session = session,
      title = "Drawing picture",
      id = "AI_FISH_HER2_myprogress",
      value = 95
    )
    
    Run_AI_model$model_result$cluster_data_plot <- readRDS(paste0(file_folder, "/", input$AI_FISH_HER2_fraction_list * 100, "_pre_draw.rds"))
    Run_AI_model$model_result$group_cluster_final <- readRDS(paste0(file_folder, "/", input$AI_FISH_HER2_fraction_list * 100, "_re_draw.rds"))
    run_table3 <- readRDS(paste0(file_folder, "/", input$AI_FISH_HER2_fraction_list * 100, "_run_table3.rds"))
    Run_AI_model$model_result$likelihood_ratio <- run_table3
    updateSelectInput(session, 
                      inputId = "HER2_Regression_likelihood_ratio", 
                      choices = rownames(run_table3))
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "Analysis complete!!",
      text = "The result is saved!!",
      type = "success"
    )
  } else {
    run_table <- cbind(Run_AI_model$model_result$red_green_df, loss_df)
    penalized_raw <- penalized(number_count ~ .-log2_ratio, positive = TRUE, data = run_table)
    use_name <- which((coef(penalized_raw)[-1]/coef(penalized_raw)[-1] %>% sum()) >= 0.01) %>% names()
    
    run_table2 <- cbind(Run_AI_model$model_result$red_green_df, run_table[,use_name])
    penalized_raw2 <- penalized(number_count ~ ., positive = TRUE, data = run_table2)
    
    pre_draw <- (coef(penalized_raw2)[-1]/sum(coef(penalized_raw2)[-1])*100) %>% sort(decreasing = TRUE)
    pre_draw <- data.frame(
      HER2 = names(pre_draw) %>% str_split("_HER") %>% sapply(function(x){x[2]}) %>% as.integer(),
      CEP = names(pre_draw) %>% str_split("CEP|_HER") %>% sapply(function(x){x[2]}) %>% as.integer(),
      weight = pre_draw,
      group = NA,
      P_N = NA,
      stringsAsFactors = FALSE
    )
    
    
    for (i in 1:nrow(pre_draw)) {
      ratio <- pre_draw$HER2[i]/pre_draw$CEP[i]
      mean_red <- pre_draw$HER2[i]
      
      if (ratio >=2 & mean_red >= 4) {
        pre_draw$group[i] <- 1
        pre_draw$P_N[i] <- "P"
      } else if (ratio >= 2 & mean_red < 4) {
        pre_draw$group[i] <- 2
        pre_draw$P_N[i] <- "N"
      } else if (ratio < 2 & mean_red >= 6) {
        pre_draw$group[i] <- 3
        pre_draw$P_N[i] <- "P"
      } else if (ratio < 2 & mean_red >= 4 & mean_red < 6) {
        pre_draw$group[i] <- 4
        pre_draw$P_N[i] <- "N"
      } else if (ratio < 2 & mean_red < 4) {
        pre_draw$group[i] <- 5
        pre_draw$P_N[i] <- "N"
      }
    }
    
    run_table3 <- run_table2[,rownames(pre_draw)]
    for (i in 1:nrow(pre_draw)) {
      run_table3[,rownames(pre_draw)[i]] <- run_table3[,rownames(pre_draw)[i]] * pre_draw$weight[i]
    }
    for (i in 1:nrow(run_table3)) {
      run_table3[i,] <- run_table3[i,]/sum(run_table3[i,]) * 100
    }
    #print(run_table3)
    run_table3 <- run_table3 %>% filter(is.na(run_table3[,1]) == FALSE)

    
    Run_AI_model$model_result$likelihood_ratio <- run_table3
    saveRDS(run_table3, paste0(file_folder, "/", input$AI_FISH_HER2_fraction_list * 100, "_run_table3.rds"))

    #print(pre_draw)
    #saveRDS(pre_draw, paste0(file_folder, "/pre_draw.rds"))
    updateSelectInput(session, 
                      inputId = "HER2_Regression_likelihood_ratio", 
                      choices = rownames(run_table3))
    
    re_draw <- data.frame(
      HER2 = c(Run_AI_model$model_result$available_cell$red_count %>% mean(),
               sum(pre_draw$HER2 * pre_draw$weight)/100),
      CEP = c(Run_AI_model$model_result$available_cell$green_count %>% mean(),
              sum(pre_draw$CEP * pre_draw$weight)/100),
      weight = c(1,1),
      Type = c(paste0("All(", Run_AI_model$model_result$available_cell$red_count %>% mean() %>% round(digits = 2), "/", Run_AI_model$model_result$available_cell$green_count %>% mean() %>% round(digits = 2), ")"), 
               paste0("Regression(", (sum(pre_draw$HER2 * pre_draw$weight)/100) %>% round(digits = 2), "/", (sum(pre_draw$CEP * pre_draw$weight)/100) %>% round(digits = 2), ")")),
      stringsAsFactors = FALSE
    )
    
    clone_cluster <- pre_draw
    clone_cluster$clone <- NA
    for (ii in 1:nrow(clone_cluster)) {
      if (ii == 1) {
        clone_cluster$clone[ii] <- 1
      } else {
        distance <- sapply(1:(ii - 1), function(x) {
          hor_d <- abs(clone_cluster$HER2[ii] - clone_cluster$HER2[x])
          ver_d <- abs(clone_cluster$CEP[ii] - clone_cluster$CEP[x])
          if (hor_d == 1 & ver_d == 1) {
            1
          } else {
            abs(clone_cluster$HER2[ii] - clone_cluster$HER2[x]) + abs(clone_cluster$CEP[ii] - clone_cluster$CEP[x])
          }
        })
        distance_1 <- which(distance == 1)
        if (length(distance_1) > 0) {
          clone_cluster$clone[ii] <- clone_cluster$clone[min(distance_1, na.rm = TRUE)]
        } else {
          clone_cluster$clone[ii] <- max(clone_cluster$clone, na.rm = TRUE) + 1
        }
      }
    }
    
    for (ii in 1:max(clone_cluster$clone)) {
      use_clone <- clone_cluster %>% filter(clone == ii)
      current_raw <- nrow(re_draw)
      re_draw[current_raw + 1, 1] <- (sum(use_clone$HER2 * use_clone$weight)/sum(use_clone$weight)) %>% round(digits = 2)
      re_draw[current_raw + 1, 2] <- (sum(use_clone$CEP * use_clone$weight)/sum(use_clone$weight)) %>% round(digits = 2)
      re_draw[current_raw + 1, 3] <- sum(use_clone$weight)
      re_draw[current_raw + 1, 4] <- "Population"
    }
    
    updateProgressBar(
      session = session,
      title = "Drawing picture",
      id = "AI_FISH_HER2_myprogress",
      value = 95
    )
    
    Run_AI_model$model_result$cluster_data_plot <- pre_draw
    Run_AI_model$model_result$group_cluster_final <- re_draw
    #Run_AI_model$model_result$basal_plot10 <- basal_plot10
    saveRDS(pre_draw, paste0(file_folder, "/", input$AI_FISH_HER2_fraction_list * 100, "_pre_draw.rds"))
    saveRDS(re_draw, paste0(file_folder, "/", input$AI_FISH_HER2_fraction_list * 100, "_re_draw.rds"))
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "Analysis complete!!",
      text = "The result is saved!!",
      type = "success"
    )
  }
  #print(all_display_df)
  #print(input$AI_FISH_HER2_case_name)
  #print(input$AI_FISH_HER2_upload$datapath) #image location
  #print(input$AI_FISH_HER2_upload$name) #image original name
})

output$HER2_Regression_density_map_copy = renderDT(
  options = list(pageLength = 50),
  Run_AI_model$model_result$cluster_data_plot
)

output$HER2_Papulation_map_copy = renderDT(
  options = list(pageLength = 25),
  Run_AI_model$model_result$group_cluster_final
)

output$HER2_Papulation_map_summary <- renderUI({
  if (length(input$HER2_Papulation_map_copy_row_last_clicked) > 0) {
    if (is.null(input$HER2_Papulation_map_copy_rows_selected) == FALSE) {
      circle_table <- Run_AI_model$model_result$group_cluster_final[input$HER2_Papulation_map_copy_rows_selected,]
      paste0("Counting nuclei: ", nrow(Run_AI_model$model_result$available_cell), "<br/>",
             "Selected nuclei: ", round(sum(circle_table$weight) * nrow(Run_AI_model$model_result$available_cell)/100), "<br/>",
             "Selected percentile: ", round(sum(circle_table$weight), digits = 2), "%<br/>", 
             "Selected HER2/CEP17: ", round(sum(circle_table$HER2 * circle_table$weight)/sum(circle_table$weight), digits = 2), "/", round(sum(circle_table$CEP * circle_table$weight)/sum(circle_table$weight), digits = 2), "<br/><br/>") %>% HTML()
    } else {
      paste0("Counting nuclei: ", nrow(Run_AI_model$model_result$available_cell), "<br/><br/><br/><br/><br/>") %>% HTML()
    }
  } else {
    paste0("Counting nuclei: ", nrow(Run_AI_model$model_result$available_cell), "<br/><br/><br/><br/><br/>") %>% HTML()
  }
})

output$HER2_Regression_density_map_summary <- renderUI({
      circle_table <- Run_AI_model$model_result$group_cluster_final
      if (is.null(circle_table) == FALSE) {
        if (nrow(circle_table) > 0) {
          paste0("Counting nuclei: ", nrow(Run_AI_model$model_result$available_cell), "<br/><br/>",
                 "Group 1: ", (Run_AI_model$model_result$cluster_data_plot %>% filter(group == 1))$weight %>% sum() %>% round(digits = 2), "%<br/>",
                 "Group 2: ", (Run_AI_model$model_result$cluster_data_plot %>% filter(group == 2))$weight %>% sum() %>% round(digits = 2), "%<br/>",
                 "Group 3: ", (Run_AI_model$model_result$cluster_data_plot %>% filter(group == 3))$weight %>% sum() %>% round(digits = 2), "%<br/>",
                 "Group 4: ", (Run_AI_model$model_result$cluster_data_plot %>% filter(group == 4))$weight %>% sum() %>% round(digits = 2), "%<br/>",
                 "Group 5: ", (Run_AI_model$model_result$cluster_data_plot %>% filter(group == 5))$weight %>% sum() %>% round(digits = 2), "%<br/><br/>",
                 "Positive: ", (Run_AI_model$model_result$cluster_data_plot %>% filter(group %in% base::c(1, 3)))$weight %>% sum() %>% round(digits = 2), "%<br/>",
                 "Negative: ", (Run_AI_model$model_result$cluster_data_plot %>% filter(group %in% base::c(2, 4, 5)))$weight %>% sum() %>% round(digits = 2), "%<br/><br/>") %>% HTML()
        }
      }
})

output$HER2_Regression_likelihood_ratio_summary <- renderUI({
  if (is.null(Run_AI_model$model_result$likelihood_ratio) == FALSE) {
    if (nrow(Run_AI_model$model_result$likelihood_ratio) > 0) {
      print_table <- Run_AI_model$model_result$likelihood_ratio[input$HER2_Regression_likelihood_ratio,]
      paste0(colnames(print_table), ": ", print_table[1,] %>% round(digits = 2), " %<br/>") %>% HTML()
    }
  }
})


#observeEvent(input$HER2_Regression_likelihood_ratio, {
#  if (is.null(Run_AI_model$model_result$likelihood_ratio) == FALSE) {
#    if (nrow(Run_AI_model$model_result$likelihood_ratio) > 0) {
#      print_table <- Run_AI_model$model_result$likelihood_ratio[input$HER2_Regression_likelihood_ratio,]
#    }
# }
#})
generate_file_fraction_df <- reactiveValues(df = data.frame())

output$generate_file_fraction = renderDT(
  generate_file_fraction_df$df,
  selection = 'none', server = FALSE, editable = TRUE,
  options = list(columnDefs = 
                   list(list(className = 'dt-center', 
                             targets = "_all"))))

observeEvent(input$select_generate_file_fraction_type, {
  if (input$Training_Slide_or_image == "Slide") {
    if (input$select_generate_file_fraction_type == "Slide size") {
      done_file <- paste0("www/", 
                          input$Training_Slide_or_image, 
                          "/", 
                          input$Training_Annotation_group, 
                          "/", 
                          input$Choose_file_training_table,
                          "/info/slide_size.rds")
      
      info_file <- paste0("www/", 
                          input$Training_Slide_or_image, 
                          "/", 
                          input$Training_Annotation_group, 
                          "/", 
                          input$Choose_file_training_table,
                          "/info/info.rds")
      
      #print(done_file)
      
      for (i in 1:length(done_file)) {
        if (file.exists(done_file[i])) {
          generate_file_fraction_df$df$slide_size_fraction[i] <- readRDS(done_file[i])
        } else {
          info_data <- readRDS(info_file[i])
          saveRDS(info_data$Width_pixel %>% as.numeric() * info_data$Height_pixel %>% as.numeric(), done_file[i])
          generate_file_fraction_df$df$slide_size_fraction[i] <- readRDS(done_file[i])
        }
      }
      generate_file_fraction_df$df$slide_size_fraction <- generate_file_fraction_df$df$slide_size_fraction/sum(generate_file_fraction_df$df$slide_size_fraction) * nrow(generate_file_fraction_df$df)
    } else if (input$select_generate_file_fraction_type == "Tissue size") {
      done_file <- paste0("www/", 
                          input$Training_Slide_or_image, 
                          "/", 
                          input$Training_Annotation_group, 
                          "/", 
                          input$Choose_file_training_table,
                          "/info/tissue_size.rds")
      
      image_file_folder <- paste0("www/", 
                                  input$Training_Slide_or_image, 
                                  "/", 
                                  input$Training_Annotation_group, 
                                  "/", 
                                  input$Choose_file_training_table)
      
      image_file <- sapply(image_file_folder, function(x) {list.files(x, pattern = ".ndpi", full.names = TRUE)})
      
      for (i in 1:length(done_file)) {
        if (file.exists(done_file[i])) {
          generate_file_fraction_df$df$tissue_size_fraction[i] <- readRDS(done_file[i])
        } else {
          thumb_image <- NDPI_center_RGB_batch_Num(file_name = image_file[i],
                                                   all_x_pixel = 1024 * 2^(12 - 5),
                                                   all_y_pixel = 1024 * 2^(12 - 5),
                                                   all_pyramid_layer = 5,
                                                   tilesize = 2048)
          if (AI_pre_label$tissue_model == "") {
            source("store_data_for_program/Tissue_Model.R", local = TRUE)
            AI_pre_label$tissue_model <- load_model_weights_hdf5(object = get_tissue(num_classes = 1), filepath = "store_data_for_program/Tissue_Model.h5")
          }
          thumb_image <- AI_pre_label$tissue_model$predict(thumb_image) %>% sum()
          saveRDS(thumb_image, done_file[i])
          generate_file_fraction_df$df$tissue_size_fraction[i] <- readRDS(done_file[i])
        }
      }
      generate_file_fraction_df$df$tissue_size_fraction <- generate_file_fraction_df$df$tissue_size_fraction/sum(generate_file_fraction_df$df$tissue_size_fraction) * nrow(generate_file_fraction_df$df)
    }
  }
})
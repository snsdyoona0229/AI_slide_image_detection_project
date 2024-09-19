output$body <- renderUI({
  if (USER$login == TRUE ) {
    if (is.null(input$Select_function_to_use) == FALSE) {
      if (input$Select_function_to_use == "Annotation" | input$Select_function_to_use == "Viewer") {
        conditionalPanel(condition = "input.Select_function_to_use == 'Annotation' | input.Select_function_to_use == 'Viewer'",
                         tags$style(type = "text/css", "#image_viewer {height: calc(100vh - 80px) !important;}"),
                         tags$style(type = "text/css", "#zoom_but1 {width: 50px;}"),
                         tags$style(type = "text/css", "#zoom_but2 {width: 50px;}"),
                         tags$style(type = "text/css", "#zoom_but3 {width: 50px;}"),
                         tags$style(type = "text/css", "#zoom_but4 {width: 50px;}"),
                         tags$style(type = "text/css", "#zoom_but5 {width: 50px;}"),
                         tags$style(type = "text/css", "#zoom_but6 {width: 50px;}"),
                         tags$style(type = "text/css", "#zoom_but7 {width: 50px;}"),
                         tags$style(type = "text/css", "#zoom_but8 {width: 50px;}"),
                         tags$style(type = "text/css", "#zoom_but9 {width: 50px;}"),
                         leafletOutput("image_viewer", width = "100%")#,
                         #tags$style(".leaflet-control-layers-expanded{color: red}")
        )
      } else if (input$Select_function_to_use == "Run AI") {
        if (is.null(input$customize_AI_model_viewer_switch) == FALSE) {
          if (input$customize_AI_model_viewer_switch) {
            conditionalPanel(condition = "input.Select_function_to_use == 'Run AI' & input.customize_AI_model_viewer_switch",
                             tags$style(type = "text/css", "#image_viewer {height: calc(100vh - 80px) !important;}"),
                             tags$style(type = "text/css", "#zoom_but1 {width: 50px;}"),
                             tags$style(type = "text/css", "#zoom_but2 {width: 50px;}"),
                             tags$style(type = "text/css", "#zoom_but3 {width: 50px;}"),
                             tags$style(type = "text/css", "#zoom_but4 {width: 50px;}"),
                             tags$style(type = "text/css", "#zoom_but5 {width: 50px;}"),
                             tags$style(type = "text/css", "#zoom_but6 {width: 50px;}"),
                             tags$style(type = "text/css", "#zoom_but7 {width: 50px;}"),
                             tags$style(type = "text/css", "#zoom_but8 {width: 50px;}"),
                             tags$style(type = "text/css", "#zoom_but9 {width: 50px;}"),
                             leafletOutput("image_viewer", width = "100%")#,
                             #tags$style(".leaflet-control-layers-expanded{color: red}")
            )
          } else if (input$Select_customuze_AI_to_use == "FISH/HER2") {
            source("program_script/Specific_model_run_script/FISH_HER2_UI_setting.R", local = TRUE)$value
          }  else if (input$Select_customuze_AI_to_use == "FISH/1p 19q") {
            source("program_script/Specific_model_run_script/FISH_1p_19q_UI_setting.R", local = TRUE)$value
          } else if (input$Select_customuze_AI_to_use == "FISH/CDK2 MDM4") {
            source("program_script/Specific_model_run_script/FISH_CDK2_MDM4_UI_setting.R", local = TRUE)$value
          }
		  #-----
		  else if (input$Select_customuze_AI_to_use == "Mitosis") {
            source("program_script/Specific_model_run_script/Mitosis_UI_setting.R", local = TRUE)$value
         }else if (input$Select_customuze_AI_to_use == "Gastric_biopsy") {
            source("program_script/Specific_model_run_script/Gastric_biopsy_UI_setting.R", local = TRUE)$value
         }else if (input$Select_customuze_AI_to_use == "Colon LN/cancer detection") {
            source("program_script/Specific_model_run_script/Colon_LN_cancer_detection_UI_setting.R", local = TRUE)$value
          }
		  #-----
		  else if (input$Select_customuze_AI_to_use == "Sentiinel_LN") {
            source("program_script/Specific_model_run_script/Sentiinel_LN_UI_setting.R", local = TRUE)$value
         }else if (input$Select_customuze_AI_to_use == "Glomeruli") {
            source("program_script/Specific_model_run_script/Glomeruli_UI_setting.R", local = TRUE)$value
         }else if (input$Select_customuze_AI_to_use == "Common") {
            source("program_script/Specific_model_run_script/Common_UI_setting.R", local = TRUE)$value
          }
		  #-----
        }
      } else if (input$Select_function_to_use == "Upload Image or Slide") {
        rHandsontableOutput('Upload_file_table')
      } else if (input$Select_function_to_use == "Generate Training File") {
        conditionalPanel(condition = "input.Select_function_to_use == 'Generate Training File'",
                         conditionalPanel(condition = "input.Select_generate_training_file_use_type == 'Supervised learning (Unet)'",
                                          column(4,
                                                 column(6,
                                                        actionBttn(
                                                          inputId = "select_all_generative_file",
                                                          label = "Select All",
                                                          style = "jelly", 
                                                          color = "primary"
                                                        )
                                                 ),
                                                 column(6,
                                                        actionBttn(
                                                          inputId = "delete_all_generative_file",
                                                          label = "Delete All",
                                                          style = "jelly", 
                                                          color = "danger"
                                                        )
                                                 ),
                                                 tags$style(".multi-wrapper .non-selected-wrapper, .multi-wrapper .selected-wrapper {height: calc(100vh - 200px) !important;}"),
                                                 multiInput(
                                                   width = "100%",
                                                   inputId = "Choose_file_training_table",
                                                   label = "Choose file to generate training file", 
                                                   choices = list.files(paste0("www/", input$Training_Slide_or_image, "/", input$Training_Annotation_group)) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE)
                                                 )
                                          ),
                                          column(8,
                                                 conditionalPanel(condition = "input.Training_Slide_or_image == 'Slide'",
                                                                  column(3,
                                                                         prettyRadioButtons(
                                                                           inputId = "train_on_multiple_layer_once_or_different_zoom_level",
                                                                           width = "100%",
                                                                           label = "Choose Multiple layer option",
                                                                           choices = c("Merge train", "Split train"),
                                                                           icon = icon("check"),
                                                                           selected = "Split train",
                                                                           bigger = TRUE,
                                                                           inline = TRUE,
                                                                           status = "info",
                                                                           animation = "jelly"
                                                                         )
                                                                  ),
                                                                  column(6,
                                                                         selectInput(inputId = "Slide_training_use_layer", 
                                                                                     label = "Choose layer to train (input layer)", 
                                                                                     choices = c("40x", 
                                                                                                 "20x", 
                                                                                                 "10x", 
                                                                                                 "5x", 
                                                                                                 "2.5x", 
                                                                                                 "1.25x", 
                                                                                                 "0.625x",
                                                                                                 "0.3125x"), 
                                                                                     selected = "20x", 
                                                                                     multiple = TRUE, width = "100%")
                                                                  ),
                                                                  column(3,
                                                                         selectInput(inputId = "Slide_predicting_use_layer", 
                                                                                     label = "Choose layer to predict (output layer)", 
                                                                                     choices = c("40x", 
                                                                                                 "20x", 
                                                                                                 "10x", 
                                                                                                 "5x", 
                                                                                                 "2.5x", 
                                                                                                 "1.25x", 
                                                                                                 "0.625x",
                                                                                                 "0.3125x"), 
                                                                                     selected = "20x", 
                                                                                     width = "100%")
                                                                  )
                                                 ),
                                                 tabsetPanel(
                                                   tabPanel(title = 'Point setting',
                                                            br(),
                                                            switchInput(
                                                              labelWidth = "200px",
                                                              label = "Use point label",
                                                              inputId = "turn_on_point_label_on_train",
                                                              offStatus = "danger"
                                                            ),
                                                            conditionalPanel(condition = "input.turn_on_point_label_on_train",
                                                                             rHandsontableOutput('point_label_on_train_table'),
                                                                             br(),
                                                                             selectInput("Preview_point_image", label = "Choose image for previewing weight", choices = "", width = "100%"),
                                                                             br(),
                                                                             actionBttn(
                                                                               inputId = "Preview_point_weighted_map",
                                                                               label = "Preview point weighted map", 
                                                                               style = "gradient",
                                                                               color = "primary",
                                                                               icon = icon("eye")
                                                                             )
                                                            )
                                                   ),
                                                   tabPanel(title = 'Polygon setting', 
                                                            br(),
                                                            switchInput(
                                                              labelWidth = "200px",
                                                              label = "Use polygon label",
                                                              inputId = "turn_on_polygon_label_on_train",
                                                              offStatus = "danger"
                                                            ),
                                                            conditionalPanel(condition = "input.turn_on_polygon_label_on_train",
                                                                             rHandsontableOutput('polygon_label_on_train_table'),
                                                                             br(),
                                                                             selectInput("Preview_polygon_image", label = "Choose image for previewing weight", choices = "", width = "100%"),
                                                                             br(),
                                                                             actionBttn(
                                                                               inputId = "Preview_polygon_weighted_map",
                                                                               label = "Preview polygon weighted map", 
                                                                               style = "gradient",
                                                                               color = "primary",
                                                                               icon = icon("eye")
                                                                             )
                                                            )
                                                   ),
                                                   tabPanel(title = 'Background and image augmentation setting',
                                                            br(),
                                                            textInput(inputId = "Basal_train_weight_fraction", label = "Basal train weight fraction", value = 1),
                                                            br(),
                                                            column(6,
                                                                   sliderTextInput(
                                                                     inputId = "training_red_augmentation",
                                                                     label = "Choose a Red color shift range (*)", 
                                                                     choices = (-100:100)/100,
                                                                     selected = c(-0.1,0.1),
                                                                     width = "100%"
                                                                   )
                                                            ),
                                                            column(6,
                                                                   sliderTextInput(
                                                                     inputId = "training_red_augmentation_AM",
                                                                     label = "Choose a Red color shift range (+/-)", 
                                                                     choices = (-100:100)/100,
                                                                     selected = c(-0.1,0.1),
                                                                     width = "100%"
                                                                   )
                                                            ),
                                                            br(),
                                                            column(6,
                                                                   sliderTextInput(
                                                                     inputId = "training_green_augmentation",
                                                                     label = "Choose a Green color shift range (*)", 
                                                                     choices = (-100:100)/100,
                                                                     selected = c(-0.1,0.1),
                                                                     width = "100%"
                                                                   )
                                                            ),
                                                            column(6,
                                                                   sliderTextInput(
                                                                     inputId = "training_green_augmentation_AM",
                                                                     label = "Choose a Green color shift range (+/-)", 
                                                                     choices = (-100:100)/100,
                                                                     selected = c(-0.1,0.1),
                                                                     width = "100%"
                                                                   )
                                                            ),
                                                            br(),
                                                            column(6,
                                                                   sliderTextInput(
                                                                     inputId = "training_blue_augmentation",
                                                                     label = "Choose a Blue color shift range (*)", 
                                                                     choices = (-100:100)/100,
                                                                     selected = c(-0.1,0.1),
                                                                     width = "100%"
                                                                   )
                                                            ),
                                                            column(6,
                                                                   sliderTextInput(
                                                                     inputId = "training_blue_augmentation_AM",
                                                                     label = "Choose a Blue color shift range (+/-)", 
                                                                     choices = (-100:100)/100,
                                                                     selected = c(-0.1,0.1),
                                                                     width = "100%"
                                                                   )
                                                            ),
                                                            br(),
                                                            checkboxInput(
                                                              inputId = "training_image_rotate",
                                                              label = "Will image rotate during training",
                                                              value = TRUE
                                                            ),
                                                            br(),
                                                            checkboxInput(
                                                              inputId = "training_image_flip_flop",
                                                              label = "Will image flip or flop during training",
                                                              value = TRUE
                                                            )
                                                   ),
                                                   tabPanel(title = 'Network setting', 
                                                            br(),
                                                            switchInput(width = "100%",
                                                                        labelWidth = "500px",
                                                                        handleWidth = "400px",
                                                                        value = TRUE,
                                                                        label = "Train new model or Train from existed model",
                                                                        inputId = "New_train_or_Train_on_existed_model",
                                                                        offStatus = "danger",
                                                                        offLabel = "Train from existed model", 
                                                                        onLabel = "Train new model"
                                                            ),
                                                            br(),
                                                            conditionalPanel("input.New_train_or_Train_on_existed_model",
                                                                             prettyRadioButtons(
                                                                               inputId = "training_network_class",
                                                                               label = "Choose training class",
                                                                               choices = list.files("network_database/"),
                                                                               selected = "U_net",
                                                                               icon = icon("check"),
                                                                               bigger = TRUE,
                                                                               status = "info",
                                                                               animation = "jelly"
                                                                             ),
                                                                             br(),
                                                                             prettyRadioButtons(
                                                                               inputId = "training_network_type",
                                                                               width = "100%",
                                                                               label = "Choose training network (defult: Pyramid_dense_Res_U_6_down_up)",
                                                                               choices = list.files("network_database/U_net/Get_network/"),
                                                                               icon = icon("check"),
                                                                               selected = "Pyramid_dense_Res_U_6_down_up",
                                                                               bigger = TRUE,
                                                                               status = "info",
                                                                               animation = "jelly"
                                                                             )
                                                            ),
                                                            conditionalPanel("input.New_train_or_Train_on_existed_model == 0",
                                                                             switchInput(width = "100%",
                                                                                         labelWidth = "500px",
                                                                                         handleWidth = "400px",
                                                                                         value = TRUE,
                                                                                         label = "Use current condition or previous condition",
                                                                                         inputId = "Train_on_existed_model_condition",
                                                                                         offStatus = "danger",
                                                                                         offLabel = "Train by old condition", 
                                                                                         onLabel = "Train by new condition"
                                                                             ),
                                                                             br(),
                                                                             selectInput(inputId = "Re_train_model_group",
                                                                                         label = "Model Group selection",
                                                                                         choices = list.files("model"),
                                                                             ),
                                                                             br(),
                                                                             selectInput(inputId = "Re_train_model_generation",
                                                                                         label = "Model Generation selection",
                                                                                         choices = list.files(""),
                                                                             ),
                                                                             br(),
                                                                             selectInput(inputId = "Re_train_model_type",
                                                                                         label = "Train or Valid model",
                                                                                         choices = base::c("Train", "Validation")
                                                                             )
                                                            )
                                                   ),
                                                   tabPanel(title = 'Loss setting', 
                                                            br(),
                                                            prettyRadioButtons(
                                                              inputId = "training_loss_function_type",
                                                              width = "100%",
                                                              label = "Choose loss functon (defult: DICE_weighted)",
                                                              choices = list.files("network_database/U_net/Get_loss/"),
                                                              icon = icon("check"),
                                                              selected = "DICE_weighted",
                                                              bigger = TRUE,
                                                              status = "info",
                                                              animation = "jelly"
                                                            )
                                                   ),
                                                   tabPanel(title = 'Optimizer setting',
                                                            br(),
                                                            prettyRadioButtons(
                                                              inputId = "training_optimizer_selection",
                                                              width = "100%",
                                                              label = "Choose a Optimizer for training (defult: Adam)",
                                                              choices = list.files("network_database/U_net/Get_optimizer/"),
                                                              icon = icon("check"),
                                                              selected = "Adam",
                                                              bigger = TRUE,
                                                              status = "info",
                                                              animation = "jelly"
                                                            ),
                                                            br(),
                                                            sliderTextInput(inputId = "training_learning_rate_selection", label = "Select a learning rate (defult: 0.001)",
                                                                            choices = c(1, 9:1/10, 9:1/100, 9:1/1000, 9:1/10000, 9:1/100000, 9:1/1000000),
                                                                            selected = 0.00002, grid = T, width = "100%")
                                                   ),
                                                   tabPanel(title = 'Monitor setting', 
                                                            br(),
                                                            prettyRadioButtons(
                                                              inputId = "training_monitor_type",
                                                              label = "Choose training monitor (defult: Triple_combine)",
                                                              width = "100%",
                                                              choices = list.files("network_database/U_net/Get_monitor/"),
                                                              selected = "Triple_combine",
                                                              icon = icon("check"),
                                                              bigger = TRUE,
                                                              status = "info",
                                                              animation = "jelly"
                                                            )
                                                   ),
                                                   tabPanel(title = 'Callback and training situation',
                                                            br(),
                                                            prettyRadioButtons(
                                                              inputId = "training_validation_fraction",
                                                              inline = TRUE,
                                                              label = "Choose a training fraction (fraction for training, rest for validation)",
                                                              choices = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
                                                              selected = 0.8,
                                                              icon = icon("check"),
                                                              bigger = TRUE,
                                                              status = "info",
                                                              animation = "jelly"
                                                            ),
                                                            br(),
                                                            switchInput(
                                                              labelWidth = "200px",
                                                              label = "Training selection",
                                                              onLabel = "Random",
                                                              offLabel = "Sequential",
                                                              value = TRUE,
                                                              inputId = "train_on_random_or_sequential",
                                                              offStatus = "danger"
                                                            ),
                                                            br(),
                                                            prettyRadioButtons(
                                                              inputId = "training_tile_size",
                                                              inline = TRUE,
                                                              label = "Choose a training tile size",
                                                              choices = c(64,128,256,512,1024,2048,4096,8192),
                                                              selected = 256,
                                                              icon = icon("check"),
                                                              bigger = TRUE,
                                                              status = "info",
                                                              animation = "jelly"
                                                            ),
                                                            br(),
                                                            sliderInput(
                                                              inputId = "training_batch_size",
                                                              label = "Choose a training batch size",
                                                              min = 1,
                                                              max = 128,
                                                              step = 1,
                                                              value = 12,
                                                              width = "100%"
                                                            ),
                                                            br(),
                                                            prettyRadioButtons(
                                                              inputId = "training_epoch_number",
                                                              inline = TRUE,
                                                              label = "Choose epoch number",
                                                              choices = c(1,5,10,20,50,100,200,500,1000,2000,5000,10000),
                                                              selected = 200,
                                                              icon = icon("check"),
                                                              bigger = TRUE,
                                                              status = "info",
                                                              animation = "jelly"
                                                            ),
                                                            br(),
                                                            prettyRadioButtons(
                                                              inputId = "training_early_stop_patient",
                                                              inline = TRUE,
                                                              label = "Choose stop patient",
                                                              choices = c(1,5,10,20,50,100,200),
                                                              selected = 10,
                                                              icon = icon("check"),
                                                              bigger = TRUE,
                                                              status = "info",
                                                              animation = "jelly"
                                                            )
                                                   ),
                                                   tabPanel(title = 'Model name and location setting',
                                                            br(),
                                                            selectInput(inputId = "training_model_group", label = "Select training model group", choices = list.files("model")),
                                                            br(),
                                                            textInput(inputId = "training_model_name", label = "Training model name")
                                                   ),
                                                   tabPanel(title = 'File fraction modify',
                                                            br(),
                                                            radioGroupButtons(
                                                              inputId = "select_generate_file_fraction_type",
                                                              label = "Select file fraction type",
                                                              choices = c("Custom value", "Slide size", "Tissue size"),
                                                              justified = TRUE,
                                                              checkIcon = list(
                                                                yes = icon("ok", lib = "glyphicon"))
                                                            ),
                                                            br(),
                                                            DTOutput('generate_file_fraction')
                                                   ),
                                                   tabPanel(title = 'Start generate training file',
                                                            br(),
                                                            selectInput("Start_generate_training_file_cpu_number", 
                                                                        label = "Select Number of CPU to use (defult: 0.9 in percentile)", 
                                                                        choices = c(nbrOfWorkers():1), 
                                                                        selected = round(nbrOfWorkers() * 0.9),
                                                                        width = "100%"),
                                                            br(),
                                                            actionBttn(
                                                              inputId = "Start_generate_training_file",
                                                              label = "Start Generating", 
                                                              style = "gradient",
                                                              color = "primary",
                                                              icon = icon("cubes")
                                                            )
                                                   )
                                                 )
                                          )
                         )
        )
      } else if (input$Select_function_to_use == "Authorization code") {
        conditionalPanel(condition = "input.Select_function_to_use == 'Authorization code'",
                         conditionalPanel(condition = "input.authorization_function_selection == 'Function'",
                                          rHandsontableOutput('Authorization_code_table'),
                                          br(),
                                          actionBttn(inputId = "save_new_authorization", label = "Save changed!")  
                         ),
                         conditionalPanel(condition = "input.authorization_function_selection == 'Delete'",
                                          selectInput("Authorization_delete_user", label = "Select user to delete", choices =Authorization_table$df$user, multiple = TRUE),
                                          br(),
                                          actionBttn(inputId = "delete_authorization", label = "Delete authorization!"),
										  br(),
										  #---1226---Add table
										  br(),
										  rHandsontableOutput('Add_Authorization_code_table'),
                                          br(),
										  br(),
                                          actionBttn(inputId = "Add_new_authorization", label = "Add authorization!")
										  #---1226---Add table
                         ),
                         conditionalPanel(condition = "input.authorization_function_selection == 'Slide'",
                                          column(8,
                                                 tags$style(".multi-wrapper .non-selected-wrapper, .multi-wrapper .selected-wrapper {height: calc(100vh - 150px) !important;}"),
                                                 multiInput(
                                                   width = "100%",
                                                   inputId = "Authorization_slide_for_user",
                                                   label = "Choose Slide to let user see!", 
                                                   choices = ""
                                                 )
                                          ),
                                          column(4,
                                                 selectInput("Authorization_slide_group", label = "Select Slide group", choices = list.files("www/Slide")),
												 #---0213---
                                                 checkboxInput(
                                                   inputId = "view_all_slide_group_check",
                                                   label = "Mark user to see all slides group", 
                                                   value = FALSE
                                                 ),
												 #---0213---
                                                 br(),
                                                 br(),
                                                 selectInput("Authorization_slide_user", label = "Select user to authorize", choices = Authorization_table$df$user, multiple = TRUE),
                                                 br(),
                                                 br(),
                                                 checkboxInput(
                                                   inputId = "view_all_slide_check",
                                                   label = "Mark user to see all slides", 
                                                   value = FALSE
                                                 ),
                                                 br(),
                                                 br(),
                                                 actionBttn(inputId = "save_Authorization_slide", label = "Save changed!") 
                                          )
                         ),
                         conditionalPanel(condition = "input.authorization_function_selection == 'Image'",
                                          column(8,
                                                 tags$style(".multi-wrapper .non-selected-wrapper, .multi-wrapper .selected-wrapper {height: calc(100vh - 150px) !important;}"),
                                                 multiInput(
                                                   width = "100%",
                                                   inputId = "Authorization_image_for_user",
                                                   label = "Choose Image to let user see!", 
                                                   choices = ""
                                                 )
                                          ),
                                          column(4,
                                                 selectInput("Authorization_image_group", label = "Select Image group", choices = list.files("www/Image")),
											     #---0213---
                                                 checkboxInput(
                                                   inputId = "view_all_image_group_check",
                                                   label = "Mark user to see all images group", 
                                                   value = FALSE
                                                 ),
												 #---0213---
                                                 br(),
                                                 br(),
                                                 selectInput("Authorization_image_user", label = "Select user to authorize", choices = Authorization_table$df$user, multiple = TRUE),
                                                 br(),
                                                 br(),
                                                 checkboxInput(
                                                   inputId = "view_all_image_check",
                                                   label = "Mark user to see all images", 
                                                   value = FALSE
                                                 ),
                                                 br(),
                                                 br(),
                                                 actionBttn(inputId = "save_Authorization_image", label = "Save changed!") 
                                          )
                         )
        )
      } else if (input$Select_function_to_use == "Folder management") {
        conditionalPanel(condition = "input.Select_function_to_use == 'Folder management'",
                         selectInput(inputId = "folder_management_option",
                                     label = "Select Major option",
                                     choices = c("Create", 
                                                 "Rename", 
                                                 "Delete"),
                                     selected = "Slide group"
                         ),
                         selectInput(inputId = "folder_management_type",
                                     label = "Folder type",
                                     choices = c("Slide group",
                                                 "Image group",
                                                 "Model group"),
                                     selected = "Slide group"
                         ),
                         br(),
                         br(),
                         uiOutput("folder_managent_UI")
        )
      } else if (input$Select_function_to_use == "Contact us") {
        conditionalPanel(condition = "input.Select_function_to_use == 'Contact us'",
                         column(4,
                                h1("Project Director"),
                                br(),
                                img(src='Picture/VGH.png', height = "80%", width = "80%"),
                                br(),
                                img(src='Picture/LWY.jpg', height = "50%", width = "50%"),
                                br(),
                                h3("Dr. Liang, Wen-Yih/梁文議"),
                                br(),
                                h4("Section chief of General Pathology, Taipei Veterans General Hospital"),
                                br(),
                                h4("No.201, Sec. 2, Shipai Rd., Beitou District, Taipei City, Taiwan 11217, R.O.C."),
                                br(),
                                h4("Email: wyliang@vghtpe.gov.tw")   
                         ),
                         column(4,
                                h1("Developer and Programmer"),
                                br(),
                                img(src='Picture/CGMH.png', height = "80%", width = "80%"),
                                br(),
                                br(),
                                br(),
                                img(src='Picture/WHS.jpg', height = "50%", width = "50%"),
                                br(),
                                h3("Dr. Wang, Hsiang-Sheng/王翔生"),
                                br(),
                                h4("Resident of General Pathology, Chang-Geng Memorial Hospital, Linkou branch"),
                                br(),
                                h4("No. 5, Fuxing St., Guishan Dist., Taoyuan City 333, Taiwan (R.O.C.)"),
                                br(),
                                h4("Email: wanghsiang1@yahoo.com.tw / wanghsiang1@gmail.com"),
                                br(),
                                h4("Back up Email: whs2009@cgu.edu.tw")
                         ),
                         column(4,
                                h1("Program Consultation"),
                                br(),
                                img(src='Picture/CWC.jpg', height = "50%", width = "50%"),
                                br(),
                                h3("M.E Chang, Che-Wei/張哲偉"),
                                br(),
                                h4("Email: pd8073@gmail.com"),
                         )
        )
      } else if (input$Select_function_to_use == "Reference") {
        conditionalPanel(condition = "input.Select_function_to_use == 'Reference'",
                         column(3,
                                h1("R package"),
                                p("=== web design ==="),
                                p("shiny"),
                                p("shinydashboard"),
                                p("shinydashboardPlus"),
                                p("shinyWidgets"),
                                p("shinyjqui"),
                                p("shinyjs"),
                                p("htmlwidgets"),
                                p("=== web table ==="),
                                p("rhandsontable"),
                                p("DT"),
                                p("=== viewer and annotation ==="),
                                p("leaflet"),
                                p("leaflet.extras"),
                                p("leafpm"),
                                p("polyclip"),
                                p("lawn")
                         ),
                         column(3,
                                h1("R package"),
                                p("=== image and text processing ==="),
                                p("stringr"),
                                p("dplyr"),
                                p("raster"),
                                p("EBImage"),
                                p("=== interact with C/C++ ==="),
                                p("Rcpp"),
                                p("RcppParallel"),
                                p("RcppGSL"),
                                p("=== interact with JavaScript ==="),
                                p("V8"),
                                p("=== interact with python ==="),
                                p("reticulate"),
                                p("=== interact with machinge learning ==="),
                                p("tensorflow"),
                                p("keras"),
                                p("=== parallel computing ==="),
                                p("future")
                         ),
                         column(3,
                                h1("python package"),
                                p("=== interact with machinge learning ==="),
                                p("tensorflow"),
                                p("tf-agents"),
                                p("keras"),
                                p("autokeras")
                         ),
                         column(3,
                                h1("3rd party SDK"),
                                p("=== Hamamatsu ==="),
                                p("NDPI"),
                                p("=== Philips ==="),
                                p("iSyntax"),
                                p("=== Others ==="),
                                p("openslide"),
                                p("Bioformats")
                         )
        )
      } else if (input$Select_function_to_use == "Tiling Slide") {
        conditionalPanel(condition = "input.Select_function_to_use == 'Tiling Slide'",
                         selectInput(inputId = "Tiling_CPU_batch_size", 
                                     label = "Choose CPU batch size for tiling", 
                                     choices = c(16,32,64,128,256), 
                                     selected = 64),
                         br(),
                         br(),
                         selectInput(inputId = "Tiling_group", label = "Choose tiling group", choices = list.files("www/Slide")),
                         br(),
                         br(),
                         column(6,
                                actionBttn(inputId = "Tiling_select_all_slide", label = "Select all slides", color = "warning"),
                         ),
                         column(6,
                                actionBttn(inputId = "Tiling_remove_all_slide", label = "Remove all slides", color = "warning"),
                         ),
                         selectInput(inputId = "Tiling_slide", label = "Choose tiling slide", choices = "", multiple = TRUE, width = "100%"),
                         br(),
                         br(),
                         actionBttn(inputId = "Tiling_start", label = "Start tiling!")
        )
      } else if (input$Select_function_to_use == "Fast Slide move") {
        conditionalPanel(condition = "input.Select_function_to_use == 'Fast Slide move'",
                         br(),
                         radioButtons(inputId = "Fast_process_type", 
                                      label = "Select fast process type",
                                      choices = c("From fast process folder", "Direct select file"),
                                      inline = TRUE, 
                                      selected = "From fast process folder"),
                         radioButtons(inputId = "Fast_process_select_file_process_type", 
                                      label = "Process type",
                                      choices = c("Copy", "Move"), 
                                      inline = TRUE, 
                                      selected = "Move"),
                         br(),
                         conditionalPanel(condition = "input.Fast_process_type == 'Direct select file'",
                                          shinyFilesButton(id = 'Fast_slide_proces_files', label = 'File select', title = 'Please select a file', multiple = TRUE)
                                          ),
                         br(),
						 
                         rHandsontableOutput('Fast_slide_file_table'),
                         br(),
                         actionBttn(inputId = "Fast_slide_process_start", label = "Start processing", color = "primary"),
        )
      } else if (input$Select_function_to_use == "Microbenchmark") {
        conditionalPanel(condition = "input.Select_function_to_use == 'Microbenchmark'",
                         actionBttn(inputId = "Start_system_analysis", label = "Start system analysis", color = "primary"),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         column(3,
                                selectInput(inputId = "Microbenchmark_slide_group", 
                                            label = "Choose slide group for microbenchmark", 
                                            choices = list.files("www/Slide"))
                         ),
                         column(3,
                                selectInput(inputId = "Microbenchmark_slide_use", 
                                            label = "Choose slide for microbenchmark", 
                                            choices = "")
                         ),
                         column(3,
                                selectInput(inputId = "Microbenchmark_slide_tile_size", 
                                            label = "Choose slide tile size for microbenchmark", 
                                            choices = c(128,256,512,1024,2048,4096),
                                            multiple = TRUE,
                                            selected = 1024)
                         ),
                         column(3,
                                selectInput(inputId = "Microbenchmark_slide_batch_size", 
                                            label = "Choose slide batch size for microbenchmark", 
                                            choices = c(1:32), 
                                            multiple = TRUE,
                                            selected = c(1,2,4,8,16))
                         ),
                         selectInput(inputId = "Microbenchmark_slide_micro_times", 
                                     label = "Choose trying times for microbenchmark", 
                                     choices = c(1:100),
                                     selected = 10),
                         actionBttn(inputId = "Start_tile_size_and_cpu_batch_size_analysis", label = "Start NDPI tiling size and CPU batch size analysis", color = "primary"),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         column(3,
                                selectInput(inputId = "Microbenchmark_model_test", 
                                            label = "Choose model for microbenchmark", 
                                            choices = list.files("model/"))
                         ),
                         column(3,
                                selectInput(inputId = "Microbenchmark_GPU_slide_tile_size", 
                                            label = "Choose slide tile size for microbenchmark", 
                                            choices = c(128,256,512,1024,2048,4096),
                                            multiple = TRUE,
                                            selected = 1024)
                         ),
                         column(3,
                                selectInput(inputId = "Microbenchmark_CPU_slide_batch_size", 
                                            label = "Choose slide batch size of CPU for microbenchmark", 
                                            choices = c(1:32), 
                                            multiple = TRUE,
                                            selected = c(1,2,4,8,16))
                         ),
                         column(3,
                                selectInput(inputId = "Microbenchmark_GPU_slide_batch_sizee", 
                                            label = "Choose slide batch size of GPU for microbenchmark", 
                                            choices = c(1:32),
                                            multiple = TRUE,
                                            selected = c(1:3))
                         ),
                         selectInput(inputId = "Microbenchmark_GPU_o_times", 
                                     label = "Choose trying times for microbenchmark", 
                                     choices = c(1:100),
                                     selected = 10),
                         actionBttn(inputId = "Start_gpu_batch_size_analysis", label = "Start GPU batch size analysis", color = "primary")
        )
      } else {
        p("working...")
      }
    }
  }
  else {
    loginpage
  }
})

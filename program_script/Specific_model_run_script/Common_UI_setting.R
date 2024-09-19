conditionalPanel(condition = "input.Select_customuze_AI_to_use == 'Common'",
                column(3,
                        tabsetPanel(
                          tabPanel("Upload Common image", 
                                   br(),
						               
                          shinyFilesButton('Fast_slide_proces_files_Run_AI_file',label='Please select a file',title='Choose Files',multiple=TRUE),
						 br(),
						 
						 rHandsontableOutput('Fast_slide_file_table_Run_AI'),

                         br(),
						      		   
                          ),		   
                           

                          tabPanel("Load Common history image", 
                                 
                                 
                                
                                       actionBttn(
                                         inputId = "select_all_generative_file_Run_AI",
                                         label = "Select All",
                                         style = "jelly", 
                                         color = "primary"
                                       ),
                                
                                
                                       actionBttn(
                                         inputId = "delete_all_generative_file_Run_AI",
                                         label = "Delete All",
                                         style = "jelly", 
                                         color = "danger"
                                       ),
                                
                                tags$style(".multi-wrapper .non-selected-wrapper, .multi-wrapper .selected-wrapper {height: calc(100vh - 200px) !important;}"),
                                multiInput(
                                  width = "100%",
                                  inputId = "Choose_file_training_table_Run_AI",
                                  label = "Choose file to generate training file", 
                                  choices = list.files(paste0("www/", input$Slide_or_image, "/", input$Annotation_group)) %>% grep(pattern = '.rds', inv = TRUE, value = TRUE) %>% str_sort(numeric = TRUE)
       
                                )
                         
						  
						          )
						       )
						  ),
                          
						   column(9,
						   
						   titlePanel("Common interface manipulate:"),
                       
                           verticalTabsetPanel(
                           id = "vertical_tab_panel_AI_prelabel_Run_AI",
                           contentWidth = 11,
                           verticalTabPanel(
                             title = "Unet",
                             box_height = "100px",
                             icon = icon("hive", "fa-2x"),
                             column(2,
                                    selectInput("AI_pre_label_model_group_Run_AI", label = "select AI model group", choices = list.files("model"))  
                             ),
                             column(2,
                                    selectInput("AI_pre_label_model_Run_AI", label = "select AI model to use", choices = "")
                             ),
                             column(5,
                                    radioGroupButtons(
                                      label = "Model type (fit by validation or training data set)",
                                      status = "primary",
                                      inputId = "AI_pre_label_train_or_validation_Run_AI",
                                      choices = c("By Validation Set", "By Training Set"),
                                      justified = TRUE,
                                      checkIcon = list(
                                        yes = icon("ok", 
                                                   lib = "glyphicon"))
                                    )
                             ),
                             column(3,
                                    br(),
                                    textOutput("model_slide_image_condition_Run_AI"),
                                    textOutput("AI_pre_label_slide_input_type_Run_AI"),
                                    textOutput("AI_pre_label_slide_input_layer_Run_AI")
                             ),
                             column(12,
                                    conditionalPanel(condition = "input.Slide_or_image == 'Slide'",
                                                     column(3,
                                                            selectizeInput("AI_pre_label_in_slide_layer_Run_AI", 
                                                                           label = "Select apply layer", 
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
                                                                           multiple = TRUE,
                                                                           width = "100%")
                                                     ),
                                                     column(3,
                                                            switchInput(
                                                              inputId = "Tissue_only_or_All_slide_switch_Run_AI",
                                                              onLabel = "Tissue only",
                                                              offLabel = "All slide",
                                                              handleWidth = "20%",
                                                              value = TRUE,
                                                              label = "Tissue vs All", 
                                                              labelWidth = "100%"
                                                            )
                                                     ),
                                                     column(6,
                                                            conditionalPanel(condition = "input.Tissue_only_or_All_slide_switch",
                                                                             column(6,
                                                                                    sliderInput(
                                                                                      inputId = "Tissue_only_dilate_pixel_Run_AI",
                                                                                      label = "Dilated pixel size (defult:5)",
                                                                                      min = 1,
                                                                                      max = 16,
                                                                                      step = 1,
                                                                                      value = 5,
                                                                                      width = "100%"
                                                                                    )
                                                                             ),
                                                                             column(6,
                                                                                    sliderInput(
                                                                                      inputId = "Tissue_only_threshold_Run_AI",
                                                                                      label = "Pixel threshold (defult:0.8)",
                                                                                      min = 0,
                                                                                      max = 1,
                                                                                      step = 0.1,
                                                                                      value = 0.8,
                                                                                      width = "100%"
                                                                                    )
                                                                             )
                                                            )
                                                     ),
                                                     column(12,
                                                            column(3,
                                                                   selectInput("AI_pre_label_in_slide_tile_size_Run_AI", 
                                                                               label = "Select tile size", 
                                                                               choices = c(128, 256, 512, 1024, 2048, 4096),
                                                                               selected = 1024, 
                                                                               width = "100%")
                                                            ),
                                                            column(3,
                                                                   selectInput("AI_pre_label_in_slide_overlap_pixel_Run_AI", 
                                                                               label = "Select tile overlap pixels", 
                                                                               choices = c(0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512),
                                                                               selected = 0, 
                                                                               width = "100%")
                                                            ),
                                                            column(3,
                                                                   selectInput("AI_pre_label_in_slide_batch_size_Run_AI", 
                                                                               label = "Select run GPU batch size", 
                                                                               choices = c(1:64), 
                                                                               selected = 1, 
                                                                               width = "100%")
                                                            ),
                                                            column(3,
                                                                   selectInput("AI_pre_label_in_slide_CPU_modify_batch_size_Run_AI", 
                                                                               label = "Select modify CPU batch size", 
                                                                               choices = c(2,4,8,16,32,64,128,256,512,1024), 
                                                                               selected = 16, 
                                                                               width = "100%")
                                                            )
                                                     )
                                    )
                             ),
                             column(12,
                                    p("Choose layers to put result in"),
                                    rHandsontableOutput('AI_pre_label_table_Run_AI'),
                                    column(12,
                                           column(12,
                                                  column(2,
                                                         br(),
                                                         actionBttn(
                                                           inputId = "Defult_AI_prelabel_condition_Run_AI",
                                                           label = "Defult",
                                                           style = "unite", 
                                                           color = "primary"
                                                         )
                                                  ),
                                                  column(2,
                                                         br(),
                                                         actionBttn(
                                                           inputId = "Load_AI_prelabel_condition_Run_AI",
                                                           label = "Load",
                                                           style = "unite", 
                                                           color = "warning"
                                                         )
                                                  ),
                                                  column(2,
                                                         br(),
                                                         actionBttn(
                                                           inputId = "Save_AI_prelabel_condition_Run_AI",
                                                           label = "Save",
                                                           style = "unite", 
                                                           color = "success"
                                                         )
                                                  ),
                                                  column(2,
                                                         br(),
                                                         actionBttn(
                                                           inputId = "Delete_AI_prelabel_condition_Run_AI",
                                                           label = "Delete",
                                                           style = "unite", 
                                                           color = "danger"
                                                         )
                                                  ),
                                                  column(4,
                                                         br(),
                                                         actionBttn(
                                                           inputId = "Start_AI_prelabel_Run_AI",
                                                           label = "Start AI labeling", 
                                                           style = "material-flat",
                                                           color = "primary"
                                                         )
                                                  )#,
                                           )#,
                                    )
                             ),
                             column(12,
                                    p("Use_in_label: This layer should be used in inference or not. (Y/N)"),
                                    p("Replace_Add_clip: Replace whole layer by AI annotation (R)/ Merge AI annotation and original annotation (A)/ Clip Original annotation by AI annotation (C)"),
                                    p("Fill_hole: Holes within labeled region should be automatically filled out or not. (Y/N)"),
                                    p("Inference_G/P/H: Apply to polygon/point/heatmap label. Acceptable layers from 1_1_1_G/P/H_1 to 20_20_20_G/P/H_10"),
                                    p("Threshold: Threshold form image processing (0.00 - 1.00). Defult: 0.60"),
                                    p("Ignore_pixel_min/max: Pixel size lesser/larger than which value of the AI annotated object should be ignored. Defult: 0/-"),
                                    p("Polygon_simplify: Simplify coefficient of AI annotated polygon. From 0 (Without simplification [Not recommended]) to 0.001. Defult: 0.00005")
                             )
                           ),
                           verticalTabPanel(
                             title = "Classification",
                             box_height = "100px",
                             icon = icon("hive", "fa-2x"),
                             "Content panel 2"
                           ),
                           verticalTabPanel(
                             title = "CAM",
                             box_height = "100px",
                             icon = icon("hive", "fa-2x"),
                             "Content panel 3"
                           ),
                           verticalTabPanel(
                             title = "DQL,DDQL",
                             box_height = "100px",
                             icon = icon("arrows-spin", "fa-2x"),
                             "Content panel 4"
                           ),
                           verticalTabPanel(
                             title = "Setting",
                             box_height = "100px",
                             icon = icon("gear", "fa-2x"),
                             prettyRadioButtons(
                               inputId = "AI_labeled_restricted_area_Run_AI",
                               label = "Restric result on selected region", 
                               choices = c("No", 
                                           "Yes only", 
                                           "Yes plus tissue"),
                               selected = "No"
                             ),
                             selectInput("AI_pre_label_model_restricted_primary_layer_Run_AI", label = "Select Primary layer", choices = c(1:10)),
                             selectInput("AI_pre_label_model_restricted_secondary_layer_Run_AI", label = "Select Secondary layer", choices = c(1:10)),
                             selectInput("AI_pre_label_model_restricted_tertiary_layer_Run_AI", label = "Select Tertiary layer", choices = c(1:10))
                           )
                         )
					    )  
                      )
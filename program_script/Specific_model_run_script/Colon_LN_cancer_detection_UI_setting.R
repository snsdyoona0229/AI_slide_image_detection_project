conditionalPanel(condition = "input.Select_customuze_AI_to_use == 'Colon LN/cancer detection'",
                column(3,
                        tabsetPanel(
                          tabPanel("Upload LN image", 
                                   br(),

						shinyFilesButton('Fast_slide_proces_files_Run_AI_file',label='Please select a file',title='Choose Files',multiple=TRUE),
						 
						 rHandsontableOutput('Fast_slide_file_table_Run_AI'),

                         br(),
						      		   
                          ),		   
                           

                          tabPanel("Load Colon LN history image", 
                                 

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
						   
						  
						   titlePanel("Simplify interface manipulate:"),
						 
						   actionBttn(
                                 inputId = "Start_AI_prelabel_Run_AI_Simplify",
                                 label = "Start AI labeling", 
                                 style = "material-flat",
                                 color = "primary"
                                 ),
						 
					    )  
                      )
                       

				 
           


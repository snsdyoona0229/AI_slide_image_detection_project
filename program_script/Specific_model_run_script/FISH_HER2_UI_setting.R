conditionalPanel(condition = "input.Select_customuze_AI_to_use == 'FISH/HER2'",
                 column(3,
                        sliderInput(
                          inputId = "AI_FISH_HER2_fraction_list",
                          label = "L/(2R+L) ratio (defult:0.25)",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          value = 0.25,
                          width = "100%"
                        ),
                        tabsetPanel(
                          tabPanel("Upload FISH image", 
                                   br(),
                                   fileInput(inputId = "AI_FISH_HER2_upload", 
                                             label = "Upload FISH image for HER2 analysis", 
                                             buttonLabel = "Upload...",
                                             accept = "image/jpeg", 
                                             multiple = TRUE),
                                   column(12,
                                          textInput(inputId = "AI_FISH_HER2_case_name",
                                                    width = "100%",
                                                    label = NULL,
                                                    placeholder = "Enter Pathology Number..."),
                                          br(),
                                          br(),
                                          actionButton(inputId = "AI_FISH_HER2_start_analysis",
                                                       label = "Start analysis")
                                   ),
                                   tableOutput("AI_FISH_HER2_files")
                          ),
                          tabPanel("Load FISH history image", 
                                   br(),
                                   selectInput(inputId = "AI_FISH_HER2_select_finished_result",
                                               label = "Select finished result",
                                               choices = list.files("model_for_report/FISH_HER2/Report_history")),
                                   br(),
                                   actionButton(inputId = "AI_FISH_HER2_Load_FISH_image_result",
                                                label = "Start loading result and analysis")
                          )
                      )
                        
                 ),
                 column(9,
                        tabsetPanel(
                          tabPanel("Ratio log2 frequency map", 
                                   sliderInput(
                                     inputId = "AI_FISH_HER2_Ratio_log2_frequency_map_width",
                                     label = "Axis width",
                                     min = 1,
                                     max = 10,
                                     step = 0.25,
                                     value = 4,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_HER2_Ratio_log2_frequency_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Size modified flow distribution map", 
                                   sliderInput(
                                     inputId = "AI_FISH_HER2_Size_modified_flow_distribution_map_width",
                                     label = "Axis width",
                                     min = 1,
                                     max = 30,
                                     step = 1,
                                     value = 10,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_HER2_Size_modified_flow_distribution_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Density map", 
                                   sliderInput(
                                     inputId = "AI_FISH_HER2_Density_map_width",
                                     label = "Axis width",
                                     min = 1,
                                     max = 30,
                                     step = 1,
                                     value = 10,
                                     width = "100%"
                                   ),
                                   column(2,
                                          htmlOutput("AI_FISH_HER2_Desnsity_map_summary")
                                   ),
                                   column(10,
                                          plotOutput("AI_FISH_HER2_Density_map", 
                                                     width = "600px", 
                                                     height = "600px")
                                   )
                          ),
                          tabPanel("Regression map", 
                                   sliderInput(
                                     inputId = "AI_FISH_HER2_Regression_density_map_width",
                                     label = "Axis width",
                                     min = 4,
                                     max = 30,
                                     step = 1,
                                     value = 10,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_HER2_Regression_density_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Regression map table", 
                                   column(4,
                                          htmlOutput("HER2_Regression_density_map_summary"),
                                          br(),
                                          selectInput("HER2_Regression_likelihood_ratio",
                                                      label = "Papulation", 
                                                      choices = ""),
                                          htmlOutput("HER2_Regression_likelihood_ratio_summary")
                                   ),
                                   column(8,
                                          DTOutput('HER2_Regression_density_map_copy')
                                   )
                          ),
                          tabPanel("Papulation map", 
                                   column(6,
                                          sliderInput(
                                            inputId = "AI_FISH_HER2_Papulation_map_width",
                                            label = "Axis width",
                                            min = 4,
                                            max = 30,
                                            step = 1,
                                            value = 10,
                                            width = "100%"
                                          ),
                                          plotOutput("AI_FISH_HER2_Papulation_map", 
                                                     width = "100%", height = "600px")
                                   ),
                                   column(6,
                                          htmlOutput("HER2_Papulation_map_summary"),
                                          DTOutput('HER2_Papulation_map_copy')
                                   )
                          )
                        )
                 )
)
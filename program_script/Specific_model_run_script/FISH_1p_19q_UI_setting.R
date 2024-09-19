conditionalPanel(condition = "input.Select_customuze_AI_to_use == 'FISH/1p 19q'",
                 column(4,
                        sliderInput(
                          inputId = "AI_FISH_1p_19q_fraction_list",
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
                                   column(6,
                                          fileInput(inputId = "AI_FISH_1p_upload", 
                                                    label = "Upload FISH image for 1p analysis", 
                                                    buttonLabel = "Upload...",
                                                    accept = "image/jpeg", 
                                                    multiple = TRUE)
                                   ),
                                   column(6,
                                          fileInput(inputId = "AI_FISH_19q_upload", 
                                                    label = "Upload FISH image for 19q analysis", 
                                                    buttonLabel = "Upload...",
                                                    accept = "image/jpeg", 
                                                    multiple = TRUE)
                                   ),
                                   column(12,
                                          textInput(inputId = "AI_FISH_1p_19q_case_name",
                                                    width = "100%",
                                                    label = NULL,
                                                    placeholder = "Enter Pathology Number..."),
                                          br(),
                                          br(),
                                          actionButton(inputId = "AI_FISH_1p_19q_start_analysis",
                                                       label = "Start analysis")
                                   ),
                                   column(6,
                                          tableOutput("AI_FISH_1p_files")
                                   ),
                                   column(6,
                                          tableOutput("AI_FISH_19q_files")
                                   )
                          ),
                          tabPanel("Load FISH history image", 
                                   br(),
                                   selectInput(inputId = "AI_FISH_1p_19q_select_finished_result",
                                               label = "Select finished result",
                                               choices = list.files("model_for_report/FISH_1p_19q/Report_history")),
                                   br(),
                                   actionButton(inputId = "AI_FISH_1p_19q_Load_FISH_image_result",
                                                label = "Start loading result and analysis")
                          )
                      )
                        
                 ),
                 column(8,
                        tabsetPanel(
                          tabPanel("Ratio log2 frequency map (1p)", 
                                   sliderInput(
                                     inputId = "AI_FISH_1p_Ratio_log2_frequency_map_width",
                                     label = "Axis width",
                                     min = 1,
                                     max = 10,
                                     step = 1,
                                     value = 4,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_1p_Ratio_log2_frequency_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Size modified flow distribution map (1p)", 
                                   sliderInput(
                                     inputId = "AI_FISH_1p_Size_modified_flow_distribution_map_width",
                                     label = "Axis width",
                                     min = 1,
                                     max = 30,
                                     step = 1,
                                     value = 10,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_1p_Size_modified_flow_distribution_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Density map (1p)", 
                                   sliderInput(
                                     inputId = "AI_FISH_1p_Density_map_width",
                                     label = "Axis width",
                                     min = 1,
                                     max = 30,
                                     step = 1,
                                     value = 10,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_1p_Density_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Regression map (1p)", 
                                   sliderInput(
                                     inputId = "AI_FISH_1p_Regression_density_map_width",
                                     label = "Axis width",
                                     min = 4,
                                     max = 30,
                                     step = 1,
                                     value = 10,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_1p_Regression_density_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Regression map table (1p)", 
                                   DTOutput('Regression_density_map_copy_1p')
                                   
                          ),
                          tabPanel("Papulation map (1p)", 
                                   sliderInput(
                                     inputId = "AI_FISH_1p_Papulation_map_width",
                                     label = "Axis width",
                                     min = 4,
                                     max = 30,
                                     step = 1,
                                     value = 10,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_1p_Papulation_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Papulation map table (1p)", 
                                   DTOutput('Papulation_map_copy_1p')
                          ),
                          tabPanel("Ratio log2 frequency map (19q)", 
                                   sliderInput(
                                     inputId = "AI_FISH_19q_Ratio_log2_frequency_map_width",
                                     label = "Axis width",
                                     min = 1,
                                     max = 10,
                                     step = 1,
                                     value = 4,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_19q_Ratio_log2_frequency_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Size modified flow distribution map (19q)", 
                                   sliderInput(
                                     inputId = "AI_FISH_19q_Size_modified_flow_distribution_map_width",
                                     label = "Axis width",
                                     min = 1,
                                     max = 30,
                                     step = 1,
                                     value = 10,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_19q_Size_modified_flow_distribution_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Density map (19q)", 
                                   sliderInput(
                                     inputId = "AI_FISH_19q_Density_map_width",
                                     label = "Axis width",
                                     min = 1,
                                     max = 30,
                                     step = 1,
                                     value = 10,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_19q_Density_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Regression map (19q)", 
                                   sliderInput(
                                     inputId = "AI_FISH_19q_Regression_density_map_width",
                                     label = "Axis width",
                                     min = 4,
                                     max = 30,
                                     step = 1,
                                     value = 10,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_19q_Regression_density_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Regression map table (19q)", 
                                   DTOutput('Regression_density_map_copy_19q')
                                   
                          ),
                          tabPanel("Papulation map (19q)", 
                                   sliderInput(
                                     inputId = "AI_FISH_19q_Papulation_map_width",
                                     label = "Axis width",
                                     min = 4,
                                     max = 30,
                                     step = 1,
                                     value = 10,
                                     width = "100%"
                                   ),
                                   plotOutput("AI_FISH_19q_Papulation_map", 
                                              width = "600px", 
                                              height = "600px")
                          ),
                          tabPanel("Papulation map table (19q)", 
                                   DTOutput('Papulation_map_copy_19q')
                          )
                        )
                 )
)
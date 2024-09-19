output$sidebarpanel <- renderUI({
  if (USER$login == TRUE){ 
    sidebarMenu(
      tags$style(".dropdown-menu {background-color: #000000;}"),
      tags$style(HTML('#sw-content-dropdown, .sw-dropdown-in {background-color: #000000;}')),

	  tags$style(".handsontable.listbox td {background: 'white';}"),
	  tags$style(".handsontable.listbox td.htDimmed {color: black;}"),
	

      br(),
      conditionalPanel(condition = "input.Select_function_to_use == 'Generate Training File'",
                       dropdownButton(
                         selectInput(inputId = "Training_Slide_or_image", label = "Choose slide or image to upload", choices = c("Image", "Slide")),
                         selectInput(inputId = "Training_Annotation_group", label = "Choose upload group", choices = "Ki_counting"),
                         inputId = "Training_option_selection_drop_down",
                         label = "Training file option",
                         icon = icon("brain"),
                         status = "primary",
                         circle = FALSE,
                         width = "500px"
                       )
      ),
	  
	  
      conditionalPanel(condition = "input.Select_function_to_use == 'Upload Image or Slide'",
                       dropdownButton(
                         selectInput(inputId = "upload_Slide_or_image", label = "Choose slide or image to upload", choices = c("Image", "Slide")),
                         selectInput(inputId = "upload_Annotation_group", label = "Choose upload group", choices = "Ki_counting"),
						 ####
						 br(),
						 shinyFilesButton('upload_image_or_slide_file',label="Choose a file",title='Choose Files',multiple=TRUE),
                         br(),
#                        fileInput(inputId = "upload_image_or_slide_file", "Choose a file", multiple = TRUE, accept = c('image/png', 
#                                                                                                                        'image/jpeg', 
#                                                                                                                        'image/jpg', 
#                                                                                                                       'image/tif', 
#                                                                                                                        'image/tiff', 
#                                                                                                                        "image/bmp", 
#                                                                                                                        ".ndpi", 
#                                                                                                                        ".isyntax",
#                                                                                                                        ".svs",
#                                                                                                                        ".tif",
#                                                                                                                        ".vms",
#                                                                                                                        ".vmu",
#                                                                                                                        ".scn",
#                                                                                                                        ".mrxs",
#                                                                                                                        ".tiff",
#                                                                                                                        ".svslide",
#                                                                                                                        ".bif")),
                         uiOutput("cover_file_to_location_button"),
                         h3("Current system only support formats list as following:"),
                         p("Image: .png/.jpg/.tif/.tiff/.bmp"),
                         h3("Slide:"),
                         p("--Aperio (.svs, .tif)"),
                         p("--Hamamatsu (.vms, .vmu, .ndpi)"),
                         p("--Leica (.scn)"),
                         p("--MIRAX (.mrxs)"),
                         p("--Philips (.tiff, .isyntax[working...])"),
                         p("--Sakura (.svslide)"),
                         p("--Trestle (.tif)"),
                         p("--Ventana (.bif, .tif)"),
                         p("--Generic tiled TIFF (.tif)"),
                         inputId = "Upload_option_selection_drop_down",
                         label = "File upload option",
                         icon = icon("upload"),
                         status = "primary",
                         circle = FALSE,
                         width = "800px"
                       )
      ),
      conditionalPanel(condition = "input.Select_function_to_use == 'Annotation' | input.Select_function_to_use == 'Viewer' | input.Select_function_to_use == 'Run AI'" ,
                       dropdownButton(
                         selectInput(inputId = "Slide_or_image", label = "Choose slide or image viewer", choices = c("Image", "Slide"), selected = "Slide"),
                         selectInput(inputId = "Annotation_group", label = "Choose annotation group", choices = "XXXXXXXXXX"),
                         selectInput(inputId = "Image_input", label = "Choose one image/slide", choices = "XXXXXXXXXX"),
                         inputId = "select_image_drop_down",
                         label = "Select image",
                         icon = icon("object-group"),
                         status = "primary",
                         circle = FALSE,
                         width = "500px"
                       )
			   
      ),
	 
      conditionalPanel(condition = "input.Select_function_to_use == 'Annotation'" ,
                       br(),
                       dropdownButton(
                         verticalTabsetPanel(
                           id = "vertical_tab_panel_AI_prelabel",
                           contentWidth = 11,
                           verticalTabPanel(
                             title = "Unet",
                             box_height = "100px",
                             icon = icon("hive", "fa-2x"),
                             column(2,
                                    selectInput("AI_pre_label_model_group", label = "select AI model group", choices = list.files("model"))  
                             ),
                             column(2,
                                    selectInput("AI_pre_label_model", label = "select AI model to use", choices = "")
                             ),
                             column(5,
                                    radioGroupButtons(
                                      label = "Model type (fit by validation or training data set)",
                                      status = "primary",
                                      inputId = "AI_pre_label_train_or_validation",
                                      choices = c("By Validation Set", "By Training Set"),
                                      justified = TRUE,
                                      checkIcon = list(
                                        yes = icon("ok", 
                                                   lib = "glyphicon"))
                                    )
                             ),
                             column(3,
                                    br(),
                                    textOutput("model_slide_image_condition"),
                                    textOutput("AI_pre_label_slide_input_type"),
                                    textOutput("AI_pre_label_slide_input_layer")
                             ),
                             column(12,
                                    conditionalPanel(condition = "input.Slide_or_image == 'Slide'",
                                                     column(3,
                                                            selectizeInput("AI_pre_label_in_slide_layer", 
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
                                                              inputId = "Tissue_only_or_All_slide_switch",
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
                                                                                      inputId = "Tissue_only_dilate_pixel",
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
                                                                                      inputId = "Tissue_only_threshold",
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
                                                                   selectInput("AI_pre_label_in_slide_tile_size", 
                                                                               label = "Select tile size", 
                                                                               choices = c(128, 256, 512, 1024, 2048, 4096),
                                                                               selected = 1024, 
                                                                               width = "100%")
                                                            ),
                                                            column(3,
                                                                   selectInput("AI_pre_label_in_slide_overlap_pixel", 
                                                                               label = "Select tile overlap pixels", 
                                                                               choices = c(0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512),
                                                                               selected = 0, 
                                                                               width = "100%")
                                                            ),
                                                            column(3,
                                                                   selectInput("AI_pre_label_in_slide_batch_size", 
                                                                               label = "Select run GPU batch size", 
                                                                               choices = c(1:64), 
                                                                               selected = 1, 
                                                                               width = "100%")
                                                            ),
                                                            column(3,
                                                                   selectInput("AI_pre_label_in_slide_CPU_modify_batch_size", 
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
                                    rHandsontableOutput('AI_pre_label_table'),
                                    column(12,
                                           column(12,
                                                  column(2,
                                                         br(),
                                                         actionBttn(
                                                           inputId = "Defult_AI_prelabel_condition",
                                                           label = "Defult",
                                                           style = "unite", 
                                                           color = "primary"
                                                         )
                                                  ),
                                                  column(2,
                                                         br(),
                                                         actionBttn(
                                                           inputId = "Load_AI_prelabel_condition",
                                                           label = "Load",
                                                           style = "unite", 
                                                           color = "warning"
                                                         )
                                                  ),
                                                  column(2,
                                                         br(),
                                                         actionBttn(
                                                           inputId = "Save_AI_prelabel_condition",
                                                           label = "Save",
                                                           style = "unite", 
                                                           color = "success"
                                                         )
                                                  ),
                                                  column(2,
                                                         br(),
                                                         actionBttn(
                                                           inputId = "Delete_AI_prelabel_condition",
                                                           label = "Delete",
                                                           style = "unite", 
                                                           color = "danger"
                                                         )
                                                  ),
                                                  column(4,
                                                         br(),
                                                         actionBttn(
                                                           inputId = "Start_AI_prelabel",
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
                               inputId = "AI_labeled_restricted_area",
                               label = "Restric result on selected region", 
                               choices = c("No", 
                                           "Yes only", 
                                           "Yes plus tissue"),
                               selected = "No"
                             ),
                             selectInput("AI_pre_label_model_restricted_primary_layer", label = "Select Primary layer", choices = c(1:10)),
                             selectInput("AI_pre_label_model_restricted_secondary_layer", label = "Select Secondary layer", choices = c(1:10)),
                             selectInput("AI_pre_label_model_restricted_tertiary_layer", label = "Select Tertiary layer", choices = c(1:10))
                           )
                         ),
                         inputId = "AI_pre_label",
                         label = "Use AI pre-label",
                         icon = icon("android"),
                         status = "primary",
                         circle = FALSE,
                         width = "1800px"
                       )
      ),
	   conditionalPanel(condition = "input.Select_function_to_use == 'Annotation' | input.Select_function_to_use == 'Viewer' | input.Select_function_to_use == 'Run AI' " ,
	                   br(),
                       dropdownButton(
					   
					     selectInput(inputId = "file_management_group", label = "Choose Slide group", choices = list.files("www/Slide")),
                         br(),
                         selectInput(inputId = "file_management_slide", label = "Choose slide", choices = "", multiple = TRUE, width = "100%"),
                         br(),
						 checkboxInput(
                                       inputId = "All_group_files",
                                       label = "slides group all files", 
                                       value = FALSE
                                       ),
						column(3,
                        tabsetPanel(
                          tabPanel("move files", 
						  
						  column(3,
						  checkboxInput(
                                       inputId = "move",
                                       label = "move", 
                                       value = TRUE
                                       )
									   ),
						  column(6,
						  checkboxInput(
                                       inputId = "copy",
                                       label = "copy", 
                                       value = FALSE
									   )
                                       ),
						  
						  br(),
                         column(10,						  
                         rHandsontableOutput('file_management_slide_table', width = "800px",height = "200px")),
						 br(),
						 actionBttn(inputId = "file_management_start", label = "Start processing files!"),
                         br(),   		   
                          ),		   
                          tabPanel("remove files", 
                           
						   column(10,	
                           rHandsontableOutput('remove_slide_table', width = "800px", height = "200px")),
						   br(),
						   actionBttn(inputId = "file_management_remove", label = "remove files!"),
 
						          )
						       )
						  ),
                         
                         inputId = "side_folder_managemen",
                         label = "Side_folder_managemen",
                         icon = icon("object-group"),
                         status = "primary",
                         circle = FALSE,
                         width = "1500px"
                       )
					  
					   
      ),
      conditionalPanel(condition = "input.Select_function_to_use == 'Annotation'| input.Select_function_to_use == 'Run AI'",
                       br(),
                       dropdownButton(
                         verticalTabsetPanel(
                           id = "Polygon_label_vertical_tab_panel",
                           contentWidth = 11,
                           verticalTabPanel(
                             title = "Polygon",
                             box_height = "100px",
                             icon = icon("bookmark", "fa-2x"),
                             column(12,
                                    column(4,
                                           sliderInput(
                                             inputId = "Polygon_opacity_value",
                                             label = "Polygon opacity",
                                             min = 0,
                                             max = 1,
                                             value = 0,
                                             step = 0.1
                                           )#,
                                    ),
                                    column(4,
                                           actionBttn(
                                             inputId = "Un_do",
                                             label = "UN-DO",
                                             style = "fill", 
                                             color = "danger"
                                           )
                                    ),
                                    column(4,
                                           actionBttn(
                                             inputId = "Re_do",
                                             label = "RE-DO",
                                             style = "fill", 
                                             color = "primary"
                                           )
                                    )
                             ),
                             column(12,
                                    orderInput(inputId = 'Change_polygon_color_order', 
                                               label = 'Polygons color order', 
                                               items = color_name_list_polygons),
                                    radioButtons("Draw_polygon_layers", 
                                                 label = "Choose polygons layers",
                                                 choices = c(1:10), 
                                                 inline = TRUE,
                                                 selected = 1)
                             ),
                             column(12,
                                    column(4,
                                           selectInput("Polygon_class_1",label = "Primary polygon class", choices = c(1:20), selected = 1)
                                    ),
                                    column(4,
                                           selectInput("Polygon_class_2",label = "Secondary polygon class", choices = c(1:20), selected = 1)
                                    ),
                                    column(4,
                                           selectInput("Polygon_class_3",label = "Tertiary polygon class", choices = c(1:20), selected = 1)
                                    )
                             ),
                             actionBttn(
                               inputId = "Save_polygons_data",
                               label = "Save polygons data", 
                               style = "material-flat",
                               color = "primary"
                             )
                           ),
                           verticalTabPanel(
                             title = "Layer",
                             box_height = "100px",
                             icon = icon("layer-group", "fa-2x"),
                             p("Polygon list table"),
                             rHandsontableOutput('Polygons_note_table'),
                             actionBttn(
                               inputId = "Save_polygons_note_data",
                               label = "Save polygons list data", 
                               style = "material-flat",
                               color = "primary"
                             )
                           ),
                           verticalTabPanel(
                             title = "Free draw",
                             box_height = "100px",
                             icon = icon("edit", "fa-2x",lib = "glyphicon"),
                             column(4,
                                    h2("Polygon free draw"),
                                    p("Press 'shift + z' to start drawing"),
                                    p("Press 'shift + a' to remove last 10 drawing points"),
                                    p("Press 'shift + 1-0' to set polygons layers from 1-10"),
                                    p("Press others to end drawing"),
                                    p("Press 'shift + s' to save drawn to polygon"),
                                    p("Press 'shift + d' to clip drawn area"),
                                    p("Press 'shift + r' to remove drawing"),
                                    p("Mouse right click to delete element")
                             ),
                             column(8,
                                    switchInput(
                                      inputId = "Polygon_free_draw_on_off",
                                      label = "Free draw", 
                                      labelWidth = "100px"
                                    ),
                                    selectInput(
                                      inputId = "Polygon_free_draw_color",
                                      choices = color_name_list_polygons,
                                      label = "Free draw temperal color" 
                                    ),
                                    sliderInput(
                                      inputId = "Polygon_free_draw_simplify",
                                      label = "Polygon simplify(0: no simplify, 0.00005: defult)",
                                      min = 0,
                                      max = 0.001,
                                      step = 0.00001,
                                      value = 0.00005,
                                      width = "100%"
                                    )
                             )
                           ),
                           verticalTabPanel(
                             title = "Area count",
                             box_height = "100px",
                             icon = icon("shapes", "fa-2x"),
                             switchInput(
                               inputId = "fix_area_draw_on_off",
                               label = "Start fix area draw", 
                               labelWidth = "300px"
                             ),
                             radioGroupButtons(
                               inputId = "fix_area_polygon_or_circle",
                               label = "Fix area of Polygon or Circle",
                               choices = c("Polygon", "Circle"),
                               checkIcon = list(
                                 yes = tags$i(class = "fa fa-check-square", 
                                              style = "color: steelblue"),
                                 no = tags$i(class = "fa fa-square-o", 
                                             style = "color: steelblue"))
                             ),
                             radioGroupButtons(
                               inputId = "fix_area_size_selection",
                               label = "Fix area Size (mm2)",
                               choices = c(0.1,0.25,0.33,0.5,0.75,1,2,3,4,5,8,10, "other value"),
                               checkIcon = list(
                                 yes = tags$i(class = "fa fa-check-square", 
                                              style = "color: steelblue"),
                                 no = tags$i(class = "fa fa-square-o", 
                                             style = "color: steelblue"))
                             ),
                             textInput("fix_area_size_ready_to_draw", label = "Size ready to draw", value = ""),
                             br(),
                             br(),
                             column(4,
                                    selectInput("fix_area_auto_hotspot_number", label = "Auto Hotspot Number size", choices = 1:10, selected = 1)
                             ),
                             column(5,
                                    textOutput("fix_area_detect_point_layer"),
                                    p("Please go to [Point label] -> [Point] to change detected point layer"),
                                    br(),
                                    textOutput("fix_area_draw_polygon_layer"),
                                    p("Please go to [Polygon label] -> [Polygon] to change detected polygon layer")
                             ),
                             column(3,
                                    actionBttn(
                                      inputId = "fix_area_auto_hotspot_draw",
                                      label = "Auto Hotspot Detection",
                                      style = "unite", 
                                      color = "success"
                                    )
                             )
                           ),
                           verticalTabPanel(
                             title = "Mutation",
                             box_height = "100px",
                             icon = icon("cubes-stacked", "fa-2x"),
                             p("Polygon Manipulation"),
                           )
                         ),
                         inputId = "polygon_drop_down",
                         label = "Polygon label",
                         icon = icon("bookmark"),
                         status = "primary",
                         circle = FALSE,
                         width = "1400px"
                       ),
                       br(),
                       dropdownButton(
                         verticalTabsetPanel(
                           id = "Point_label_vertical_tab_panel",
                           contentWidth = 11,
                           verticalTabPanel(
                             title = "Point",
                             box_height = "100px",
                             icon = icon("bullseye", "fa-2x"),
                             column(4,
                                    switchInput(
                                      inputId = "Point_circle_switch",
                                      label = "Point to circle marker", 
                                      labelWidth = "250px"
                                    )
                             ),
                             column(4,
                                    sliderInput(
                                      inputId = "Point_radius_setting",
                                      label = "Select Point radius(defult: 7)",
                                      min = 1,
                                      max = 20,
                                      step = 1,
                                      value = 7,
                                      width = "100%"
                                    )
                             ),
                             column(4,
                                    sliderInput(
                                      inputId = "Circle_radius_setting",
                                      label = "Select Circle radius(defult: 20)",
                                      min = 1,
                                      max = 50,
                                      step = 1,
                                      value = 20,
                                      width = "100%"
                                    )
                             ),
                             column(12,
                                    orderInput(inputId = 'Change_point_color_order', 
                                               label = 'Points color order', 
                                               items = color_name_list_points),
                                    radioGroupButtons(
                                      label = "Draw points options",
                                      status = "primary",
                                      inputId = "Start_to_draw_point",
                                      choices = c("None", "Add points", "Remove points"),
                                      justified = TRUE,
                                      checkIcon = list(
                                        yes = icon("ok", 
                                                   lib = "glyphicon"))
                                    )
                             ),
                             radioButtons("Draw_point_layers", 
                                          label = "Choose points layers",
                                          choices = c(1:10), 
                                          inline = TRUE,
                                          selected = 1),
                             column(12,
                                    column(4,
                                           selectInput("Point_class_1",label = "Primary point class", choices = c(1:20), selected = 1)
                                    ),
                                    column(4,
                                           selectInput("Point_class_2",label = "Secondary point class", choices = c(1:20), selected = 1)
                                    ),
                                    column(4,
                                           selectInput("Point_class_3",label = "Tertiary point class", choices = c(1:20), selected = 1)
                                    )
                             ),
                             actionBttn(
                               inputId = "Save_points_data",
                               label = "Save points data", 
                               style = "material-flat",
                               color = "primary"
                             )
                           ),
                           verticalTabPanel(
                             title = "Layer",
                             box_height = "100px",
                             icon = icon("layer-group", "fa-2x"),
                             p("Point list table"),
                             rHandsontableOutput('Points_note_table'),
                             actionBttn(
                               inputId = "Save_points_note_data",
                               label = "Save points list data", 
                               style = "material-flat",
                               color = "primary"
                             )
                           ),
                           verticalTabPanel(
                             title = "Free draw",
                             box_height = "100px",
                             icon = icon("edit", "fa-2x", lib = "glyphicon"),
                             column(4,
                                    h2("Point free draw"),
                                    p("Press '1-0' to set points layers from 1-10"),
                                    p("Press 'q' to set point draw to 'None'"),
                                    p("Press 'w' to set point draw to 'Add points'"),
                                    p("Press 'e' to set point draw to 'Remove points'"),
                                    p("Mouse right click to delete element")
                             ),
                             column(8,
                                    switchInput(
                                      inputId = "Point_free_draw_on_off",
                                      label = "Free draw", 
                                      labelWidth = "100px"
                                    )
                             )
                           ),
                           verticalTabPanel(
                             title = "Mutation",
                             box_height = "100px",
                             icon = icon("cubes-stacked", "fa-2x"),
                             p("Point Manipulation"),
                           )
                         ),
                         inputId = "point_drop_down",
                         label = "Point label",
                         icon = icon("bullseye"),
                         status = "primary",
                         circle = FALSE,
                         width = "1400px"
                       ),
                       br(),
                       dropdownButton(
                         verticalTabsetPanel(
                           id = "Tag_label_vertical_tab_panel",
                           contentWidth = 11,
                           verticalTabPanel(
                             title = "Tag",
                             box_height = "100px",
                             icon = icon("tag", "fa-2x"),
                             selectInput("Tag_class",label = "Select Tag class", choices = "", multiple = TRUE),
                             actionBttn(
                               inputId = "Save_tags_data",
                               label = "Save tags data", 
                               style = "material-flat",
                               color = "primary"
                             )
                           ),
                           verticalTabPanel(
                             title = "Layer",
                             box_height = "100px",
                             icon = icon("layer-group", "fa-2x"),
                             p("Tag list table"),
                             rHandsontableOutput('Tags_note_table'),
                             actionBttn(
                               inputId = "Save_tags_not_data",
                               label = "Save tags list", 
                               style = "material-flat",
                               color = "primary"
                             )
                           ),
                           verticalTabPanel(
                             title = "Mutation",
                             box_height = "100px",
                             icon = icon("cubes-stacked", "fa-2x"),
                             p("Tag Manipulation"),
                           )
                         ),
                         inputId = "tag_drop_down",
                         label = "Tag label",
                         icon = icon("tag"),
                         status = "primary",
                         circle = FALSE,
                         width = "1400px"
                       ),
                       br(),
                       dropdownButton(
                         column(12,
                                column(6,
                                       actionBttn(
                                         inputId = "Delete_heat_data",
                                         label = "Delete heatmaps data", 
                                         style = "material-flat",
                                         color = "danger"
                                       )
                                ),
                                column(6,
                                       actionBttn(
                                         inputId = "Redraw_heat_data",
                                         label = "Redraw heatmaps data", 
                                         style = "material-flat",
                                         color = "primary"
                                       )
                                )
                         ),
                         column(12,
                                column(4,
                                       sliderInput(
                                         inputId = "Heat_opacity_value",
                                         label = "Heat opacity",
                                         min = 0,
                                         max = 1,
                                         value = 0.7,
                                         step = 0.1
                                       )
                                ),
                                column(8,
                                       orderInput(inputId = 'Change_heat_color_order', 
                                                  label = 'Heat maps color order', 
                                                  items = color_name_list)
                                )
                         ),
                         radioButtons("Draw_heat_layers", 
                                      label = "Choose heat layers",
                                      choices = c(1:10), 
                                      inline = TRUE,
                                      selected = 1),
                         column(12,
                                column(4,
                                       selectInput("Heat_class_1",label = "Primary heat class", choices = c(1:20), selected = 1)
                                ),
                                column(4,
                                       selectInput("Heat_class_2",label = "Secondary heat class", choices = c(1:20), selected = 1)
                                ),
                                column(4,
                                       selectInput("Heat_class_3",label = "Tertiary heat class", choices = c(1:20), selected = 1)
                                )
                         ),
                         inputId = "heat_drop_down",
                         label = "Heat label",
                         icon = icon("ghost"),
                         status = "primary",
                         circle = FALSE,
                         width = "1000px"
                       )
      ),
      conditionalPanel(condition = "(input.Select_function_to_use == 'Annotation' | input.Select_function_to_use == 'Viewer' | input.Select_function_to_use == 'Run AI') & input.Slide_or_image == 'Slide'",
                       br(),
                       dropdownButton(
                         uiOutput(outputId = "slide_image_preview"),
                         inputId = "Slide_pre_view",
                         label = "Slide pre-view",
                         icon = icon("eye"),
                         status = "primary",
                         circle = FALSE,
                         width = "500px"
                       )
      ),
	  
	   conditionalPanel(condition = "(input.Select_function_to_use == 'Annotation' | input.Select_function_to_use == 'Viewer' | input.Select_function_to_use == 'Run AI') & input.Slide_or_image == 'Slide'",
                       br(),
                       dropdownButton(
                         selectInput("Slide_viewer_option",label = "Choose Slide viewer format", choices = c("JPG", "Tile", "RAW"), selected = "JPG"),
                         selectInput("Slide_minimap_option",label = "Choose minimap size", choices = c("small", "medium", "large", "giant"), selected = "medium"),
                         inputId = "select_slide_option",
                         label = "Slide option",
                         icon = icon("cog",lib = "glyphicon"),
                         status = "primary",
                         circle = FALSE,
                         width = "500px"
                       )
      ),
	  
      conditionalPanel(condition = "input.Select_function_to_use == 'Authorization code'",
	                 
                       dropdownButton(
                         selectInput(inputId = "authorization_function_selection", label = "Choose authorization function", choices = c("Function", "Slide", "Image", "Delete")),
                         p("Function: which function can be used by user!"),
                         p("Slide: which slide can be seen by user!"),
                         p("Image: which image can be seen by user!"),
                         inputId = "select_authorization_function",
                         label = "Authorization Function",
                         icon = icon("handshake"),
                         status = "primary",
                         circle = FALSE,
                         width = "500px"
                       )
      )

    )
  }
})
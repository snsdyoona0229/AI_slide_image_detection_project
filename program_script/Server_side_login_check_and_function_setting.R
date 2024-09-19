login <- FALSE
USER <- reactiveValues(login = login)
current_user_autherization <- reactiveValues(df = "",
                                             admin = "",
                                             load_list = "")

observe({ 
  credentials <- Authorization_table$df
  if (USER$login == FALSE) {
    if (!is.null(input$login)) {
      if (input$login > 0) {
        Username <- input$userName
        Password <- input$passwd
        if(length(which(credentials$user == Username))==1) { 
          pasmatch  <- credentials["password"][which(credentials$user == Username),]
          pasverify <- pasmatch == Password
          if(pasverify) {
            USER$login <- TRUE
            
            current_user_autherization$df <- Authorization_table$df[Authorization_table$df$user == input$userName,]
            current_user_autherization$admin <- current_user_autherization$df[1,colnames(current_user_autherization$df) %>% str_detect("Authoriza") %>% which()]
            current_user_autherization$load_list <- load_list_function(input$userName)
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } else {
          shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
          shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
        }
      } 
    }
  }    
})

output$logoutbtn <- renderUI({
  req(USER$login)
  tags$li(a(icon("off",lib = "glyphicon"), "Logout", 
            href="javascript:window.location.reload(true)"),
          class = "dropdown", 
          style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
})

output$function_selection <- renderUI({
  req(USER$login)
  user_all <- read.csv("account_and_login_data/login_file.csv", stringsAsFactors = FALSE)
  current_user_data <- user_all[which(user_all$user == input$userName),]
  open_name <- (current_user_data %>% names())[which(current_user_data == 1)] %>% str_replace_all("\\.", " ")
  dropdownButton(
    selectInput(inputId = "Select_function_to_use", label = "Select function to use", choices = open_name),
    inputId = "Select_function_to_use_drop_down",
    label = "Select Function to Use",
    icon = icon("laptop"),
    status = "danger",
    circle = FALSE,
    width = "500px"
  )
})

output$additional_pdf_file_selection <- renderUI({
  req(USER$login & (input$Select_function_to_use == "Annotation") | (input$Select_function_to_use == "Viewer"))
  dropdownButton(
    selectInput(inputId = "Select_pdf_file_to_see", 
                label = "Select addtional pdf file for information", 
                choices = list.files(paste0("www/", 
                                            input$Slide_or_image,
                                            "/",
                                            input$Annotation_group,
                                            "/",
                                            input$Image_input), pattern = ".pdf")),
    actionBttn(inputId = "Open_additional_pdf_file", label = "Open PDF file"),
    inputId = "Select_pdf_file_to_see_drop_down",
    label = "Select PDF file for additional information",
    icon = icon("file-pdf"),
    status = "warning",
    circle = FALSE,
    width = "500px"
  )
})

output$customize_AI_report <- renderUI({
  req(USER$login & (input$Select_function_to_use == "Run AI"))
  dropdownButton(
    switchInput(
      inputId = "customize_AI_model_viewer_switch",
      label = "Switch to viewer", 
      labelWidth = "200px"
    ),
    selectInput(inputId = "Select_customuze_AI_to_use", 
                label = shiny::HTML("<p><span style='color: white'>Select customize AI to use</span></p>"), 
                choices = c("Common",
							"Colon LN/cancer detection",
							"Sentiinel_LN",
							"Mitosis", 
                            "Gastric_biopsy",
							"Glomeruli",
							"FISH/HER2",
							"FISH/1p 19q", 
                            "FISH/CDK2 MDM4")),
    inputId = "Select_customuze_AI_to_use_drop_down",
    label = "Select AI for report generation",
    icon = icon("keyboard"),
    circle = FALSE,
    width = "500px"
  )
})


output$import_annotation_file <- renderUI({
  req(USER$login & (input$Select_function_to_use == "Annotation") & (input$Slide_or_image == "Slide"))
  dropdownButton(
    p("Current avalible format:", style = "color:white"),
    p("--- NanoZoomer (.ndpa)", style = "color:white"),
    p("--- AetherAI (.json)", style = "color:white"),
    p("--- Our platform (.rds)", style = "color:white"),
    shinyFilesButton(id = 'import_annotation_file', 
                     label = 'File select', 
                     title = 'Please select a file', 
                     multiple = FALSE),
    inputId = "Import_annotation_file_to_use_drop_down",
    label = "Import annotation file",
    icon = icon("share",lib = "glyphicon"),
    status = "success",
    circle = FALSE,
    width = "500px"
  )
})

observeEvent(input$Select_function_to_use, {
  if (input$Select_function_to_use %in% c("Annotation", "Viewer", "Run AI")) {
    if (input$sidebar) {
      updateSidebar("sidebar")
    }
  } else {
    if (input$sidebar == FALSE) {
      updateSidebar("sidebar")
    }
  }
})

output$train_AI_selection_in_generate_file <- renderUI({
  req(USER$login & (input$Select_function_to_use == "Generate Training File"))
  dropdownButton(
    selectInput(inputId = "Select_generate_training_file_use_type", 
                label = shiny::HTML("<p><span style='color: white'>Select training method</span></p>"), 
                choices = c("Supervised learning (Unet)",
                            "Semi-Supervised learning (CAM)",
                            "Reinforcement learning (DQL,DDQL)")),
    inputId = "Select_customuze_training_AI_file_type_to_use_drop_down",
    label = "Select Training method for AI creation",
    icon = icon("atom"),
    circle = FALSE,
    width = "500px"
  )
})




							
							 
							

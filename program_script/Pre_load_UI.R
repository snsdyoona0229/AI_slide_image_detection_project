ui <- dashboardPage(
  #sidebar_fullCollapse = TRUE,
  header = dashboardHeader(
    title = "Digital Pathology", 
    leftUi = tagList(
      uiOutput("function_selection"),
      uiOutput("additional_pdf_file_selection"),
      uiOutput("import_annotation_file"),
      uiOutput("customize_AI_report"),
      uiOutput("train_AI_selection_in_generate_file")
    ),
    uiOutput("logoutbtn")
  ),
  sidebar = dashboardSidebar(
    id = "sidebar",
    collapsed = TRUE,
    minified = FALSE,
    uiOutput("sidebarpanel")
  ),
  body = dashboardBody(
    shinyjs::useShinyjs(), 
    uiOutput("body"),
    tags$head(tags$style("#geojson_dbclick_analysis .modal-dialog {width: 90%;}"))
  )#,
  #controlbar = dashboardControlbar(
  #  skin = "dark",
  #  uiOutput("controlbarpanel")
  #)
)

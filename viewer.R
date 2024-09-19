library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyFiles)
library(shinyjqui)
library(htmlwidgets)
library(rhandsontable)
library(DT)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(leafem)
library(leafpm)
library(polyclip)
library(lawn)
library(stringr)
library(dplyr)
library(raster)
library(EBImage)
library(reticulate)
library(keras)
library(tensorflow)
#library(tfaddons)
library(RNDPI)
library(future)
library(future.apply)
library(future.callr)
library(promises)
library(microbenchmark)
library(XML)
library(ggplot2)
library(cowplot)
library(penalized)
library(DBI)
library(magrittr)
library(sf)
library(sp)
library(sf)
library(parallel)
library(data.table)
library(dplyr)
library(future)
library(future.callr)
library(callr)
library(glue)
library(jpeg)
library(utf8)
library(promises)
library(dqrng)
library(bigmemory)
library(RhpcBLASctl)
library(sessioninfo)
library(shinyjs)

memory.limit(1000000)

plan(list(multisession, sequential))
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#Sys.setenv("PATH" = paste0("compile_program_callback\\openslide-win64-20171122\\bin;", Sys.getenv("PATH")))

IO <- import("io")
BASE64 <- import("base64")
tf <- import("tensorflow")
PIL <- import("PIL")
np <- import("numpy")
os <- import("os")
torch <- import("torch")
numba<- import("numba")
sys <- import("sys", convert = TRUE)
os$add_dll_directory('D:/File_promgraming_inside/leaflet_image/compile_program_callback/openslide-win64-20171122/bin')
openslide <- import("openslide")


options(shiny.maxRequestSize=100*1024^3)
options(future.globals.maxSize= 10*1024^3)
#K <- backend()
#===========preload data==============================================================================================================
#preload login page
source("program_script/Pre_load_login_page.R", local = TRUE)

#preload function, lonlat to xy or xy to lonlat or NDPI_data or generate file
function_all <- list.files("program_script/Pre_load_function/", pattern = ".R",full.names = TRUE)
for (source_file in function_all) {
  source(source_file)
}

function_all <- list.files("program_script/Pre_load_function_cpp/", pattern = ".cpp",full.names = TRUE)
for (source_file in function_all) {
  Rcpp::sourceCpp(source_file)
}
#py_function
#source_python("program_script/Pre_load_function_cpp/py_function.py")
#preload point polygon data and history + point color data
source("program_script/Pre_load_point_polygon_file_function_and_point_color_panel.R", local = TRUE)

#preload UI data
source("program_script/Pre_load_UI.R", local = TRUE)

#preload image and slide function
source("program_script/Pre_load_imag_slide_list_function.R", local = TRUE)
#===========preload data==============================================================================================================

server <- function(input, output, session) {

  #annotation data and history, load and change
  source("program_script/Server_side_point_polygon_data_during_add_and_change.R", local = TRUE)

  #login check and function setting
  source("program_script/Server_side_login_check_and_function_setting.R", local = TRUE)

  #body setting after login
  source("program_script/Server_side_body_setting.R", local = TRUE, encoding = "UTF-8")

  #siderbar setting after login
  source("program_script/Server_side_siderbar_setting.R", local = TRUE, encoding = "UTF-8")

  #controlbar setting after login
  #source("program_script/Server_side_controlbar_setting.R", local = TRUE)
  #source("program_script/Server_side_viewer_annotation_run_AI_UI.R", local = TRUE)

  #leaflet image and slide viewer
  source("program_script/Server_side_leaflet_slide_image_viewer.R", local = TRUE)

  #viewer and annotation slide/image selection setting
  source("program_script/Server_side_viewer_slide_image_selection_setting.R", local = TRUE)

  #annotation function and setting
  source("program_script/Server_side_annotation_setting_and_function.R", local = TRUE)
  source("program_script/Server_side_annotation_setting_and_function_auto_hotspot_detection.R", local = TRUE)
  source("program_script/Server_side_annotation_setting_and_function_change_point_and_polygon_color_order.R", local = TRUE)
  source("program_script/Server_side_annotation_setting_and_function_change_heat_color_order.R", local = TRUE)
  source("program_script/Server_side_annotation_setting_and_function_point_click_event.R", local = TRUE)
  source("program_script/Server_side_annotation_setting_and_function_switch_point_or_polygon_class.R", local = TRUE)
  source("program_script/Server_side_annotation_setting_and_function_switch_heat_class.R", local = TRUE)
  source("program_script/Server_side_annotation_setting_and_function_save_point_and_polygon.R", local = TRUE)
  source("program_script/Server_side_annotation_setting_and_function_redo_and_undo.R", local = TRUE)
  source("program_script/Server_side_annotation_setting_and_function_add_edit_delete_polygon.R", local = TRUE)
  source("program_script/Server_side_annotation_setting_and_function_turn_on_off_free_draw.R", local = TRUE)
  source("program_script/Server_side_annotation_setting_and_function_fix_area_draw_function.R", local = TRUE)

  #accelerator
  source("program_script/Accelerator.R", local = TRUE)
  #samplify_polygon
  source("program_script/samplify_polygon.R", local = TRUE)
  #samplify_polygon
  source("program_script/Polygon_Function.R", local = TRUE)
  #NDPI_transform.R
  source("program_script/NDPI_transform.R", local = TRUE)




  #upload file function and setting
  source("program_script/Server_side_upload_file_function_and_setting.R", local = TRUE)

  #generate training file and setting
  source("program_script/Server_side_generate_training_file_function_and_setting.R", local = TRUE)

  #preview slide image
  source("program_script/Server_side_leaflet_slide_viewer_preview_image.R", local = TRUE)

  #authorization setting and function
  source("program_script/Server_side_authorization_setting_and_function.R", local = TRUE)

  #create, delete and rename slide/image/model folder
  source("program_script/Server_side_folder_management.R", local = TRUE)

  #AI pre label, function and setting
  source("program_script/Server_side_AI_pre_label_in_annotation.R", local = TRUE)
  source("program_script/Server_side_AI_pre_label_load_and_save_condition.R", local = TRUE)

  #slide tiling fucntion and parallel
  source("program_script/Server_side_tiling_function_and_parallel.R", local = TRUE)

  #slide tiling fucntion and parallel
  source("program_script/Server_side_slide_fast_processing.R", local = TRUE)

  #save/download screen shot/print screen image
  source("program_script/Server_side_save_screen_shot_image.R", local = TRUE)

  #measuring length and clean leangth ### click event are in annotation file
  source("program_script/Server_side_measuring_length_and_clean_length.R", local = TRUE)

  #autherization for slide and image
  source("program_script/Server_side_authorization_for_slide_and_image.R", local = TRUE)

  #additional pdf information in viewer and annotation
  source("program_script/Server_side_additional_pdf_information.R", local = TRUE)

  #microbenchmark
  source("program_script/Server_side_microbenchmark.R", local = TRUE)

  #Run AI setting
  source("program_script/Server_side_Run_AI_setting.R", local = TRUE)

  #slide generating training file with calculation of tissue and slide size
  source("program_script/Server_side_slide_tissue_fraction_calculation.R", local = TRUE)

  #importing 3rd party annotation.R
  source("program_script/Server_side_importing_3rd_party_annotation.R", local = TRUE)

  #SQLite.R
  #source("program_script/SQLite.R", local = TRUE)
  #SServer_side_file_management.R
  source("program_script/Server_side_file_management.R", local = TRUE)
  ###############################################  put in run AI
  Run_AI_model <- reactiveValues(model = "",
                                 model_tag = "",
                                 model_option = list(),
                                 model_result = list())
  ###############################################
  #####################Specific AI run
  #Server_side_AI_pre_label_in_annotation_Run_AI.R
  source("program_script/Server_side_AI_pre_label_in_annotation_Run_AI.R", local = TRUE)

  #specific AI setting
  source("program_script/Specific_model_run_script/Switch_model_defult_setting.R", local = TRUE)
  #HER2-CEP FISH
  source("program_script/Specific_model_run_script/FISH_HER2_Server_setting.R", local = TRUE)
  #1p19q FISH
  source("program_script/Specific_model_run_script/FISH_1p_19q_Server_setting.R", local = TRUE)
  #CDK2/MDM4 FISH
  source("program_script/Specific_model_run_script/FISH_CDK2_MDM4_Server_setting.R", local = TRUE)

  #Test_Run_File.R
  source("program_script/Specific_model_run_script/Test_Run_File.R", local = TRUE)


 # observeEvent(input$Slide_or_image11, {
 #    print(input$Slide_or_image11)
#  })
}
#shinyParallel::runApp(app)
shinyApp(ui, server)

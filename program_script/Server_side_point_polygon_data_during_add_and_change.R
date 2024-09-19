Points_data_current_on_map <- reactiveValues()
Polygons_data_current_on_map <- reactiveValues()
Heats_data_current_on_map <- reactiveValues()
Tags_data_current_on_map <- reactiveValues()

Points_note_on_map <- reactiveValues()
Polygons_note_on_map <- reactiveValues()
Tags_note_on_map <- reactiveValues()
Free_draw_polygon_current_on_map <- reactiveValues()
Heats_data_current_on_map <- reactiveValues()
Polygons_history_current_on_map <- reactiveValues()



#observe({
observeEvent(input$Image_input, {
	
  if ((is.null(input$Annotation_group) | is.null(input$Image_input)) == FALSE) {
    
    Points_data_current_on_map$df <- setDT(point_file_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/points.rds")))	
	    
  }
  
})
#observe({
observeEvent(input$Image_input, {

  if ((is.null(input$Annotation_group) | is.null(input$Image_input)) == FALSE) {
    Heats_data_current_on_map$list <- heat_file_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/heats.rds"))

  }
  
})
#observe({
observeEvent(input$Image_input, {
  if ((is.null(input$Annotation_group) | is.null(input$Image_input)) == FALSE) {
    Tags_data_current_on_map$list <- tag_file_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/tags.rds"))
  }
   
})
#observe({
observeEvent(input$Image_input, {

  if ((is.null(input$Annotation_group) | is.null(input$Image_input)) == FALSE) {
    Polygons_data_current_on_map$list <- polygon_file_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/polygons.rds"))
	
    isolate({
	
      #current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
      
      #===================================================== ver1 into ver2
      current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
      
      #print(current_polygon_class)
      #===================================================== ver1 into ver2
      #foreach(batch_number =1:batch_size,.combine = rbind) %dopar% {
	  
	 # lapply(modified_polygon_list, function(x){
      
      modified_polygon_list <- Polygons_data_current_on_map$list[[current_polygon_class]]
      if (length(modified_polygon_list) > 0) {
        Polygons_data_current_on_map$bbox <-lapply(modified_polygon_list, function(x){
          lawn_bbox(x)
        })
      } else {
        Polygons_data_current_on_map$bbox <- list()
      }
      
    })
  }

})
observe({
  if ((is.null(input$Annotation_group) | is.null(input$Image_input)) == FALSE) {
    Polygons_history_current_on_map$list <- polygon_history_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/polygons_history.rds"))
  }
})
observe({
  if ((is.null(input$Annotation_group) | is.null(input$Image_input)) == FALSE) {
    Points_note_on_map$df <- point_note_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/points_note.rds"))
  }
})
observe({
  if ((is.null(input$Annotation_group) | is.null(input$Image_input)) == FALSE) {
    Polygons_note_on_map$df <- polygon_note_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/polygons_note.rds"))
  }
})
observe({
  if ((is.null(input$Annotation_group) | is.null(input$Image_input)) == FALSE) {
    Tags_note_on_map$df <- tag_note_function(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/tags_note.rds"))
  }
})
observe({
  if ((is.null(input$Annotation_group) | is.null(input$Image_input)) == FALSE) {
    Free_draw_polygon_current_on_map$leaflet_geojson <- readRDS("store_data_for_program/blank_leaflet_geojson.rds")
  }
})
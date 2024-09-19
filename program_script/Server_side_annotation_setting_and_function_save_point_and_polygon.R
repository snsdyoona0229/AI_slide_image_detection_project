observeEvent(input$Save_points_data, {
  confirmSweetAlert(
    session = session,
    inputId = "Check_save_points",
    title = "Save points data and re-write previous data?",
    btn_labels =  c("Cancel", "Save anyway!!"),
    btn_colors = c("#FE642E", "#04B404")
  )
})

observeEvent(input$Check_save_points, {

  if (input$Check_save_points) {
    saveRDS(Points_data_current_on_map$df, paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/points.rds"))
    if(is.null(input$Points_note_table) == FALSE){
      saveRDS(as.data.frame(hot_to_r(input$Points_note_table)), paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/points_note.rds"))
    }
  }
})

observeEvent(input$Save_polygons_data, {
  confirmSweetAlert(
    session = session,
    inputId = "Check_save_polygons",
    title = "Save polygons data and re-write previous data?",
    btn_labels =  c("Cancel", "Save anyway!!"),
    btn_colors = c("#FE642E", "#04B404")
  )
})

observeEvent(input$Check_save_polygons, {
  if (input$Check_save_polygons) {
    saveRDS(Polygons_data_current_on_map$list, paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/polygons.rds"))
    saveRDS(Polygons_history_current_on_map$list, paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/polygons_history.rds"))
    if(is.null(input$Polygons_note_table) == FALSE){
      saveRDS(as.data.frame(hot_to_r(input$Polygons_note_table)), paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/polygons_note.rds"))
    }
  }
})
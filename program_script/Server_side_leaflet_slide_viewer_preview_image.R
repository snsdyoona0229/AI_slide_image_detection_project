output$slide_image_preview <- renderUI({
  src_img <- paste0("Slide/", input$Annotation_group, "/", input$Image_input, "/info/SlideImage.jpg")
  tags$img(src = src_img, height = "100%", width = "100%")
})
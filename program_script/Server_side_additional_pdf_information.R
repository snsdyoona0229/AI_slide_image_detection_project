output$pdfviewer <- renderText({
  paste('<iframe style="height:600px; width:100%" src="', paste0(input$Slide_or_image,
                                                                 "/",
                                                                 input$Annotation_group,
                                                                 "/",
                                                                 input$Image_input,
                                                                 "/",
                                                                 input$Select_pdf_file_to_see), '"></iframe>', sep = "")
})

observeEvent(input$Open_additional_pdf_file, {
  if (input$Select_pdf_file_to_see != "") {
    showModal(modalDialog(
      title = "Additional information",
      size = "l",
      htmlOutput('pdfviewer')
    ))
  }
})
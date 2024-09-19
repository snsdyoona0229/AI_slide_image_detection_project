observeEvent(input$fix_area_size_selection, {
  if (input$fix_area_size_selection == "other value") {
  } else {
    updateTextInput(session = session, 
                    inputId = "fix_area_size_ready_to_draw",
                    value = input$fix_area_size_selection)
  }
})

observeEvent(input$fix_area_size_ready_to_draw, {
  if ((is.null(input$fix_area_size_ready_to_draw) + is.null(input$fix_area_size_selection)) == 0) {
    if (input$fix_area_size_ready_to_draw != input$fix_area_size_selection) {
      updateRadioGroupButtons(session = session, 
                              inputId = "fix_area_size_selection", 
                              selected = "other value")
    }
  }
})
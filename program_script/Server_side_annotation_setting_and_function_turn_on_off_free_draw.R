observeEvent(input$Point_free_draw_on_off, {
  updateSwitchInput(session = session,
                    inputId = "Polygon_free_draw_on_off",
                    value = input$Point_free_draw_on_off,
  )
  if (input$Point_free_draw_on_off) {
    updateSwitchInput(session = session,
                      inputId = "fix_area_draw_on_off",
                      value = FALSE
    )
  }
})

observeEvent(input$Polygon_free_draw_on_off, {
  updateSwitchInput(session = session,
                    inputId = "Point_free_draw_on_off",
                    value = input$Polygon_free_draw_on_off,
  )
  if (input$Polygon_free_draw_on_off) {
    updateSwitchInput(session = session,
                      inputId = "fix_area_draw_on_off",
                      value = FALSE
    )
  }
})

observeEvent(input$fix_area_draw_on_off, {
  if (input$fix_area_draw_on_off) {
    updateSwitchInput(session = session,
                      inputId = "Polygon_free_draw_on_off",
                      value = FALSE
    )
    updateSwitchInput(session = session,
                      inputId = "Point_free_draw_on_off",
                      value = FALSE
    )
    updateRadioGroupButtons(session = session,
                            inputId = "Start_to_draw_point",
                            selected = "None"
    )
  }
})
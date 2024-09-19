observeEvent(input$start_measuring_length, {
  Measuring_condition$line_M <- 1
  leafletProxy("image_viewer", session) %>% 
    clearGroup("measure_temp_dot")
})

Measuring_condition <- reactiveValues(line_M = 0,
                                      point_1 = c(0,0),
                                      point_2 = c(0,0))

observeEvent(input$clean_all_measuring_length, {
  Measuring_condition$line_M <- 0
  Measuring_condition$point_1 <- c(0,0)
  Measuring_condition$point_2 <- c(0,0)
  leafletProxy("image_viewer", session) %>% 
    clearGroup("measure_temp_dot") %>% 
    clearGroup("measure_line")
})
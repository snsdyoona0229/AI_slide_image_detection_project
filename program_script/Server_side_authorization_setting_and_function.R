output$Authorization_code_table <- renderRHandsontable({
  rhandsontable(Authorization_table$df)
})

observeEvent(input$save_new_authorization, {
  new_df <- as.data.frame(hot_to_r(input$Authorization_code_table))
  new_df[which(is.na(new_df), arr.ind = TRUE)] <- FALSE
  write.csv(new_df, "account_and_login_data/login_file.csv", row.names = FALSE)
  Authorization_table$df <- read.csv("account_and_login_data/login_file.csv", stringsAsFactors = FALSE)
  sendSweetAlert(
    session = session,
    title = "Saved!!",
    text = "The authorization have been changed!",
    type = "success"
  )
})

Authorization_table <- reactiveValues(df = read.csv("account_and_login_data/login_file.csv", stringsAsFactors = FALSE))
observeEvent(input$Create_new_folder_start, {
  confirmSweetAlert(
    session = session,
    inputId = "Check_create_new_folder",
    title = "Are you sure to create this folder?",
    btn_labels =  c("Cancel", "Create anyway!!"),
    btn_colors = c("#FE642E", "#04B404")
  )
})

observeEvent(input$Check_create_new_folder, {
  if (input$Check_create_new_folder) {
    if (input$Create_new_folder == "") {
      sendSweetAlert(
        session = session,
        title = "No folder name is detected!",
        type = "error"
      )
    } else {
      if (input$folder_management_type == "Slide group") {
        create_name <- input$Create_new_folder %>% str_replace_all("[[:punct:]]", "_")
        if (create_name %in% list.files("www/Slide")) {
          sendSweetAlert(
            session = session,
            title = "Folder already exist! Please change name!",
            type = "error"
          )
        } else {
          dir.create(paste0("www/Slide/", create_name))
          sendSweetAlert(
            session = session,
            title = "Folder is created successfully!",
            type = "success"
          )
        }
      } else if (input$folder_management_type == "Image group") {
        create_name <- input$Create_new_folder %>% str_replace_all("[[:punct:]]", "_")
        if (create_name %in% list.files("www/Image")) {
          sendSweetAlert(
            session = session,
            title = "Folder already exist! Please change name!",
            type = "error"
          )
        } else {
          dir.create(paste0("www/Image/", create_name))
          sendSweetAlert(
            session = session,
            title = "Folder is created successfully!",
            type = "success"
          )
        }
      } else if (input$folder_management_type == "Model group") {
        create_name <- input$Create_new_folder %>% str_replace_all("[[:punct:]]", "_")
        if (create_name %in% list.files("model")) {
          sendSweetAlert(
            session = session,
            title = "Folder already exist! Please change name!",
            type = "error"
          )
        } else {
          dir.create(paste0("model/", create_name))
          sendSweetAlert(
            session = session,
            title = "Folder is created successfully!",
            type = "success"
          )
        }
      }
    }
    
  }
})


observeEvent(input$Delete_folder_start, {
  confirmSweetAlert(
    session = session,
    inputId = "Check_delete_folder",
    title = "Are you sure to delete this folder?",
    btn_labels =  c("Cancel", "Delete anyway!!"),
    btn_colors = c("#FE642E", "#04B404")
  )
})

observeEvent(input$Check_delete_folder, {
  if (input$Check_delete_folder) {
    if (input$folder_management_type == "Slide group") {
      delete_name <- paste0("www/Slide/", input$Delete_folder)
      unlink(delete_name, recursive = TRUE)
      updateSelectInput(session, inputId = "Delete_folder", choices = list.files("www/Slide"))
      sendSweetAlert(
        session = session,
        title = "Folder is deleted successfully!",
        type = "success"
      )
    } else if (input$folder_management_type == "Image group") {
      delete_name <- paste0("www/Image/", input$Delete_folder)
      unlink(delete_name, recursive = TRUE)
      updateSelectInput(session, inputId = "Delete_folder", choices = list.files("www/Image"))
      sendSweetAlert(
        session = session,
        title = "Folder is deleted successfully!",
        type = "success"
      )
    } else if (input$folder_management_type == "Model group") {
      delete_name <- paste0("model/", input$Delete_folder)
      unlink(delete_name, recursive = TRUE)
      updateSelectInput(session, inputId = "Delete_folder", choices = list.files("model"))
      sendSweetAlert(
        session = session,
        title = "Folder is deleted successfully!",
        type = "success"
      )
    }
  }
})



observeEvent(input$Rename_folder_start, {
  confirmSweetAlert(
    session = session,
    inputId = "Check_rename_folder",
    title = "Are you sure to rename this folder?",
    btn_labels =  c("Cancel", "Rename anyway!!"),
    btn_colors = c("#FE642E", "#04B404")
  )
})


observeEvent(input$Check_rename_folder, {
  if (input$Check_rename_folder) {
    if (input$Rename_folder_text == "") {
      sendSweetAlert(
        session = session,
        title = "No folder name is detected!",
        type = "error"
      )
    } else {
      if (input$folder_management_type == "Slide group") {
        rename_name <- input$Rename_folder_text %>% str_replace_all("[[:punct:]]", "_")
        if (rename_name %in% list.files("www/Slide")) {
          sendSweetAlert(
            session = session,
            title = "Folder already exist! Please change name!",
            type = "error"
          )
        } else {
          file.rename(paste0("www/Slide/", input$Rename_folder), paste0("www/Slide/", rename_name))
          updateSelectInput(session, inputId = "Rename_folder", choices = list.files("www/Slide"))
          sendSweetAlert(
            session = session,
            title = "Folder is renamed successfully!",
            type = "success"
          )
        }
      } else if (input$folder_management_type == "Image group") {
        rename_name <- input$Rename_folder_text %>% str_replace_all("[[:punct:]]", "_")
        if (rename_name %in% list.files("www/Image")) {
          sendSweetAlert(
            session = session,
            title = "Folder already exist! Please change name!",
            type = "error"
          )
        } else {
          file.rename(paste0("www/Image/", input$Rename_folder), paste0("www/Image/", rename_name))
          updateSelectInput(session, inputId = "Rename_folder", choices = list.files("www/Image"))
          sendSweetAlert(
            session = session,
            title = "Folder is renamed successfully!",
            type = "success"
          )
        }
      } else if (input$folder_management_type == "Model group") {
        rename_name <- input$Rename_folder_text %>% str_replace_all("[[:punct:]]", "_")
        if (rename_name %in% list.files("model")) {
          sendSweetAlert(
            session = session,
            title = "Folder already exist! Please change name!",
            type = "error"
          )
        } else {
          file.rename(paste0("model/", input$Rename_folder), paste0("model/", rename_name))
          updateSelectInput(session, inputId = "Rename_folder", choices = list.files("model"))
          sendSweetAlert(
            session = session,
            title = "Folder is renamed successfully!",
            type = "success"
          )
        }
      }
    }
  }
})

output$folder_managent_UI <- renderUI({
  if (input$folder_management_option == "Create") {
    tagList(
      textInput(inputId = "Create_new_folder", label = "Name of new folder"),
      br(),
      br(),
      actionBttn(
        inputId = "Create_new_folder_start",
        label = "Create!", 
        style = "material-flat",
        color = "primary"
      )
    )
  } else if (input$folder_management_option == "Delete") {
    if (input$folder_management_type == "Slide group") {
      tagList(
        selectInput(inputId = "Delete_folder",
                    label = "Choose folder group to delet",
                    choices = list.files("www/Slide")
        ),
        br(),
        br(),
        actionBttn(
          inputId = "Delete_folder_start",
          label = "Delete!", 
          style = "material-flat",
          color = "danger"
        )
      )
    } else if (input$folder_management_type == "Image group") {
      tagList(
        selectInput(inputId = "Delete_folder",
                    label = "Choose folder group to delet",
                    choices = list.files("www/Image")
        ),
        br(),
        br(),
        actionBttn(
          inputId = "Delete_folder_start",
          label = "Delete!", 
          style = "material-flat",
          color = "danger"
        )
      )
    } else if (input$folder_management_type == "Model group") {
      tagList(
        selectInput(inputId = "Delete_folder",
                    label = "Choose folder group to delet",
                    choices = list.files("model")
        ),
        br(),
        br(),
        actionBttn(
          inputId = "Delete_folder_start",
          label = "Delete!", 
          style = "material-flat",
          color = "danger"
        )
      )
    }
  } else if (input$folder_management_option == "Rename") {
    if (input$folder_management_type == "Slide group") {
      tagList(
        selectInput(inputId = "Rename_folder",
                    label = "Choose folder group to rename",
                    choices = list.files("www/Slide")
        ),
        textInput(inputId = "Rename_folder_text", label = "Name of new folder"),
        br(),
        br(),
        actionBttn(
          inputId = "Rename_folder_start",
          label = "Rename!", 
          style = "material-flat",
          color = "warning"
        )
      )
    } else if (input$folder_management_type == "Image group") {
      tagList(
        selectInput(inputId = "Rename_folder",
                    label = "Choose folder group to rename",
                    choices = list.files("www/Image")
        ),
        textInput(inputId = "Rename_folder_text", label = "Name of new folder"),
        br(),
        br(),
        actionBttn(
          inputId = "Rename_folder_start",
          label = "Rename!", 
          style = "material-flat",
          color = "warning"
        )
      )
    } else if (input$folder_management_type == "Model group") {
      tagList(
        selectInput(inputId = "Rename_folder",
                    label = "Choose folder group to rename",
                    choices = list.files("model")
        ),
        textInput(inputId = "Rename_folder_text", label = "Name of new folder"),
        br(),
        br(),
        actionBttn(
          inputId = "Rename_folder_start",
          label = "Rename!", 
          style = "material-flat",
          color = "warning"
        )
      )
    }
  }
})

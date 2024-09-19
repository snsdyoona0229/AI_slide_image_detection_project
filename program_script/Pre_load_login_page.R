loginpage <- div(id = "loginpage", style = "width: 1600px; max-width: 100%; margin: 0 auto; padding: 20px;",
                   column(3,
                          img(src='Picture/Taiwan_pathology.png', height = "100%", width = "100%")
                   ),
                   column(6,
                          tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                          textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username"), width = "100%"),
                          passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password"), width = "100%"),
                          br(),
                          div(
                            style = "text-align: center;",
                            actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                            shinyjs::hidden(
                              div(id = "nomatch",
                                  tags$p("Incorrect username or password!",
                                         style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                         class = "text-center"))),
                            br()
                          )
                   ),
                   column(3,
                          img(src='Picture/IAP.png', height = "100%", width = "100%")
                   )
                 
)
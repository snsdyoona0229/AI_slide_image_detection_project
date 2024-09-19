require(shiny)
folder_address = "E:/Desktop/leaflet_image/viewer.R"

#x <- system("ifconfig", intern=TRUE)
#z <- x[grep("IPv4", x)]
#ip <- gsub(".*? ([[:digit:]])", "\\1", z)
#print(paste0("the Shiny Web application runs on: http://", "192.168.1.29", ":80/"))

#runApp(folder_address, launch.browser=FALSE, port = 80, host = "192.168.1.29")

print(paste0("the Shiny Web application runs on: http://", "192.168.68.85", ":39/"))

runApp(folder_address, launch.browser=FALSE, port = 80, host = "192.168.68.39")



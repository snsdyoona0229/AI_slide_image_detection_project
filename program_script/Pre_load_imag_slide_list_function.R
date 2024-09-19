load_list_function <- function(user_name){
  if (file.exists(paste0("account_and_login_data/Slide_Image_list/", user_name, ".rds"))) {
    load_list <- readRDS(paste0("account_and_login_data/Slide_Image_list/", user_name, ".rds"))
  } else {
    load_list <- list("Slide" = list(),
                      "Image" = list())
    saveRDS(load_list, paste0("account_and_login_data/Slide_Image_list/", user_name, ".rds"))
  }
  return(load_list)
}
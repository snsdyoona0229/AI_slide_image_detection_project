polygon_correction_function <- function(polygon_list, method = "one") {
  polygon_number <- length(polygon_list)
  for (j in 1:polygon_number) {
    #j <- 3
    if (class(polygon_list[[j]]$geometry$coordinates) == "list" & (polygon_list[[j]]$geometry$coordinates %>% length()) == 1) {
      polygon_length <- length(polygon_list[[j]]$geometry$coordinates[[1]])
      if (polygon_list[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygon_list[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
        polygon_list[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygon_list[[j]]$geometry$coordinates[[1]][[1]]
      }
      if ((polygon_list[[j]] %>% lawn_kinks())$features %>% length() != 0) {
        all_poly <- polygon_list[[j]] %>% lawn_coordall()
        clip_poly <- polysimplify(list(list(x = all_poly[,1], y = all_poly[,2])))
        pointer_to_max <- list(polygon_number = 0, point_number = 0)
        for (l in 1:length(clip_poly)) {
          if (clip_poly[[l]]$x %>% length() > pointer_to_max$point_number) {
            pointer_to_max$polygon_number <- l
            pointer_to_max$point_number <- clip_poly[[l]]$x %>% length()
          }
        }
        clip_poly <- clip_poly[[pointer_to_max$polygon_number]] %>% abind::abind(along = 2) %>% unname()
        choose_poly <- lapply(1:nrow(clip_poly), function(x){
          list(clip_poly[x,1], clip_poly[x,2])
        })
        polygon_list[[j]]$geometry$coordinates <- list(choose_poly)
      }
      polygon_length <- length(polygon_list[[j]]$geometry$coordinates[[1]])
      if (polygon_list[[j]]$geometry$coordinates[[1]][[1]] %>% paste0(collapse = "_") != polygon_list[[j]]$geometry$coordinates[[1]][[polygon_length]] %>% paste0(collapse = "_")) {
        polygon_list[[j]]$geometry$coordinates[[1]][[polygon_length + 1]] <- polygon_list[[j]]$geometry$coordinates[[1]][[1]]
      }
    } else if (class(polygon_list[[j]]$geometry$coordinates) == "array") {
      polygon_length <- nrow(polygon_list[[j]]$geometry$coordinates[1,,])
      if (polygon_list[[j]]$geometry$coordinates[1,1,] %>% paste0(collapse = "_") != polygon_list[[j]]$geometry$coordinates[1,polygon_length,] %>% paste0(collapse = "_")) {
        polygon_list[[j]]$geometry$coordinates <- abind(polygon_list[[j]]$geometry$coordinates, 
                                                        polygon_list[[j]]$geometry$coordinates[1,1,] %>% array(c(1,1,2)),
                                                        along = 2) %>% unname()
      }
      if ((polygon_list[[j]] %>% lawn_kinks())$features %>% length() != 0) {
        all_poly <- polygon_list[[j]] %>% lawn_coordall()
        clip_poly <- polysimplify(list(list(x = all_poly[,1], y = all_poly[,2])))
        pointer_to_max <- list(polygon_number = 0, point_number = 0)
        for (l in 1:length(clip_poly)) {
          if (clip_poly[[l]]$x %>% length() > pointer_to_max$point_number) {
            pointer_to_max$polygon_number <- l
            pointer_to_max$point_number <- clip_poly[[l]]$x %>% length()
          }
        }
        clip_poly <- clip_poly[[pointer_to_max$polygon_number]] %>% abind::abind(along = 2) %>% unname()
        clip_poly <- abind::abind(clip_poly, clip_poly[1,] %>% array(c(1,2)), along = 1) %>% array(c(1,nrow(clip_poly) + 1, 2)) %>% unname()
        polygon_list[[j]]$geometry$coordinates <- clip_poly
      }
    } else if (class(polygon_list[[j]]$geometry$coordinates) == "list" & (polygon_list[[j]]$geometry$coordinates %>% length()) > 1) {
      for (loop_check in 1:length(polygon_list[[j]]$geometry$coordinates)) {
        polygon_length <- nrow(polygon_list[[j]]$geometry$coordinates[[loop_check]])
        if (polygon_list[[j]]$geometry$coordinates[[loop_check]][1,] %>% paste0(collapse = "_") != polygon_list[[j]]$geometry$coordinates[[loop_check]][polygon_length,] %>% paste0(collapse = "_")) {
          polygon_list[[j]]$geometry$coordinates[[loop_check]] <- abind(polygon_list[[j]]$geometry$coordinates[[loop_check]], 
                                                                        polygon_list[[j]]$geometry$coordinates[[loop_check]][1,] %>% array(c(1,2)),
                                                                        along = 1) %>% unname()
        }
      }
      if ((polygon_list[[j]] %>% lawn_kinks())$features %>% length() != 0) {
        for (loop_check in 1:length(polygon_list[[j]]$geometry$coordinates)) {
          all_poly <- polygon_list[[j]]$geometry$coordinates[[loop_check]]
          clip_poly <- polysimplify(list(list(x = all_poly[,1], y = all_poly[,2])))
          pointer_to_max <- list(polygon_number = 0, point_number = 0)
          for (l in 1:length(clip_poly)) {
            if (clip_poly[[l]]$x %>% length() > pointer_to_max$point_number) {
              pointer_to_max$polygon_number <- l
              pointer_to_max$point_number <- clip_poly[[l]]$x %>% length()
            }
          }
          clip_poly <- clip_poly[[pointer_to_max$polygon_number]] %>% abind::abind(along = 2) %>% unname()
          clip_poly <- abind::abind(clip_poly, clip_poly[1,] %>% array(c(1,2)), along = 1) %>% array(c(nrow(clip_poly) + 1, 2)) %>% unname()
          polygon_list[[j]]$geometry$coordinates[[loop_check]] <- clip_poly
        }
      }
    }
  }
  return(polygon_list)
}
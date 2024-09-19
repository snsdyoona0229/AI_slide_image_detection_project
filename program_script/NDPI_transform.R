np <- import("numpy")


NDPI_center_RGB_batch_Num_function <- function(opened_slide,image_name,image_file_root,all_x_pixel,all_y_pixel,all_pyramid_layer,tilesize,overlap_pixel,select_level,slide_info_file) {      	
		slide_Level_info <- slide_info_file$source_lens
		slide_Level_all <- c("40x", "20x","10x","5x","2.5x","1.25x","0.625x","0.3125x","0.15625x","0.078125x")
		tilesize <- tilesize + overlap_pixel*2

		slide_level <- which(slide_Level_all==slide_Level_info)
        train_level <- which(slide_Level_all==select_level)
	    level <- train_level - slide_level

		if(level >= 0 ){

		   all_x_pixel <- all_x_pixel  - (tilesize*(2^(level-2+slide_level)))
		   all_y_pixel <- all_y_pixel  - (tilesize*(2^(level-2+slide_level)))
           
		   #--level_dimensions--#
		   level_dimensions <- (opened_slide$level_dimensions[[1]][[1]] + opened_slide$level_dimensions[[1]][[2]]) / (opened_slide$level_dimensions[[2]][[1]] + opened_slide$level_dimensions[[2]][[2]])
		   #--level_dimensions--#
		   if(round(level_dimensions) == 2){
		   
		   #level_normal <- level + (slide_level-1)
		   level_normal <- level + slide_level-1
		   
		   region <- opened_slide$read_region(as.integer(c(all_x_pixel,all_y_pixel)),as.integer(level_normal),as.integer(c(tilesize,tilesize)))$transpose(as.integer(5))$convert('RGB')
		   
           }
           if(round(level_dimensions) == 4){
		   tilesize_new <- tilesize * 2
		   level_unnormal <- (level + (slide_level-1))%/%2
		   if(train_level%%2==0){
		     region_new <- opened_slide$read_region(as.integer(c(all_x_pixel,all_y_pixel)),as.integer(level_unnormal),as.integer(c(tilesize_new,tilesize_new)))$transpose(as.integer(5))$convert('RGB')
			}else{
			 region_new <- opened_slide$read_region(as.integer(c(all_x_pixel,all_y_pixel)),as.integer(level_unnormal),as.integer(c(tilesize,tilesize)))$transpose(as.integer(5))$convert('RGB')
			}
		   region <- region_new$resize(c(tilesize,tilesize) %>% as.integer())

           }		   		   
		   return(np$reshape(region,c(1,tilesize,tilesize,3)%>% as.integer())/255.0)
		}
		if(level < 0 ){

		   level_diffrent <- train_level - slide_level
		   level <- 0

		   all_x_pixel <- all_x_pixel - (tilesize*(2^(level_diffrent-2+slide_level)))#1
		   all_y_pixel <- all_y_pixel - (tilesize*(2^(level_diffrent-2+slide_level)))#1
		   
		   region <- opened_slide$read_region(as.integer(c(all_x_pixel,all_y_pixel)),as.integer(level),as.integer(c(tilesize,tilesize)))$transpose(as.integer(5))$convert('RGB')
           
           return(np$reshape(region,c(1,tilesize,tilesize,3)%>% as.integer())/255.0)	   
		
		}
}

#numba$jit(target_backend='cuda')


NDPI_center_RGB_batch_Num_function_Background<- function(opened_slide,image_name,image_file_root,all_x_pixel,all_y_pixel,all_pyramid_layer,tilesize,slide_info_file) {		
		 initial_value_level <- (slide_info_file$Width_pixel/4096) + (slide_info_file$Height_pixel/4096)
         tile_generator <- openslide$deepzoom$DeepZoomGenerator(osr = opened_slide,
                                                               tile_size = as.integer(2048), 
                                                               overlap = as.integer(0))											   
	     tile_table <- tile_generator$level_dimensions
         min_pixel_token <- 5
		 min_pixel <- abs(initial_value_level - tile_table[[1]][[1]] - tile_table[[1]][[2]])
		 
		 for(tile_level in 1:length(tile_table)){
		   if(min_pixel > abs(initial_value_level - tile_table[[tile_level]][[1]] - tile_table[[tile_level]][[2]])){
		     min_pixel <- abs(initial_value_level - tile_table[[tile_level]][[1]] - tile_table[[tile_level]][[2]])
			 min_pixel_token <- min_pixel_token + 1
		   }
		 }

        tile_img <- tile_generator$get_tile(abs(as.integer(min_pixel_token)),as.integer(c(0, 0)))
        tile_img <- tile_img$transpose(as.integer(5))
        blank_image <- PIL$Image$new("RGB", size = as.integer(c(2048,2048)),color= "white")
        blank_image$paste(im = tile_img)
			
        return(np$reshape(blank_image,c(1,2048,2048,3)%>% as.integer())/255.0)
}


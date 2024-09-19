samplify_polygon <- function(clip_polygon) {

		clip_polygon_samplify <- NULL
		self_intersecting <- c()
		count_self_intersecting <- 1
		count_size <- c()
		
		if(length(clip_polygon) >10){
		
		   for(i in 1:length(clip_polygon)){ 
		   
		     count_size[i] <- length(clip_polygon[[i]]$x)
	
           }
  
		  weight <- 7 + (length(clip_polygon[[i]]$x)/100)
		  #prnit(count_size)
		   
		   for(i in 1:length(clip_polygon)){ 
		   
	          if(length(clip_polygon[[i]]$x) < weight){
              self_intersecting[count_self_intersecting] <- i
		      count_self_intersecting <- count_self_intersecting  + 1
			  }
		   }
		   
		   if(length(self_intersecting) > 0){
	
		   clip_polygon_samplify <- clip_polygon[-self_intersecting]		
		   
           }
          return(clip_polygon_samplify)
		}else{
		
		  return(clip_polygon)
		
		}
}
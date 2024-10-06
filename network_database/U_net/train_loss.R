b_run_tensor <- tf$constant(value = 0, 
                            shape = list(1 %>% as.integer(), 
                                         input_size %>% as.integer(), 
                                         input_size %>% as.integer(), 
                                         layer_output_number %>% as.integer()), 
                            dtype = "float32")

max_data <- b_run_tensor %>% 
  tf$shape() %>% 
  tf$slice(begin = tf$constant(array(data = 3), 
                               dtype = "int32"), 
           size = tf$constant(array(data = 1), 
                              dtype = "int32"))

plus_data <- tf$constant(array(data = 1), 
                         dtype = "int32")

#check_tensor <- tf$concat(list(b_run_tensor %>% tf$shape() %>% tf$slice(begin = tf$constant(array(data = 0), dtype = "int32"), size = tf$constant(array(data = 3), dtype = "int32")),
#                               tf$ones(shape = 1, dtype = "int32")), axis = as.integer(0))

pre_begin_tensor <- tf$zeros(shape = list(3 %>% as.integer()), 
                             dtype = "int32")

outer_use <- list()
outer_cent_use <- list()
outer_size <- list()
outer_basal_weight <- list()
outer_dot <- list()
outer_bias <- list()
inner_use <- list()
inner_cent_use <- list()
inner_size <- list()
inner_basal_weight <- list()
inner_dot <- list()
inner_bias <- list()

for (i in 1:layer_output_number) {
  if (info_option$Outer_weight_train[i] == "N") {
    outer_use[[i]] <- 0
    outer_cent_use[[i]] <- 0
    outer_size[[i]] <- 10
    outer_basal_weight[[i]] <- info_option$Outer_non_labeled_object_basal_weight[i]
    outer_dot[[i]] <- 1
    outer_bias[[i]] <- 0
  } else if (info_option$Outer_weight_train[i] == "C") {
    outer_use[[i]] <- 1
    outer_cent_use[[i]] <- 1
    outer_size[[i]] <- info_option$Outer_central_size[i]
    outer_basal_weight[[i]] <- info_option$Outer_non_labeled_object_basal_weight[i]
    outer_dot[[i]] <- info_option$Outer_central_dot[i]
    outer_bias[[i]] <- info_option$Outer_central_bias[i]
  } else if (info_option$Outer_weight_train[i] == "B") {
    outer_use[[i]] <- 1
    outer_cent_use[[i]] <- 0
    outer_size[[i]] <- info_option$Outer_boundary_size[i]
    outer_basal_weight[[i]] <- info_option$Outer_non_labeled_object_basal_weight[i]
    outer_dot[[i]] <- info_option$Outer_boundary_dot[i]
    outer_bias[[i]] <- info_option$Outer_boundary_bias[i]
  }
  
  if (info_option$Inner_weight_train[i] == "N") {
    inner_use[[i]] <- 0
    inner_cent_use[[i]] <- 0
    inner_size[[i]] <- 10
    inner_basal_weight[[i]] <- info_option$Inner_labeled_object_basal_weight[i]
    inner_dot[[i]] <- 1
    inner_bias[[i]] <- 0
  } else if (info_option$Inner_weight_train[i] == "C") {
    inner_use[[i]] <- 1
    inner_cent_use[[i]] <- 1
    inner_size[[i]] <- info_option$Inner_central_size[i]
    inner_basal_weight[[i]] <- info_option$Inner_labeled_object_basal_weight[i]
    inner_dot[[i]] <- info_option$Inner_central_dot[i]
    inner_bias[[i]] <- info_option$Inner_central_bias[i]
  } else if (info_option$Inner_weight_train[i] == "B") {
    inner_use[[i]] <- 1
    inner_cent_use[[i]] <- 0
    inner_size[[i]] <- info_option$Inner_boundary_size[i]
    inner_basal_weight[[i]] <- info_option$Inner_labeled_object_basal_weight[i]
    inner_dot[[i]] <- info_option$Inner_boundary_dot[i]
    inner_bias[[i]] <- info_option$Inner_boundary_bias[i]
  }
}

outer_use <- tf$cast(outer_use, dtype = "float32")
outer_cent_use <- tf$cast(outer_cent_use, dtype = "float32")
outer_size <- tf$cast(outer_size, dtype = "int32")
outer_basal_weight <- tf$cast(outer_basal_weight, dtype = "float32")
outer_dot <- tf$cast(outer_dot, dtype = "float32")
outer_bias <- tf$cast(outer_bias, dtype = "float32")

inner_use <- tf$cast(inner_use, dtype = "float32")
inner_cent_use <- tf$cast(inner_cent_use, dtype = "float32")
inner_size <- tf$cast(inner_size, dtype = "int32")
inner_basal_weight <- tf$cast(inner_basal_weight, dtype = "float32")
inner_dot <- tf$cast(inner_dot, dtype = "float32")
inner_bias <- tf$cast(inner_bias, dtype = "float32")


i <- tf$constant(array(data = 0), dtype = "int32")
c <- function(i, run_tensor, outer_dot, outer_bias, inner_dot, inner_bias, outer_use, outer_cent_use, outer_size, outer_basal_weight, inner_use, inner_cent_use, inner_size, inner_basal_weight) {tf$less(i, max_data)}
b <- function(i, run_tensor, outer_dot, outer_bias, inner_dot, inner_bias, outer_use, outer_cent_use, outer_size, outer_basal_weight, inner_use, inner_cent_use, inner_size, inner_basal_weight) {
  check_tensor <- tf$concat(list(run_tensor %>% 
                                   tf$shape() %>% 
                                   tf$slice(begin = tf$constant(array(data = 0), 
                                                                dtype = "int32"), 
                                            size = tf$constant(array(data = 3), 
                                                               dtype = "int32")),
                                 tf$ones(shape = 1 %>% as.integer(), 
                                         dtype = "int32")), 
                            axis = as.integer(0))
  
  begin_value <- tf$concat(list(pre_begin_tensor, i), axis = as.integer(0))
  
  slide_1 <- tf$cast(list(1), dtype = "int32")
  
  begin_i <- i %>% 
    tf$reshape(shape = slide_1)
  
  inner_array <- tf$slice(run_tensor, 
                          begin = begin_value, 
                          size = check_tensor)
  
  outter_array <- 1 - inner_array
  
  outter_kernel_value <- tf$concat(list(tf$slice(outer_size, 
                                                 begin = begin_i, 
                                                 size = slide_1),
                                        tf$slice(outer_size, 
                                                 begin = begin_i, 
                                                 size = slide_1),
                                        tf$ones(shape = list(2 %>% as.integer()), 
                                                dtype = "int32")), 
                                   axis = as.integer(0))
  
  inner_kernel_value <- tf$concat(list(tf$slice(inner_size, 
                                                begin = begin_i, 
                                                size = slide_1),
                                       tf$slice(inner_size, 
                                                begin = begin_i, 
                                                size = slide_1),
                                       tf$ones(shape = list(2 %>% as.integer()), 
                                               dtype = "int32")), 
                                  axis = as.integer(0))
  
  outer_center <- tf$sqrt(k_conv2d(outter_array, 
                                   kernel = tf$ones(shape = outter_kernel_value, 
                                                    dtype = "float32"), 
                                   padding = "same", 
                                   strides = 1, 
                                   dilation_rate = 1)) * tf$slice(outer_dot, 
                                                                  begin = begin_i, 
                                                                  size = slide_1) + tf$slice(outer_bias, 
                                                                                             begin = begin_i, 
                                                                                             size = slide_1)
  
  outer_center <- outer_center * outter_array * tf$slice(outer_cent_use, 
                                                         begin = begin_i, 
                                                         size = slide_1) * tf$slice(outer_use, 
                                                                                    begin = begin_i, 
                                                                                    size = slide_1) 
  
  outer_border <- tf$sqrt(k_conv2d(inner_array, 
                                   kernel = tf$ones(shape = outter_kernel_value, 
                                                    dtype = "float32"), 
                                   padding = "same", 
                                   strides = 1, 
                                   dilation_rate = 1)) * tf$slice(outer_dot, 
                                                                  begin = begin_i, 
                                                                  size = slide_1)
  
  outer_border <- outer_border + outer_border %>% tf$clip_by_value(clip_value_min = 0, 
                                                                   clip_value_max = 1) * tf$slice(outer_bias, 
                                                                                                  begin = begin_i, 
                                                                                                  size = slide_1)
  
  outer_border <- outer_border * outter_array * tf$slice(1 - outer_cent_use, 
                                                         begin = begin_i, 
                                                         size = slide_1) * tf$slice(outer_use, 
                                                                                    begin = begin_i, 
                                                                                    size = slide_1)
  
  inner_center <- tf$sqrt(k_conv2d(inner_array, 
                                   kernel = tf$ones(shape = inner_kernel_value, 
                                                    dtype = "float32"), 
                                   padding = "same", 
                                   strides = 1, 
                                   dilation_rate = 1)) * tf$slice(inner_dot, 
                                                                  begin = begin_i, 
                                                                  size = slide_1) + tf$slice(inner_bias, 
                                                                                             begin = begin_i, 
                                                                                             size = slide_1)
  
  inner_center <- inner_center * inner_array * tf$slice(inner_cent_use, 
                                                        begin = begin_i, 
                                                        size = slide_1) * tf$slice(inner_use, 
                                                                                   begin = begin_i, 
                                                                                   size = slide_1)
  
  inner_border <- tf$sqrt(k_conv2d(outter_array, 
                                   kernel = tf$ones(shape = inner_kernel_value, 
                                                    dtype = "float32"), 
                                   padding = "same", 
                                   strides = 1, 
                                   dilation_rate = 1)) * tf$slice(inner_dot, 
                                                                  begin = begin_i, 
                                                                  size = slide_1)
  
  inner_border <- inner_border + inner_border %>% tf$clip_by_value(clip_value_min = 0, 
                                                                   clip_value_max = 1) * tf$slice(inner_bias, 
                                                                                                  begin = begin_i, 
                                                                                                  size = slide_1)
  
  inner_border <- inner_border * inner_array * tf$slice(1 - inner_cent_use, 
                                                        begin = begin_i, 
                                                        size = slide_1) * tf$slice(inner_use, 
                                                                                   begin = begin_i, 
                                                                                   size = slide_1)
  
  label_weight <- tf$slice(inner_basal_weight, 
                           begin = begin_i, 
                           size = slide_1)
  
  non_label_weight <- tf$slice(outer_basal_weight, 
                               begin = begin_i, 
                               size = slide_1)
  
  replace_array <- outer_center + outer_border + inner_center + inner_border + inner_array * label_weight + outter_array * non_label_weight
  
  replace_array <- tf$transpose(replace_array, 
                                tf$cast(list(3,0,1,2), 
                                        dtype = "int32"))
  
  transform_run_tensor <- tf$transpose(run_tensor, 
                                       tf$cast(list(3,0,1,2), 
                                               dtype = "int32"))
  
  
  transform_run_tensor <- tf$tensor_scatter_nd_update(transform_run_tensor, 
                                                      tf$reshape(i, 
                                                                 shape = tf$cast(list(1,1), 
                                                                                 dtype = "int32")), 
                                                      replace_array)
  
  run_tensor <- tf$transpose(transform_run_tensor, 
                             tf$cast(list(1,2,3,0), 
                                     dtype = "int32"))
  
  list(tf$add(i, plus_data), run_tensor, outer_dot, outer_bias, inner_dot, inner_bias, outer_use, outer_cent_use, outer_size, outer_basal_weight, inner_use, inner_cent_use, inner_size, inner_basal_weight)
}



custom_loss <- function(y_true, y_pred) {
  y_true_pos <- k_flatten(y_true)
  y_pred_pos <- k_flatten(y_pred)
  
  weight <- tf$while_loop(c, b, list(i, y_true, outer_dot, outer_bias, inner_dot, inner_bias, outer_use, outer_cent_use, outer_size, outer_basal_weight, inner_use, inner_cent_use, inner_size, inner_basal_weight))[[2]]
  weight <- tf$where(tf$math$is_nan(weight), tf$ones(tf$shape(weight)), weight)
  weight <- k_flatten(weight)
  
  intersection <- k_sum(y_true_pos * y_pred_pos * weight)
  
  dice_coef <- (2 * intersection + 1)/(k_sum(y_true_pos * y_true_pos * weight) + k_sum(y_pred_pos * y_pred_pos * weight) + 1)
  
  result <- 1 - dice_coef
  return(result)
}
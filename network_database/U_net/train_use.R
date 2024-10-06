get_unet <- function(input_shape = list(NULL, NULL, 3),
                     num_classes) {
  
  
  inputs <- layer_input(shape = input_shape)
  
  
  pre_RS1L1 <- inputs %>% 
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_RS1L1 <- pre_RS1L1 %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  RS1L1 <- layer_add(list(pre_RS1L1, post_RS1L1))
  
  
  
  RS1L1d <- RS1L1 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  pre_RS1L2 <- RS1L1d %>% 
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_RS1L2 <- pre_RS1L2 %>%
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  RS1L2 <- layer_add(list(pre_RS1L2, post_RS1L2))
  
  
  
  
  RS1L2d <- RS1L2 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  pre_RS1L3 <- RS1L2d %>% 
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_RS1L3 <- pre_RS1L3 %>%
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  RS1L3 <- layer_add(list(pre_RS1L3, post_RS1L3))
  
  
  
  
  RS1L3d <- RS1L3 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  pre_RS1L4 <- RS1L3d %>% 
    layer_conv_2d(filters = 128, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_RS1L4 <- pre_RS1L4 %>%
    layer_conv_2d(filters = 128, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 128, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  RS1L4 <- layer_add(list(pre_RS1L4, post_RS1L4))
  
  
  RS1L4d <- RS1L4 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  pre_RS1L5 <- RS1L4d %>% 
    layer_conv_2d(filters = 256, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_RS1L5 <- pre_RS1L5 %>%
    layer_conv_2d(filters = 256, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 256, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  RS1L5 <- layer_add(list(pre_RS1L5, post_RS1L5))
  
  
  RS1L5d <- RS1L5 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  pre_RS1L6 <- RS1L5d %>% 
    layer_conv_2d(filters = 512, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_RS1L6 <- pre_RS1L6 %>%
    layer_conv_2d(filters = 512, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 512, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  RS1L6 <- layer_add(list(pre_RS1L6, post_RS1L6))
  
  
  RS1L6d <- RS1L6 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  pre_S1c <- RS1L6d %>% 
    layer_conv_2d(filters = 1024, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  post_S1c <- pre_S1c %>%
    layer_conv_2d(filters = 1024, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 1024, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  S1c <- layer_add(list(pre_S1c, post_S1c))
  
  
  
  
  L6_dim <-tf$shape(RS1L6) %>% tf$slice(list(as.integer(1)), list(as.integer(2)))
  L5_dim <-tf$shape(RS1L5) %>% tf$slice(list(as.integer(1)), list(as.integer(2)))
  L4_dim <-tf$shape(RS1L4) %>% tf$slice(list(as.integer(1)), list(as.integer(2)))
  L3_dim <-tf$shape(RS1L3) %>% tf$slice(list(as.integer(1)), list(as.integer(2)))
  L2_dim <-tf$shape(RS1L2) %>% tf$slice(list(as.integer(1)), list(as.integer(2)))
  L1_dim <-tf$shape(RS1L1) %>% tf$slice(list(as.integer(1)), list(as.integer(2)))
  
  
  
  
  RS1L2u <- RS1L2 %>%
    layer_conv_2d_transpose(filters = 16, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L1_dim)
  
  pre_RS2L1 <- RS1L2u %>% 
    {layer_concatenate(inputs = list(RS1L1, .), axis = 3)} %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_RS2L1 <- layer_add(list(RS1L2u, pre_RS2L1)) %>% 
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  RS2L1 <- layer_add(list(RS1L1, pre_RS2L1, post_RS2L1))
  
  
  RS2L1d <- RS2L1 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  
  
  
  
  RS1L3u <- RS1L3 %>%
    layer_conv_2d_transpose(filters = 32, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L2_dim)
  
  pre_RS2L2 <- RS1L3u %>% 
    {layer_concatenate(inputs = list(RS2L1d, RS1L2, .), axis = 3)} %>%
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_RS2L2 <- layer_add(list(RS1L3u, pre_RS2L2)) %>% 
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  RS2L2 <- layer_add(list(RS1L2, pre_RS2L2, post_RS2L2))
  
  
  RS2L2d <- RS2L2 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  RS2L2u <- RS2L2 %>%
    layer_conv_2d_transpose(filters = 16, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L1_dim)
  
  pre_RS3L1 <- RS2L2u %>% 
    {layer_concatenate(inputs = list(RS2L1, .), axis = 3)} %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_RS3L1 <- layer_add(list(RS2L2u, pre_RS3L1)) %>% 
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  RS3L1 <- layer_add(list(RS2L1, pre_RS3L1, post_RS3L1))
  
  
  RS3L1d <- RS3L1 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  RS1L4u <- RS1L4 %>%
    layer_conv_2d_transpose(filters = 64, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L3_dim)
  
  pre_RS2L3 <- RS1L4u %>% 
    {layer_concatenate(inputs = list(RS2L2d, RS1L3, .), axis = 3)} %>%
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_RS2L3 <- layer_add(list(RS1L4u, pre_RS2L3)) %>% 
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  RS2L3 <- layer_add(list(RS1L3, pre_RS2L3, post_RS2L3))
  
  
  RS2L3d <- RS2L3 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  
  RS2L3u <- RS2L3 %>%
    layer_conv_2d_transpose(filters = 32, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L2_dim)
  
  pre_RS3L2 <- RS2L3u %>% 
    {layer_concatenate(inputs = list(RS3L1d, RS2L2, .), axis = 3)} %>%
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_RS3L2 <- layer_add(list(RS2L3u, pre_RS3L2)) %>% 
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  RS3L2 <- layer_add(list(RS2L2, pre_RS3L2, post_RS3L2))
  
  
  RS3L2d <- RS3L2 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  RS1L5u <- RS1L5 %>%
    layer_conv_2d_transpose(filters = 128, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L4_dim)
  
  pre_RS2L4 <- RS1L5u %>% 
    {layer_concatenate(inputs = list(RS2L3d, RS1L4, .), axis = 3)} %>%
    layer_conv_2d(filters = 128, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_RS2L4 <- layer_add(list(RS1L5u, pre_RS2L4)) %>% 
    layer_conv_2d(filters = 128, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 128, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  RS2L4 <- layer_add(list(RS1L4, pre_RS2L4, post_RS2L4))
  
  
  RS2L4d <- RS2L4 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  
  
  RS3L2u <- RS3L2 %>%
    layer_conv_2d_transpose(filters = 16, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L1_dim)
  
  pre_S4c <- RS3L2u %>% 
    {layer_concatenate(inputs = list(RS3L1, .), axis = 3)} %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_S4c <- layer_add(list(RS3L2u, pre_S4c)) %>% 
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  S4c <- layer_add(list(RS3L1, pre_S4c, post_S4c))
  
  
  S4cd <- S4c %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  
  
  
  
  RS2L4u <- RS2L4 %>%
    layer_conv_2d_transpose(filters = 64, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L3_dim)
  
  pre_S3c <- RS2L4u %>% 
    {layer_concatenate(inputs = list(RS3L2d, RS2L3, .), axis = 3)} %>%
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_S3c <- layer_add(list(RS2L4u, pre_S3c)) %>% 
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  S3c <- layer_add(list(RS2L3, pre_S3c, post_S3c))
  
  
  S3cd <- S3c %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  
  
  
  
  
  
  RS1L6u <- RS1L6 %>%
    layer_conv_2d_transpose(filters = 256, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L5_dim)
  
  pre_S2c <- RS1L6u %>% 
    {layer_concatenate(inputs = list(RS2L4d, RS1L5, .), axis = 3)} %>%
    layer_conv_2d(filters = 256, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_S2c <- layer_add(list(RS1L6u, pre_S2c)) %>% 
    layer_conv_2d(filters = 256, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 256, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  S2c <- layer_add(list(RS1L5, pre_S2c, post_S2c))
  
  
  S2cd <- S2c %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  
  
  
  
  S3cu <- S3c %>%
    layer_conv_2d_transpose(filters = 32, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L2_dim)
  
  pre_LS3L2 <- S3cu %>% 
    {layer_concatenate(inputs = list(S4cd, RS3L2, .), axis = 3)} %>%
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_LS3L2 <- layer_add(list(S3cu, pre_LS3L2)) %>% 
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  LS3L2 <- layer_add(list(RS3L2, pre_LS3L2, post_LS3L2))
  
  
  LS3L2d <- LS3L2 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  
  
  
  
  
  
  S2cu <- S2c %>%
    layer_conv_2d_transpose(filters = 128, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L4_dim)
  
  pre_LS2L4 <- S2cu %>% 
    {layer_concatenate(inputs = list(S3cd, RS2L4, .), axis = 3)} %>%
    layer_conv_2d(filters = 128, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_LS2L4 <- layer_add(list(S2cu, pre_LS2L4)) %>% 
    layer_conv_2d(filters = 128, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 128, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  LS2L4 <- layer_add(list(RS2L4, pre_LS2L4, post_LS2L4))
  
  
  LS2L4d <- LS2L4 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  
  
  
  LS3L2u <- LS3L2 %>%
    layer_conv_2d_transpose(filters = 16, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L1_dim)
  
  pre_LS3L1 <- LS3L2u %>% 
    {layer_concatenate(inputs = list(S4c, .), axis = 3)} %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_LS3L1 <- layer_add(list(LS3L2u, pre_LS3L1)) %>% 
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  LS3L1 <- layer_add(list(S4c, pre_LS3L1, post_LS3L1))
  
  
  LS3L1d <- LS3L1 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  
  
  
  LS2L4u <- LS2L4 %>%
    layer_conv_2d_transpose(filters = 64, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L3_dim)
  
  pre_LS2L3 <- LS2L4u %>% 
    {layer_concatenate(inputs = list(LS3L2d, S3c, .), axis = 3)} %>%
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_LS2L3 <- layer_add(list(LS2L4u, pre_LS2L3)) %>% 
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  LS2L3 <- layer_add(list(S3c, pre_LS2L3, post_LS2L3))
  
  
  LS2L3d <- LS2L3 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  
  S1cu <- S1c %>%
    layer_conv_2d_transpose(filters = 512, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L6_dim)
  
  pre_LS1L6 <- S1cu %>% 
    {layer_concatenate(inputs = list(S2cd, RS1L6, .), axis = 3)} %>%
    layer_conv_2d(filters = 512, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_LS1L6 <- layer_add(list(S1cu, pre_LS1L6)) %>% 
    layer_conv_2d(filters = 512, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 512, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  LS1L6 <- layer_add(list(RS1L6, pre_LS1L6, post_LS1L6))
  
  
  
  
  
  
  
  
  LS1L6u <- LS1L6 %>%
    layer_conv_2d_transpose(filters = 256, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L5_dim)
  
  pre_LS1L5 <- LS1L6u %>% 
    {layer_concatenate(inputs = list(LS2L4d, S2c, .), axis = 3)} %>%
    layer_conv_2d(filters = 256, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_LS1L5 <- layer_add(list(LS1L6u, pre_LS1L5)) %>% 
    layer_conv_2d(filters = 256, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 256, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  LS1L5 <- layer_add(list(S2c, pre_LS1L5, post_LS1L5))
  
  
  
  
  
  
  
  
  
  LS2L3u <- LS2L3 %>%
    layer_conv_2d_transpose(filters = 32, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L2_dim)
  
  pre_LS2L2 <- LS2L3u %>% 
    {layer_concatenate(inputs = list(LS3L1d, LS3L2, .), axis = 3)} %>%
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_LS2L2 <- layer_add(list(LS2L3u, pre_LS2L2)) %>% 
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  LS2L2 <- layer_add(list(LS3L2, pre_LS2L2, post_LS2L2))
  
  
  LS2L2d <- LS2L2 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  
  
  
  LS1L5u <- LS1L5 %>%
    layer_conv_2d_transpose(filters = 128, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L4_dim)
  
  pre_LS1L4 <- LS1L5u %>% 
    {layer_concatenate(inputs = list(LS2L3d, LS2L4, .), axis = 3)} %>%
    layer_conv_2d(filters = 128, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_LS1L4 <- layer_add(list(LS1L5u, pre_LS1L4)) %>% 
    layer_conv_2d(filters = 128, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 128, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  LS1L4 <- layer_add(list(LS2L4, pre_LS1L4, post_LS1L4))
  
  
  
  
  
  
  LS2L2u <- LS2L2 %>%
    layer_conv_2d_transpose(filters = 16, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L1_dim)
  
  pre_LS2L1 <- LS2L2u %>% 
    {layer_concatenate(inputs = list(LS3L1, .), axis = 3)} %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_LS2L1 <- layer_add(list(LS2L2u, pre_LS2L1)) %>% 
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  LS2L1 <- layer_add(list(LS3L1, pre_LS2L1, post_LS2L1))
  
  
  LS2L1d <- LS2L1 %>%
    layer_max_pooling_2d(pool_size = list(2, 2), strides = list(2, 2))
  
  
  
  
  
  
  
  LS1L4u <- LS1L4 %>%
    layer_conv_2d_transpose(filters = 64, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L3_dim)
  
  pre_LS1L3 <- LS1L4u %>% 
    {layer_concatenate(inputs = list(LS2L2d, LS2L3, .), axis = 3)} %>%
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_LS1L3 <- layer_add(list(LS1L4u, pre_LS1L3)) %>% 
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 64, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  LS1L3 <- layer_add(list(LS2L3, pre_LS1L3, post_LS1L3))
  
  
  
  
  
  
  
  
  LS1L3u <- LS1L3 %>%
    layer_conv_2d_transpose(filters = 32, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L2_dim)
  
  pre_LS1L2 <- LS1L3u %>% 
    {layer_concatenate(inputs = list(LS2L1d, LS2L2, .), axis = 3)} %>%
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_LS1L2 <- layer_add(list(LS1L3u, pre_LS1L2)) %>% 
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 32, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  LS1L2 <- layer_add(list(LS2L2, pre_LS1L2, post_LS1L2))
  
  
  
  
  
  
  
  
  LS1L2u <- LS1L2 %>%
    layer_conv_2d_transpose(filters = 16, kernel_size = 3, strides = 2, padding = "same", activation = "relu") %>% 
    tf$image$resize(size = L1_dim)
  
  pre_LS1L1 <- LS1L2u %>% 
    {layer_concatenate(inputs = list(LS2L1, .), axis = 3)} %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  post_LS1L1 <- layer_add(list(LS1L2u, pre_LS1L1)) %>% 
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu") %>%
    layer_conv_2d(filters = 16, kernel_size = list(3, 3), padding = "same", activation = "relu")
  
  
  LS1L1 <- layer_add(list(LS2L1, pre_LS1L1, post_LS1L1))
  
  
  
  
  
  
  classify <- layer_conv_2d(LS1L1,
                            filters = num_classes, 
                            kernel_size = list(1, 1),
                            activation = "sigmoid")
  
  
  model <- keras_model(
    inputs = inputs,
    outputs = classify
  )
  
  return(model)
}


tf <- import("tensorflow")
accelerate <- import("accelerate")
torch <- import("torch")
gc <- import("gc")
os <- import("os")
numba<- import("numba")
#numba$jit(target='cuda')
accelerator <- function(){
  accelerator <- accelerate$Accelerator()
  accelerator$free_memory()
}
setting_evn <- function(){
  old_opts <- tf$config$optimizer$get_experimental_options()
  tf$config$optimizer$set_experimental_options(old_opts)
  physical_devices <- tf$config$list_physical_devices('GPU')
  tf$config$experimental$set_memory_growth(physical_devices[[1]],TRUE)
}

mem_release <- function(){
  torch$cuda$empty_cache()
  gc$collect()
}

sess_close <- function(){
  sess <- tf$compat$v1$Session()
  sess$close()
}

clear_gpu <- function(){
  tf$keras$backend$clear_session()
}

gpu_use <- function(){

gpu_options <- tf$compat$v1$GPUOptions(allow_growth=TRUE)
sess <- tf$compat$v1$Session(config=tf$compat$v1$ConfigProto(gpu_options=gpu_options))
tf$compat$v1$keras$backend$set_session(sess)

}

gpu_adjust <- function(){

gpu_options <- tf$compat$v1$GPUOptions(per_process_gpu_memory_fraction=as.integer(0.1))
sess <- tf$compat$v1$Session(config=tf$compat$v1$ConfigProto(gpu_options=gpu_options))
tf$compat$v1$keras$backend$set_session(sess)

}

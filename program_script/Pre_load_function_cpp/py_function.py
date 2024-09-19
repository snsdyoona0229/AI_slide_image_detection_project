import tensorflow as tf
from tensorflow import keras
from keras import backend as K
from keras.backend import set_session
from accelerate import Accelerator
from keras.backend import clear_session
from keras.backend import get_session
from numba import cuda
import torch
import gc
import os

#import os, signal

def accelerator():
  accelerator = Accelerator()
  accelerator.free_memory()
  
def mem_release():
  gc.collect()
  torch.cuda.empty_cache()
  torch.cuda.ipc_collect()
  gc.collect()

  
def sess_close():
  sess = tf.compat.v1.Session()
  sess.close()
	
def setting_evn():
  old_opts = tf.config.optimizer.get_experimental_options()
  tf.config.optimizer.set_experimental_options(old_opts)
  gpus = tf.config.experimental.list_physical_devices('GPU')
  tf.config.experimental.set_memory_growth(gpus[0], True)
  torch.cuda.stream('cuda:0')
  torch.cuda.synchronize()
	
def clear_gpu():
  cfg = K.config_pb2.ConfigProto()
  cfg.gpu_options.allow_growth = True
  K.set_session(K.tf.compat.v1.Session(config=cfg))
  tf.compat.v1.reset_default_graph()
  tf.keras.backend.clear_session()
  torch.cuda.empty_cache()
	
def clean_gc():
  
  for name in dir():
    if not name.startswith('_'):
        del globals()[name]
        
  globals().clear()
  gc.collect()
  
def cuda_close_tf():
  cuda.close()
  
def cuda_reset_tf():
  device = cuda.get_current_device()
  device.reset()
 
def gpu_set():
#  cfg = K.tf.ConfigProto()
  cfg = tf.compat.v1.ConfigProto()
  cfg.gpu_options.allow_growth = True
  K.set_session(tf.compat.v1.Session(config=cfg))
#  K.set_session(K.tf.Session(config=cfg))

def use_percent(p):
  config = tf.compat.v1.ConfigProto() 
  config.gpu_options.allow_growth = True
  config.gpu_options.per_process_gpu_memory_fraction = p
#  config = tf.compat.v1.ConfigProto(gpu_options.per_process_gpu_memory_fraction=0.333)
  config.gpu_options.allow_growth = True
  session = tf.compat.v1.Session(config=config)
  K.set_session(tf.compat.v1.Session(config=config))
  

  
  
#  session = tf.compat.v1.Session(config=config)
  #session.run(tf.compat.v1.global_variables_initializer())
  
  #sess = tf.Session(config=TF_CONFIG)
  
  
  #config = tf.compat.v1.ConfigProto()
  #config.gpu_options.per_process_gpu_memory_fraction = 50
  #session = tf.compat.v1.Session(config=config)
  
  #session.run(tf.compat.v1.global_variables_initializer())
  
  #config.gpu_options.allow_growth = True
  #K.set_session(tf.compat.v1.Session(config=config))
  
#gpu_options = tf.GPUOptions(per_process_gpu_memory_fraction=0.333)
#sess = tf.Session(config=tf.ConfigProto(gpu_options=gpu_options))







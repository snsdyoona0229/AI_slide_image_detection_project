reticulate::py_run_string("
import numpy as np
import matplotlib.pyplot as plt
import openslide
from openslide.deepzoom import DeepZoomGenerator
from PIL import Image
import keras
import tensorflow as tf
import numba as nb
import torch
from PIL import Image
import cupy as cp

def openslide_img(image_root,all_x_pixel,all_y_pixel,level_normal,tilesize):
  opened_slide = openslide.OpenSlide(image_root)
  region = opened_slide.read_region((int(all_x_pixel),int(all_y_pixel)),int(level_normal),(int(tilesize),int(tilesize))).transpose(5)
  region = region.convert('RGB')
  img_np = np.array(region, dtype=np.float32)
  tensor = torch.FloatTensor(img_np)/255.0
  img = tensor.numpy()
  
  return img
  
def tile_generator(image_name,min_pixel_token):
   opened_slide = openslide.OpenSlide(image_name)
   tile_generator = openslide.deepzoom.DeepZoomGenerator(osr = opened_slide,
                                                         tile_size = 2048, 
                                                         overlap = 0)	
   tile_img = tile_generator.get_tile(int(abs(min_pixel_token+5)),(0,0))
   tile_img = tile_img.transpose(5)
   blank_image = Image.new('RGB', size =(2048,2048),color= (255, 255, 255))
   blank_image.paste(im = tile_img)
   img_np = np.array(blank_image, dtype=np.float32)
   tensor = torch.FloatTensor(img_np)/255.0
   img = tensor.numpy()

   return img
   
def openslide_img_cp(image_root,all_x_pixel,all_y_pixel,level_normal,tilesize):
  opened_slide = openslide.OpenSlide(image_root)
  region = opened_slide.read_region((int(all_x_pixel),int(all_y_pixel)),int(level_normal),(int(tilesize),int(tilesize))).transpose(5)
  region = region.convert('RGB')
  img_np = np.array(region, dtype=np.float32)
  tensor = cp.asarray(img_np)/255.0
  img = cp.asnumpy(tensor)
  
  return img
  
def tile_generator_cp(image_name,min_pixel_token):
   opened_slide = openslide.OpenSlide(image_name)
   tile_generator = openslide.deepzoom.DeepZoomGenerator(osr = opened_slide,
                                                         tile_size = 2048, 
                                                         overlap = 0)	
   tile_img = tile_generator.get_tile(int(abs(min_pixel_token+5)),(0,0))
   tile_img = tile_img.transpose(5)
   blank_image = Image.new('RGB', size =(2048,2048),color= (255, 255, 255))
   blank_image.paste(im = tile_img)
   img_np = np.array(blank_image, dtype=np.float32)
   tensor = cp.asarray(img_np)/255.0
   img = cp.asnumpy(tensor)

   return img

")




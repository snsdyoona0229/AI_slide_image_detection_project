reticulate::py_run_string("

from shapely.geometry import Polygon,LineString,LinearRing
from shapely.geometry.multipolygon import MultiPolygon
from shapely.geometry import shape, JOIN_STYLE
import shutil
import os

import math
import pandas as pd
import os


def list_num(lon_original,lat_original,lon_new,lat_new):
  
  x_original = lon_original
  y_original = lat_original
  x_new = lon_new
  y_new = lat_new
  
  coordinate_original = list()
  coordinate_new = list()
  
  for i in range(0,len(x_original)):

      tem_original = (y_original[i],x_original[i])
      coordinate_original.append(tem_original)
           
  for y in range(0,len(lon_new)):
      
      tem_new = (y_new[y],x_new[y])
      coordinate_new.append(tem_new)
      
  p = Polygon(coordinate_original)
  p =p.buffer(0.01, 1, join_style=JOIN_STYLE.mitre).buffer(-0.01, 1, join_style=JOIN_STYLE.mitre)
  #p =p.buffer(0.01, 2, join_style=JOIN_STYLE.mitre).buffer(-0.01, 2, join_style=JOIN_STYLE.round)
  q = Polygon(coordinate_new)
  #inter = p.intersection(q)
  inter = p.union(q)
  
  coordinate = list(inter.exterior.coords)
  
  #inter = p.intersection(q)
      
  return coordinate

def Dilated(lon_original,lat_original):

  x_original = lon_original
  y_original = lat_original
  
  coordinate_original = list()
  for i in range(0,len(x_original)):

      tem_original = (y_original[i],x_original[i])
      coordinate_original.append(tem_original)
      
  p = Polygon(coordinate_original)
  p = p.buffer(0.01, 1, join_style=JOIN_STYLE.mitre).buffer(-0.01, 1, join_style=JOIN_STYLE.mitre)
  
  p = list(p.exterior.coords)
  
  return p
 
def interior_coords(lon_original,lat_original,lon_new,lat_new):

  x_original = lon_original
  y_original = lat_original
  x_new = lon_new
  y_new = lat_new
  
  coordinate_original = list()
  coordinate_new = list()
  
  for i in range(0,len(x_original)):

      tem_original = (y_original[i],x_original[i])
      coordinate_original.append(tem_original)
           
  for y in range(0,len(lon_new)):
      
      tem_new = (y_new[y],x_new[y])
      coordinate_new.append(tem_new)
      
  p = Polygon(coordinate_original)
  #p =p.buffer(0.01, 1, join_style=JOIN_STYLE.mitre).buffer(-0.01, 1, join_style=JOIN_STYLE.mitre)
  p = p.buffer(0.01, 1, join_style=JOIN_STYLE.mitre).buffer(-0.01, 1, join_style=JOIN_STYLE.mitre)
  q = Polygon(coordinate_new)
  inter = p.union(q)

  
  coordinate = list(inter.interiors)
  polygon_hole = []
  
  for x in range(0,len(coordinate)):
  
      polygon_annotation = list(coordinate[x].coords[:])
      polygon_hole.append(polygon_annotation)

#  interior_coords = []
#  for interior in inter.interiors:
#      interior_coords += interior.coords[:]    
  return polygon_hole

def intersection(lon_original,lat_original,lon_new,lat_new):
  
  x_original = lon_original
  y_original = lat_original
  x_new = lon_new
  y_new = lat_new
  
  coordinate_original = list()
  coordinate_new = list()
  
  for i in range(0,len(x_original)):

      tem_original = (y_original[i],x_original[i])
      coordinate_original.append(tem_original)
           
  for y in range(0,len(lon_new)):
      
      tem_new = (y_new[y],x_new[y])
      coordinate_new.append(tem_new)
      
  p = Polygon(coordinate_original)
  p =p.buffer(0.01, 1, join_style=JOIN_STYLE.mitre).buffer(-0.01, 1, join_style=JOIN_STYLE.mitre)
  #p =p.buffer(0.01, 2, join_style=JOIN_STYLE.mitre).buffer(-0.01, 2, join_style=JOIN_STYLE.round)
  q = Polygon(coordinate_new)
  #inter = p.intersection(q)
  inter = p.intersects(q)

  return inter

#----#

def difference(lon_original,lat_original,lon_new,lat_new):
  
  x_original = lon_original
  y_original = lat_original
  x_new = lon_new
  y_new = lat_new
  
  coordinate_original = list()
  coordinate_new = list()
  
  for i in range(0,len(x_original)):

      tem_original = (y_original[i],x_original[i])
      coordinate_original.append(tem_original)
           
  for y in range(0,len(lon_new)):
      
      tem_new = (y_new[y],x_new[y])
      coordinate_new.append(tem_new)
      
  p = Polygon(coordinate_original)
  p =p.buffer(0.01, 1, join_style=JOIN_STYLE.mitre).buffer(-0.01, 1, join_style=JOIN_STYLE.mitre)
  q = Polygon(coordinate_new)
  inter = p.difference(q)
  
  coordinate = list(inter.exterior.coords)
      
  return coordinate
 
")


#-----------try_difference-----------#
				
#	polygon_difference <- py$difference(Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]][,1],Polygons_data_current_on_map$list[[current_polygon_class]][[i]]$geometry$coordinates[[1]][,2],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[,,1],Polygons_data_current_on_map$list[[current_polygon_class]][[new_polygon_id[poly_number]]]$geometry$coordinates[,,2])
#	print(polygon_difference)
#	t_lon <- c()
#	t_lat <- c()
#	for(d in 1:length(polygon_difference)){
				
#		t_lon[d] <- polygon_difference[[d]][[2]]
#		t_lat[d] <- polygon_difference[[d]][[1]]

#	}
				
#	leafletProxy("image_viewer", session) %>% addPolygons(lng = t_lon, lat = t_lat)
				
#-----------try_difference-----------#
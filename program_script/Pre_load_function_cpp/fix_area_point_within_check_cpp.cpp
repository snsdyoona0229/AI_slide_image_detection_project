#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
DataFrame fix_square_area_point_within_check_cpp(DataFrame point_xy,
                                                 float length_size) {
  NumericVector x = point_xy["x"];
  NumericVector y = point_xy["y"];
  CharacterVector id = point_xy["id"];
  CharacterVector pair_down_id = point_xy["pair_down_id"];
  NumericVector pair_center_x = point_xy["pair_center_x"];
  NumericVector pair_center_y = point_xy["pair_center_y"];
  IntegerVector max_number = point_xy["max_number"];
  
  for(int i = 0; i < (point_xy.nrows() - 1); i++){
    NumericVector point1 = {x[i], y[i]}; 
    for(int j = (i + 1); j < point_xy.nrows(); j++){
      NumericVector point2 = {x[j], y[j]};
      
      if(abs(point1[0] - point2[0]) <= length_size && abs(point1[1] - point2[1]) <= length_size) {
        NumericVector edge1 = {point1[0], point2[1]};
        NumericVector edge2 = {point2[0], point1[1]};
        
        NumericVector center1 = {0,0};
        if (edge1[0] < point2[0]) {
          center1[0] = edge1[0] + length_size/2;
        } else {
          center1[0] = edge1[0] - length_size/2;
        }
        if (edge1[1] < point1[1]) {
          center1[1] = edge1[1] + length_size/2;
        } else {
          center1[1] = edge1[1] - length_size/2;
        }
        
        NumericVector center2 = {0,0};
        if (edge2[0] < point1[0]) {
          center2[0] = edge2[0] + length_size/2;
        } else {
          center2[0] = edge2[0] - length_size/2;
        }
        
        if (edge2[1] < point2[1]) {
          center2[1] = edge2[1] + length_size/2;
        } else {
          center2[1] = edge2[1] - length_size/2;
        }          
        
        int point_within1 = 0;
        int point_within2 = 0;
        
        for(int k = 0; k < point_xy.nrows(); k++){
          if (x[k] >= (center1[0] - length_size/2) && 
              x[k] <= (center1[0] + length_size/2) && 
              y[k] >= (center1[1] - length_size/2) && 
              y[k] <= (center1[1] + length_size/2)) {
            point_within1 = point_within1 + 1;
          }
          
          if (x[k] >= (center2[0] - length_size/2) && 
              x[k] <= (center2[0] + length_size/2) && 
              y[k] >= (center2[1] - length_size/2) && 
              y[k] <= (center2[1] + length_size/2)) {
            point_within2 = point_within2 + 1;
          }
        }
        
        if (point_within1 >= point_within2) {
          if (point_within1 > max_number[i]) {
            max_number[i] = point_within1;
            pair_center_x[i] = center1[0];
            pair_center_y[i] = center1[1];
            pair_down_id[i] = id[j];
          }
        } else {
          if (point_within2 > max_number[i]) {
            max_number[i] = point_within2;
            pair_center_x[i] = center2[0];
            pair_center_y[i] = center2[1];
            pair_down_id[i] = id[j];
          }
        }
      }
    }
  }
  return DataFrame::create(Named("x") = x,         
                           Named("y") = y,
                           Named("id") = id,
                           Named("pair_down_id") = pair_down_id,
                           Named("pair_center_x") = pair_center_x,
                           Named("pair_center_y") = pair_center_y,
                           Named("max_number") = max_number);
}


// [[Rcpp::export]]
DataFrame fix_circle_area_point_within_check_cpp(DataFrame point_xy,
                                                 float length_size) {
  NumericVector x = point_xy["x"];
  NumericVector y = point_xy["y"];
  CharacterVector id = point_xy["id"];
  CharacterVector pair_down_id = point_xy["pair_down_id"];
  NumericVector pair_center_x = point_xy["pair_center_x"];
  NumericVector pair_center_y = point_xy["pair_center_y"];
  IntegerVector max_number = point_xy["max_number"];
  float radius_square = (length_size/2) * (length_size/2);
  
  for(int i = 0; i < (point_xy.nrows() - 1); i++){
    NumericVector point1 = {x[i], y[i]}; 
    for(int j = (i + 1); j < point_xy.nrows(); j++){
      NumericVector point2 = {x[j], y[j]};
      
      if((point1[0] - point2[0]) * (point1[0] - point2[0]) + (point1[1] - point2[1]) * (point1[1] - point2[1]) <= length_size * length_size) {
        NumericVector midpoint = {(point1[0] + point2[0])/2, (point1[1] + point2[1])/2};
        NumericVector vector_arraw = {point1[1] - point2[1], point2[0] - point1[0]};
        float vector_size = (sqrt(length_size/2) * (length_size/2) - 
                             (midpoint[0] - point1[0]) * (midpoint[0] - point1[0]) - 
                             (midpoint[1] - point1[1]) * (midpoint[1] - point1[1]))/(vector_arraw[0] * vector_arraw[0] + vector_arraw[1] * vector_arraw[1]);
        
        NumericVector center1 = {0,0};
        center1[0] = midpoint[0] + vector_size * vector_arraw[0];
        center1[1] = midpoint[1] + vector_size * vector_arraw[1];
        
        NumericVector center2 = {0,0};
        center2[0] = midpoint[0] - vector_size * vector_arraw[0];
        center2[1] = midpoint[1] - vector_size * vector_arraw[1];
        
        int point_within1 = 0;
        int point_within2 = 0;
        
        for(int k = 0; k < point_xy.nrows(); k++){
          if (x[k] >= (center1[0] - length_size/2) && 
              x[k] <= (center1[0] + length_size/2) && 
              y[k] >= (center1[1] - length_size/2) && 
              y[k] <= (center1[1] + length_size/2)) {
            if ((x[k] - center1[0]) * (x[k] - center1[0]) + (y[k] - center1[1]) * (y[k] - center1[1]) <= radius_square){
              point_within1 = point_within1 + 1;
            }
          }
          
          if (x[k] >= (center2[0] - length_size/2) && 
              x[k] <= (center2[0] + length_size/2) && 
              y[k] >= (center2[1] - length_size/2) && 
              y[k] <= (center2[1] + length_size/2)) {
            if ((x[k] - center2[0]) * (x[k] - center2[0]) + (y[k] - center2[1]) * (y[k] - center2[1]) <= radius_square){
              point_within2 = point_within2 + 1;
            }
          }
        }
        
        if (point_within1 >= point_within2) {
          if (point_within1 > max_number[i]) {
            max_number[i] = point_within1;
            pair_center_x[i] = center1[0];
            pair_center_y[i] = center1[1];
            pair_down_id[i] = id[j];
          }
        } else {
          if (point_within2 > max_number[i]) {
            max_number[i] = point_within2;
            pair_center_x[i] = center2[0];
            pair_center_y[i] = center2[1];
            pair_down_id[i] = id[j];
          }
        }
      }
    }
  }
  return DataFrame::create(Named("x") = x,         
                           Named("y") = y,
                           Named("id") = id,
                           Named("pair_down_id") = pair_down_id,
                           Named("pair_center_x") = pair_center_x,
                           Named("pair_center_y") = pair_center_y,
                           Named("max_number") = max_number);
}
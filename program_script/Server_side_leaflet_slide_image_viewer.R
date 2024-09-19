output$image_viewer <- renderLeaflet({

  if (input$Select_function_to_use == "Annotation" | input$Select_function_to_use == "Run AI") {
    isolate({
      modified_point_table <- Points_data_current_on_map$df %>% 
        filter(point_primary == input$Point_class_1) %>% 
        filter(point_secondary == input$Point_class_2) %>% 
        filter(point_tertiary == input$Point_class_3)
      current_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3, "_G_", input$Draw_polygon_layers)
      polygon_names <- Polygons_data_current_on_map$list[[current_polygon_class]] %>% names()
      modified_polygon_list <- Polygons_data_current_on_map$list[[current_polygon_class]]
      
      show_polygon_class <- paste0(input$Polygon_class_1, "_", input$Polygon_class_2, "_", input$Polygon_class_3)
      show_polygon_list <- Polygons_data_current_on_map$list[str_detect(names(Polygons_data_current_on_map$list), 
                                                                        paste0(show_polygon_class, "_G_"))]
      show_heat_class <- paste0(input$Heat_class_1, "_", input$Heat_class_2, "_", input$Heat_class_3)
      show_heat_list <- Heats_data_current_on_map$list[str_detect(names(Heats_data_current_on_map$list), 
                                                                  paste0(show_heat_class, "_H_"))]
      
      if (is.null(modified_polygon_list)) {
        modified_polygon_list <- list()
      }
    })
  }
 
  
  if (input$Slide_or_image == "Image") {
    image_file_name <- list.files(paste0("www/Image/", input$Annotation_group, "/", input$Image_input, "/"), pattern = ".jpg")
    if (length(image_file_name) > 0) {
      split_image_name <- str_split(image_file_name, "___|.jpg") %>% unlist()
      width_img <- split_image_name[2]
      height_img <- split_image_name[3]
      url_img <- paste0("Image/", input$Annotation_group, "/", input$Image_input, "/", image_file_name)
      df <- data.frame(
        w = width_img,
        h = height_img,
        url = url_img,
        stringsAsFactors = FALSE
      )
      
      m <- leaflet(options = leafletOptions(zoomDelta = 0.25, 
                                            zoomSnap = 0, 
                                            minZoom = 1, 
                                            maxZoom = 14, 
                                            doubleClickZoom = FALSE, 
                                            zoomControl = FALSE, 
                                            keyboardPanDelta = 512, 
                                            attributionControl = FALSE)
      ) %>%
        setView(lng = 0, lat = 0, zoom = 9) %>% 
        addFullscreenControl(position = "topleft")
      
      if (input$Select_function_to_use == "Viewer") {
        m <- m %>% onRender("
            function(el, x, data) {
            
              document.getElementById('image_viewer').onkeydown = function (e) {			
            		if(e.keyCode == '54') {
            			 e.stopPropagation();	
            		}
            	};
            	
              var myMap = this;
              var w = data.w,
                  h = data.h,
                  url = data.url;
              var southWest = myMap.unproject([0, h], myMap.getMaxZoom()-3);
              var northEast = myMap.unproject([w, 0], myMap.getMaxZoom()-3);
              var bounds = new L.LatLngBounds(southWest, northEast);
              L.imageOverlay(url, bounds, {className: 'image'}).addTo(myMap);
              myMap.setMaxBounds(bounds);
            }"
                            , data = df)
        
      } else if (input$Select_function_to_use == "Annotation" | input$Select_function_to_use == "Run AI") {
        m <- m %>% 
          #addPmToolbar(targetGroup = c("G_1", "G_2", "G_3", "G_4", "G_5", "G_6", "G_7", "G_8", "G_9", "G_10"), 
          #             toolbarOptions = pmToolbarOptions(drawPolygon = FALSE, 
          #                                               editMode = FALSE, 
          #                                               cutPolygon = FALSE, 
          #                                               removalMode = FALSE, 
          #                                               drawRectangle = FALSE, 
          #                                               drawCircle = FALSE, 
          #                                               drawPolyline = FALSE, 
          #                                               drawMarker = FALSE, 
          #                                               position = "topright")) %>% 
          #addMapPane("Heats", zIndex = 410) %>% 
          addMapPane("Polygons", zIndex = 410) %>%
          addMapPane("Points", zIndex = 420) %>%
          #addEasyButton(easyButton(
          #  position = "topright",
          #  icon = icon("eraser"), title = "Enable delete mode!",
          #  onClick = JS("function(btn, map){ Shiny.setInputValue('delete_mode', [1], {priority: 'event'}); }"))) %>% 
          #addEasyButton(easyButton(
          #  position = "topright",
          #  icon = icon("ban"), title = "Disable delete mode!",
          #  onClick = JS("function(btn, map){ Shiny.setInputValue('delete_mode', [0], {priority: 'event'}); }"))) %>% 
          addLayersControl(overlayGroups = c("P_1", "P_2", "P_3", "P_4", "P_5", "P_6", "P_7", "P_8", "P_9", "P_10",
                                             "G_1", "G_2", "G_3", "G_4", "G_5", "G_6", "G_7", "G_8", "G_9", "G_10",
                                             "H_1", "H_2", "H_3", "H_4", "H_5", "H_6", "H_7", "H_8", "H_9", "H_10")) %>% 
          onRender("
              function(el, x, data) {
              
                document.getElementById('image_viewer').onkeydown = function (e) {			
              		if(e.keyCode == '54') {
              			 e.stopPropagation();	
              		}
              	};
              	
                this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lng, lat];
                        Shiny.onInputChange('mouse_coordinates', coord);
                });
                this.on('mouseout', function(e) {
                        Shiny.onInputChange('mouse_coordinates', null);
                });
                this.on('keypress', function(e) {
                    var keyPressed = e.originalEvent.key;
                    Shiny.setInputValue('free_draw_start', keyPressed, {priority: 'event'});
                    //Shiny.onInputChange('free_draw_start', keyPressed);
                    //alert(keyPressed);
                });
                var myMap = this;
                var w = data.w,
                    h = data.h,
                    url = data.url;
                var southWest = myMap.unproject([0, h], myMap.getMaxZoom()-3);
                var northEast = myMap.unproject([w, 0], myMap.getMaxZoom()-3);
                var bounds = new L.LatLngBounds(southWest, northEast);
                L.imageOverlay(url, bounds, {className: 'image'}).addTo(myMap);
                myMap.setMaxBounds(bounds);
              }
              ", data = df)
      }
    }
  } else if (input$Slide_or_image == "Slide") {
  


    slide_file_name <- list.files(paste0("www/Slide/", input$Annotation_group, "/", input$Image_input, "/"), pattern = ".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax")
    if (length(slide_file_name) > 0) {
      if (str_detect(slide_file_name, pattern = ".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax")) {
        if (input$Slide_minimap_option == "small") {
          zoomlevelfix <- 2
          minimap_diverse <- 1024
          minimap_location <- "_2minimap.jpg"
        } else if (input$Slide_minimap_option == "medium") {
          zoomlevelfix <- 3
          minimap_diverse <- 512
          minimap_location <- "_3minimap.jpg"
        } else if (input$Slide_minimap_option == "large") {
          zoomlevelfix <- 4
          minimap_diverse <- 256
          minimap_location <- "_4minimap.jpg"
        } else if (input$Slide_minimap_option == "giant") {
          zoomlevelfix <- 5
          minimap_diverse <- 128
          minimap_location <- "_5minimap.jpg"
        }
        
        split_slide_name <- str_split(slide_file_name, "___|.ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax") %>% unlist()
        width_slide <- split_slide_name[2] %>% as.integer()
        height_slide <- split_slide_name[3] %>% as.integer()
        lens_slide <- split_slide_name[4] %>% str_replace_all("x", "") %>% as.integer()
        lon_lat_center <- XY2LonLat(x = width_slide/2, y = height_slide/2, zoom = 13)
        zero_location <- XY2LonLat(x = 0, y = 0, zoom = 13)
        max_location <- XY2LonLat(x = width_slide, y = height_slide, zoom = 13)
        
        current_slide_size_info$x_value <- (width_slide/1024) %>% floor()
        current_slide_size_info$y_value <- (height_slide/1024) %>% floor()
        current_slide_size_info$z_value <- lens_slide
        
        m <- leaflet(options = leafletOptions(zoomDelta = 0.25, 
                                              zoomSnap = 0, 
                                              minZoom = 3, 
                                              maxZoom = 16, 
                                              doubleClickZoom = FALSE, 
                                              zoomControl = FALSE, 
                                              keyboardPanDelta = 512, 
                                              attributionControl = FALSE)) %>%
          setView(lng = lon_lat_center$lon, lat = lon_lat_center$lat, zoom = 5) %>% 
          #addEasyButton(easyButton(
          #  icon = icon("arrow-up"), title = "Measure length",
          #  onClick = JS("function(btn, map){ Shiny.setInputValue('start_measuring_length', [0], {priority: 'event'}); }"))) %>% 
          #addEasyButton(easyButton(
          #  icon = icon("recycle"), title = "Clean all measurement",
          #  onClick = JS("function(btn, map){ Shiny.setInputValue('clean_all_measuring_length', [0], {priority: 'event'}); }"))) %>% 
          setMaxBounds(lng1 = zero_location$lon, lat1 = zero_location$lat,
                       lng2 = max_location$lon, lat2 = max_location$lat) %>% 
          addFullscreenControl(position = "topleft") %>%  
          addMiniMap(toggleDisplay = TRUE,
                     position = "bottomright",
                     tiles = paste0("Slide/", input$Annotation_group, "/", input$Image_input, "/temp/{x}_{y}", minimap_location),
                     centerFixed = lon_lat_center,
                     aimingRectOptions = list(color = "#ff0000", weight = 3, clickable = FALSE),
                     zoomLevelFixed = zoomlevelfix, 
                     width = width_slide/minimap_diverse, height = height_slide/minimap_diverse) 
        
        if (input$Select_function_to_use == "Viewer") {
          m <- m %>% 
            addEasyButtonBar(
              easyButton(
                icon = '<strong>0.625x</strong>', title = "Change view to 0.625x",
                onClick = JS("function(btn, map){ map.setView(map.getCenter(), 6); }"), id = "zoom_but1"),
              easyButton(
                icon = '<strong>1.25x</strong>', title = "Change view to 1.25x",
                onClick = JS("function(btn, map){ map.setView(map.getCenter(), 7); }"), id = "zoom_but2"),
              easyButton(
                icon = '<strong>2.5x</strong>', title = "Change view to 2.5x",
                onClick = JS("function(btn, map){ map.setView(map.getCenter(), 8); }"), id = "zoom_but3"),
              easyButton(
                icon = '<strong>5x</strong>', title = "Change view to 5x",
                onClick = JS("function(btn, map){ map.setView(map.getCenter(), 9); }"), id = "zoom_but4"),
              easyButton(
                icon = '<strong>10x</strong>', title = "Change view to 10x",
                onClick = JS("function(btn, map){ map.setView(map.getCenter(), 10); }"), id = "zoom_but5"),
              easyButton(
                icon = '<strong>20x</strong>', title = "Change view to 20x",
                onClick = JS("function(btn, map){ map.setView(map.getCenter(), 11); }"), id = "zoom_but6"),
              easyButton(
                icon = '<strong>40x</strong>', title = "Change view to 40x",
                onClick = JS("function(btn, map){ map.setView(map.getCenter(), 12); }"), id = "zoom_but7"),
              easyButton(
                icon = '<strong>80x</strong>', title = "Change view to 80x",
                onClick = JS("function(btn, map){ map.setView(map.getCenter(), 13); }"), id = "zoom_but8"),
              easyButton(
                icon = '<strong>160x</strong>', title = "Change view to 160x",
                onClick = JS("function(btn, map){ map.setView(map.getCenter(), 14); }"), id = "zoom_but9")
            ) %>% 
            addEasyButton(easyButton(
              icon = icon("print"), title = "Print Screen Current View",
              onClick = JS("function(btn, map){ Shiny.setInputValue('start_print_screen', [map.getZoom(), map.getCenter(), map.getBounds()], {priority: 'event'}); }")))

              text_file_name <- paste0(input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input)
              slide_name <- list.files(paste0("www/", text_file_name), pattern = ".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax", full.names = TRUE)
			   
        if (input$Slide_viewer_option == "RAW" & str_detect(slide_name, pattern = ".svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax")){
            m <- m %>% 
              onRender("
                function(el, x, data) {
                
                  document.getElementById('image_viewer').onkeydown = function (e) {			
                		if(e.keyCode == '54') {
                			 e.stopPropagation();	
                		}
                	};
                
                  var myMap = this;
                  this.on('mousemove', function(e) {
                      var lat = e.latlng.lat;
                      var lng = e.latlng.lng;
                      var coord = [lng, lat];
                      Shiny.onInputChange('mouse_coordinates', coord);
                  });
                  this.on('mouseout', function(e) {
                      Shiny.onInputChange('mouse_coordinates', null);
                  });
                  L.Control.textbox = L.Control.extend({
                		onAdd: function(map) {
                		  var text = L.DomUtil.create('div');
                  		text.id = 'zoom_level_text';
                  		var zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                  		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                  		text.style.backgroundColor = 'rgba(255,255,255,0.6)';
                  		
                  		
                  		map.on('zoomend',function(e){
                  		  zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                    		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                    	});
                  	  return text;
                		},
                		onRemove: function(map) {
                			// Nothing to do here
                		}
                	});
                	L.control.textbox = function(opts) { return new L.Control.textbox(opts);}
                	L.control.textbox({ position: 'bottomleft'}).addTo(this);
                  L.GridLayer.CanvasCircles = L.GridLayer.extend({
                      createTile: function (coords) {
                          Shiny.setInputValue('test_coords', coords, {priority: 'event'});
                          var tile = document.createElement('canvas');
                          var tileSize = this.getTileSize();
                          var tile_id = coords.x + '-' + coords.y + '-' + coords.z;
                          tile.setAttribute('width', tileSize.x);
                          tile.setAttribute('height', tileSize.y);
                          Shiny.addCustomMessageHandler(tile_id,
                            function(raw_data) {
                                var image_data = new Uint8Array(raw_data);
                                var ctx = tile.getContext('2d');
                                var imgData = ctx.createImageData(1024, 1024);
                                var i;
                                for (i = 0; i < 1048576; i ++) {
                                  imgData.data[i * 4 +0] =  image_data[i];
                                  imgData.data[i * 4 +1] =  image_data[1048576+i];
                                  imgData.data[i * 4 +2] =  image_data[2097152+i];
                                  imgData.data[i * 4 +3] = 255;
                                }
                                ctx.putImageData(imgData, 0, 0);
                            }
                          );
                          return tile;
                      }
                  });
                  canvasCircles = function(opts) {
                    return new L.GridLayer.CanvasCircles({maxNativeZoom: 12,
                                                          minNativeZoom: 5,
                                                          tileSize: 1024});
                  };
                  var canvasCirclesLayer = canvasCircles(); //This is what makes the difference!
                  this.addLayer(canvasCirclesLayer);
                }"#, data = df
              )
          }else if (input$Slide_viewer_option == "RAW" & str_detect(slide_name, pattern = ".ndpi")) {
		  
		    m <- m %>% 
              onRender("
                function(el, x, data) {
                
                  document.getElementById('image_viewer').onkeydown = function (e) {			
                		if(e.keyCode == '54') {
                			 e.stopPropagation();	
                		}
                	};
                
                  var myMap = this;
                  this.on('mousemove', function(e) {
                      var lat = e.latlng.lat;
                      var lng = e.latlng.lng;
                      var coord = [lng, lat];
                      Shiny.onInputChange('mouse_coordinates', coord);
                  });
                  this.on('mouseout', function(e) {
                      Shiny.onInputChange('mouse_coordinates', null);
                  });
                  L.Control.textbox = L.Control.extend({
                		onAdd: function(map) {
                		  var text = L.DomUtil.create('div');
                  		text.id = 'zoom_level_text';
                  		var zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                  		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                  		text.style.backgroundColor = 'rgba(255,255,255,0.6)';
                  		
                  		
                  		map.on('zoomend',function(e){
                  		  zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                    		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                    	});
                  	  return text;
                		},
                		onRemove: function(map) {
                			// Nothing to do here
                		}
                	});
                	L.control.textbox = function(opts) { return new L.Control.textbox(opts);}
                	L.control.textbox({ position: 'bottomleft'}).addTo(this);
                  L.GridLayer.CanvasCircles = L.GridLayer.extend({
                      createTile: function (coords) {
                          Shiny.setInputValue('test_coords', coords, {priority: 'event'});
                          var tile = document.createElement('canvas');
                          var tileSize = this.getTileSize();
                          var tile_id = coords.x + '-' + coords.y + '-' + coords.z;
                          tile.setAttribute('width', tileSize.x);
                          tile.setAttribute('height', tileSize.y);
                          Shiny.addCustomMessageHandler(tile_id,
                            function(raw_data) {
                                var image_data = new Uint8Array(raw_data);
                                var ctx = tile.getContext('2d');
                                var imgData = ctx.createImageData(1024, 1024);
                                var i;
                                for (i = 0; i < 1048576; i ++) {
                                  imgData.data[i * 4 +0] = image_data[i * 3 + 2];
                                  imgData.data[i * 4 +1] = image_data[i * 3 + 1];
                                  imgData.data[i * 4 +2] = image_data[i * 3];
                                  imgData.data[i * 4 +3] = 255;
                                }
                                ctx.putImageData(imgData, 0, 0);
                            }
                          );
                          return tile;
                      }
                  });
                  canvasCircles = function(opts) {
                    return new L.GridLayer.CanvasCircles({maxNativeZoom: 12,
                                                          minNativeZoom: 5,
                                                          tileSize: 1024});
                  };
                  var canvasCirclesLayer = canvasCircles(); //This is what makes the difference!
                  this.addLayer(canvasCirclesLayer);
                }"#, data = df
              )
		  
		  }else if (input$Slide_viewer_option == "Tile") {
            m <- m %>% 
              addTiles(urlTemplate = paste0("Slide/", input$Annotation_group, "/", input$Image_input, "/temp/{x}_{y}_{z}.jpg"), options = tileOptions(tileSize = 1024, maxNativeZoom = 12, minNativeZoom = 5)) %>%
              onRender("
                function(el, x, data) {
                
                  document.getElementById('image_viewer').onkeydown = function (e) {			
                		if(e.keyCode == '54') {
                			 e.stopPropagation();	
                		}
                	};
                
                  var myMap = this;
                  this.on('mousemove', function(e) {
                            var lat = e.latlng.lat;
                            var lng = e.latlng.lng;
                            var coord = [lng, lat];
                            Shiny.onInputChange('mouse_coordinates', coord);
                  });
                  this.on('mouseout', function(e) {
                          Shiny.onInputChange('mouse_coordinates', null);
                  });
                  L.Control.textbox = L.Control.extend({
                		onAdd: function(map) {
                		  var text = L.DomUtil.create('div');
                  		text.id = 'zoom_level_text';
                  		var zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                  		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                  		text.style.backgroundColor = 'rgba(255,255,255,0.6)';
                      		
                  		map.on('zoomend',function(e){
                  		  zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                    		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                    	});
                  	  return text;
                		},
                		onRemove: function(map) {
                			// Nothing to do here
                		}
                	});
                	L.control.textbox = function(opts) { return new L.Control.textbox(opts);}
                	L.control.textbox({ position: 'bottomleft'}).addTo(this); 
                }"#, data = df
              )
          } else if (input$Slide_viewer_option == "JPG") {
            m <- m %>%
              onRender("
                function(el, x, data) {
                
                  document.getElementById('image_viewer').onkeydown = function (e) {			
                		if(e.keyCode == '54') {
                			 e.stopPropagation();	
                		}
                	};
                
                  var myMap = this;
                  this.on('mousemove', function(e) {
                     var lat = e.latlng.lat;
                     var lng = e.latlng.lng;
                     var coord = [lng, lat];
                     Shiny.onInputChange('mouse_coordinates', coord);
                  });
                  this.on('mouseout', function(e) {
                    Shiny.onInputChange('mouse_coordinates', null);
                  });
				 
				  
				  
                  L.Control.textbox = L.Control.extend({
                		onAdd: function(map) {
                		  var text = L.DomUtil.create('div');
                  		text.id = 'zoom_level_text';
              	    	var zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
              		    text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
              		    text.style.backgroundColor = 'rgba(255,255,255,0.6)';
                  		map.on('zoomend',function(e){
                  		  zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                    		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                    	});
              	      return text;
            		    },
            		    onRemove: function(map) {
            			    // Nothing to do here
            		    }
            	    });
            	    L.control.textbox = function(opts) { return new L.Control.textbox(opts);}
            	    L.control.textbox({ position: 'bottomleft'}).addTo(this);
                  L.GridLayer.CanvasCircles = L.GridLayer.extend({
                      createTile: function (coords) {
                          Shiny.setInputValue('test_coords', coords, {priority: 'event'});
                          var tile = document.createElement('canvas');
                          var tileSize = this.getTileSize();
                          var tile_id = coords.x + '-' + coords.y + '-' + coords.z;
                          tile.setAttribute('width', tileSize.x);
                          tile.setAttribute('height', tileSize.y);
                          Shiny.addCustomMessageHandler(tile_id,
                            function(raw_data) {
                                var ctx = tile.getContext('2d');
                                var img = new Image();
                                  img.onload = function(){
                                    ctx.drawImage(img,0,0);
                                  };
                                img.src = 'data:image/jpeg;base64,' + raw_data;
                            }
                          );
                          return tile;
                      }
                  });
                  canvasCircles = function(opts) {
                    return new L.GridLayer.CanvasCircles({maxNativeZoom: 12,
                                                          minNativeZoom: 5,
                                                          tileSize: 1024});
                  };
                  var canvasCirclesLayer = canvasCircles(); //This is what makes the difference!
                  this.addLayer(canvasCirclesLayer);
                }"#, data = df
              )
          }
        } else if (input$Select_function_to_use == "Annotation"| input$Select_function_to_use == "Run AI") {
		
          m <- m %>% 
            #addPmToolbar(targetGroup = c("G_1", "G_2", "G_3", "G_4", "G_5", "G_6", "G_7", "G_8", "G_9", "G_10"), 
            #             toolbarOptions = pmToolbarOptions(drawPolygon = FALSE, 
            #                                               editMode = FALSE, 
            #                                               cutPolygon = FALSE, 
            #                                               removalMode = FALSE, 
            #                                               drawRectangle = FALSE, 
            #                                               drawCircle = FALSE, 
            #                                               drawPolyline = FALSE, 
            #                                               drawMarker = FALSE, 
            #                                               position = "topright")) %>% 
            #addMapPane("Heats", zIndex = 410) %>% 
            addMapPane("Polygons", zIndex = 410) %>%
            addMapPane("Points", zIndex = 420) %>%
            #addEasyButton(easyButton(
            #  position = "topright",
            #  icon = icon("eraser"), title = "Enable delete mode!",
            #  onClick = JS("function(btn, map){ Shiny.setInputValue('delete_mode', [1], {priority: 'event'}); }"))) %>% 
            #addEasyButton(easyButton(
            #  position = "topright",
            #  icon = icon("ban"), title = "Disable delete mode!",
            #  onClick = JS("function(btn, map){ Shiny.setInputValue('delete_mode', [0], {priority: 'event'}); }"))) %>% 
            addLayersControl(overlayGroups = c("P_1", "P_2", "P_3", "P_4", "P_5", "P_6", "P_7", "P_8", "P_9", "P_10",
                                               "G_1", "G_2", "G_3", "G_4", "G_5", "G_6", "G_7", "G_8", "G_9", "G_10",
                                               "H_1", "H_2", "H_3", "H_4", "H_5", "H_6", "H_7", "H_8", "H_9", "H_10")) %>% 
            addEasyprint(options = easyprintOptions(
              tileWait = 50000,
              sizeModes = list("A4Portrait"),
              title = 'Print map',
              position = 'topleft',
              exportOnly = TRUE))  %>%
            addEasyButton(easyButton(
              icon = icon("print"), title = "Print Screen Current View",
              onClick = JS("function(btn, map){ Shiny.setInputValue('start_print_screen', [map.getZoom(), map.getCenter(), map.getBounds()], {priority: 'event'}); }")))
			  
              text_file_name <- paste0(input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input)
              slide_name <- list.files(paste0("www/", text_file_name), pattern = ".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax", full.names = TRUE)
          
        if (input$Slide_viewer_option == "RAW" & str_detect(slide_name, pattern = ".svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax")){
            m <- m %>% 
              onRender("
                function(el, x, data) {
                
                  document.getElementById('image_viewer').onkeydown = function (e) {			
                		if(e.keyCode == '54') {
                			 e.stopPropagation();	
                		}
                	};
                
                  var myMap = this;
                  this.on('mousemove', function(e) {
                      var lat = e.latlng.lat;
                      var lng = e.latlng.lng;
                      var coord = [lng, lat];
                      Shiny.onInputChange('mouse_coordinates', coord);
                  });
                  this.on('mouseout', function(e) {
                      Shiny.onInputChange('mouse_coordinates', null);
                  });
                  L.Control.textbox = L.Control.extend({
                		onAdd: function(map) {
                		  var text = L.DomUtil.create('div');
                  		text.id = 'zoom_level_text';
                  		var zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                  		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                  		text.style.backgroundColor = 'rgba(255,255,255,0.6)';
                  		
                  		
                  		map.on('zoomend',function(e){
                  		  zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                    		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                    	});
                  	  return text;
                		},
                		onRemove: function(map) {
                			// Nothing to do here
                		}
                	});
                	L.control.textbox = function(opts) { return new L.Control.textbox(opts);}
                	L.control.textbox({ position: 'bottomleft'}).addTo(this);
                  L.GridLayer.CanvasCircles = L.GridLayer.extend({
                      createTile: function (coords) {
                          Shiny.setInputValue('test_coords', coords, {priority: 'event'});
                          var tile = document.createElement('canvas');
                          var tileSize = this.getTileSize();
                          var tile_id = coords.x + '-' + coords.y + '-' + coords.z;
                          tile.setAttribute('width', tileSize.x);
                          tile.setAttribute('height', tileSize.y);
                          Shiny.addCustomMessageHandler(tile_id,
                            function(raw_data) {

								var image_data = new Uint8Array(raw_data);
								console.log(image_data);
                                var ctx = tile.getContext('2d');
                                var imgData = ctx.createImageData(1024, 1024);
                                var i;
                                for (i = 0; i < 1048576; i ++) {
                                  imgData.data[i * 4 +0] =  image_data[i];
                                  imgData.data[i * 4 +1] =  image_data[1048576+i];
                                  imgData.data[i * 4 +2] =  image_data[2097152+i];
                                  imgData.data[i * 4 +3] = 255;
                                }
                                ctx.putImageData(imgData, 0, 0);
                            }
                          );
                          return tile;
                      }
                  });
                  canvasCircles = function(opts) {
                    return new L.GridLayer.CanvasCircles({maxNativeZoom: 12,
                                                          minNativeZoom: 5,
                                                          tileSize: 1024});
                  };
                  var canvasCirclesLayer = canvasCircles(); //This is what makes the difference!
                  this.addLayer(canvasCirclesLayer);
                }"#, data = df
              )
             }else if(input$Slide_viewer_option == "RAW" & str_detect(slide_name, pattern = ".ndpi")) {
		  
		      m <- m %>% 
              onRender("
                function(el, x, data) {
                
                  document.getElementById('image_viewer').onkeydown = function (e) {			
                		if(e.keyCode == '54') {
                			 e.stopPropagation();	
                		}
                	};
                
                  var myMap = this;
                  this.on('mousemove', function(e) {
                      var lat = e.latlng.lat;
                      var lng = e.latlng.lng;
                      var coord = [lng, lat];
                      Shiny.onInputChange('mouse_coordinates', coord);
                  });
                  this.on('mouseout', function(e) {
                      Shiny.onInputChange('mouse_coordinates', null);
                  });
                  L.Control.textbox = L.Control.extend({
                		onAdd: function(map) {
                		  var text = L.DomUtil.create('div');
                  		text.id = 'zoom_level_text';
                  		var zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                  		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                  		text.style.backgroundColor = 'rgba(255,255,255,0.6)';
                  		
                  		
                  		map.on('zoomend',function(e){
                  		  zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                    		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                    	});
                  	  return text;
                		},
                		onRemove: function(map) {
                			// Nothing to do here
                		}
                	});
                	L.control.textbox = function(opts) { return new L.Control.textbox(opts);}
                	L.control.textbox({ position: 'bottomleft'}).addTo(this);
                  L.GridLayer.CanvasCircles = L.GridLayer.extend({
                      createTile: function (coords) {
                          Shiny.setInputValue('test_coords', coords, {priority: 'event'});
                          var tile = document.createElement('canvas');
                          var tileSize = this.getTileSize();
                          var tile_id = coords.x + '-' + coords.y + '-' + coords.z;
                          tile.setAttribute('width', tileSize.x);
                          tile.setAttribute('height', tileSize.y);
                          Shiny.addCustomMessageHandler(tile_id,
                            function(raw_data) {

								var image_data = new Uint8Array(raw_data);
								console.log(image_data);
                                var ctx = tile.getContext('2d');
                                var imgData = ctx.createImageData(1024, 1024);
                                var i;
                                for (i = 0; i < 1048576; i ++) {
                                  imgData.data[i * 4 +0] = image_data[i * 3 + 2];
                                  imgData.data[i * 4 +1] = image_data[i * 3 + 1];
                                  imgData.data[i * 4 +2] = image_data[i * 3];
                                  imgData.data[i * 4 +3] = 255;
                                }
                                ctx.putImageData(imgData, 0, 0);
                            }
                          );
                          return tile;
                      }
                  });
                  canvasCircles = function(opts) {
                    return new L.GridLayer.CanvasCircles({maxNativeZoom: 12,
                                                          minNativeZoom: 5,
                                                          tileSize: 1024});
                  };
                  var canvasCirclesLayer = canvasCircles(); //This is what makes the difference!
                  this.addLayer(canvasCirclesLayer);
                }"#, data = df
		   )
		  }else if (input$Slide_viewer_option == "Tile") {
            m <- m %>% 
              addTiles(urlTemplate = paste0("Slide/", input$Annotation_group, "/", input$Image_input, "/temp/{x}_{y}_{z}.jpg"), options = tileOptions(tileSize = 1024, maxNativeZoom = 12, minNativeZoom = 5)) %>%
              onRender("
                function(el, x, data) {
                
                  document.getElementById('image_viewer').onkeydown = function (e) {			
                		if(e.keyCode == '54') {
                			 e.stopPropagation();	
                		}
                	};
                
                  var myMap = this;
                  L.Control.textbox = L.Control.extend({
                		onAdd: function(map) {
                		  var text = L.DomUtil.create('div');
                  		text.id = 'zoom_level_text';
                  		var zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                  		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                  		text.style.backgroundColor = 'rgba(255,255,255,0.6)';
                  		map.on('zoomend',function(e){
                  		  zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                    		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                    	});
						 map.on('moveend',function(e){
                  		  zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
							Shiny.setInputValue('mouse', [zoom_level, map.getCenter(), map.getBounds()], {priority: 'event'});

                    	  });
                  	  return text;
                		},
                		onRemove: function(map) {
                			// Nothing to do here
                		}
                	});
                	L.control.textbox = function(opts) { return new L.Control.textbox(opts);}
                	L.control.textbox({ position: 'bottomleft'}).addTo(this); 
                  this.on('mousemove', function(e) {
                            var lat = e.latlng.lat;
                            var lng = e.latlng.lng;
                            var coord = [lng, lat];
                            Shiny.onInputChange('mouse_coordinates', coord);
                  });
                  this.on('mouseout', function(e) {
                          Shiny.onInputChange('mouse_coordinates', null);
                  });
                  this.on('keypress', function(e) {
                      var keyPressed = e.originalEvent.key;
                      Shiny.setInputValue('free_draw_start', keyPressed, {priority: 'event'});
                  });
                }"#, data = df
              )
          } else if (input$Slide_viewer_option == "JPG") {
            m <- m %>% 
              onRender("
                function(el, x, data) {
                
                  document.getElementById('image_viewer').onkeydown = function (e) {			
                		if(e.keyCode == '54') {
                			 e.stopPropagation();	
                		}
                	};
                
                  var myMap = this;
                  L.Control.textbox = L.Control.extend({
                		onAdd: function(map) {
                		  var text = L.DomUtil.create('div');
                  		text.id = 'zoom_level_text';
                  		var zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                  		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';
                  		text.style.backgroundColor = 'rgba(255,255,255,0.6)';
                  		map.on('zoomend',function(e){
                  		  zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
                    		text.innerHTML = '<h3>Zoom Level: ' + zoom_level + 'x</h3>';

                    	  });
						  map.on('moveend',function(e){
                  		  zoom_level = Math.round(Math.pow(2, map.getZoom()-12)* 40 * 100)/100;
							Shiny.setInputValue('mouse', [zoom_level, map.getCenter(), map.getBounds()], {priority: 'event'});

                    	  });
                  	  return text;

                		},
                		onRemove: function(map) {
                			// Nothing to do here
                		}
                	});
                	L.control.textbox = function(opts) { return new L.Control.textbox(opts);}
                	L.control.textbox({ position: 'bottomleft'}).addTo(this);
                  this.on('mousemove', function(e) {
				  
                          var lat = e.latlng.lat;
                          var lng = e.latlng.lng;
                          var coord = [lng, lat];

                          Shiny.onInputChange('mouse_coordinates', coord);
                  });
                  this.on('mouseout', function(e) {
                          Shiny.onInputChange('mouse_coordinates',null);
                  });

                  this.on('keypress', function(e) {
                      var keyPressed = e.originalEvent.key;
                      Shiny.setInputValue('free_draw_start', keyPressed, {priority: 'event'});
                  });
                  L.GridLayer.CanvasCircles = L.GridLayer.extend({
                      createTile: function (coords) {
                          Shiny.setInputValue('test_coords', coords, {priority: 'event'});
                          var tile = document.createElement('canvas');
                          var tileSize = this.getTileSize();
                          var tile_id = coords.x + '-' + coords.y + '-' + coords.z;
                          tile.setAttribute('width', tileSize.x);
                          tile.setAttribute('height', tileSize.y);
                          Shiny.addCustomMessageHandler(tile_id,
                            function(raw_data) {
                                var ctx = tile.getContext('2d');
                                var img = new Image();
                                  img.onload = function(){
                                    ctx.drawImage(img,0,0);
                                  };
                                img.src = 'data:image/jpeg;base64,' + raw_data;
								
								requestAnimationFrame(raw_data);

                            }
							
                          );
						   
                          return tile;
                      }
                  });
				
                  canvasCircles = function(opts) {
                    return new L.GridLayer.CanvasCircles({maxNativeZoom: 12,
                                                          minNativeZoom: 5,
                                                          tileSize: 1024});
                  };
                  var canvasCirclesLayer = canvasCircles(); //This is what makes the difference!
                  this.addLayer(canvasCirclesLayer);
                }
				"#, data = df
              )
          }
		  
        }
      }
    }
  }

if(file.exists("www/Select_customuze_AI_to_use.RDS")==FALSE){

 saveRDS("good_morning01",file = "www/Select_customuze_AI_to_use.RDS")

}
if(file.exists(paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/temp/previous.rds"))==TRUE){

saveRDS(NULL,paste0("www/", input$Slide_or_image, "/" ,input$Annotation_group, "/", input$Image_input, "/temp/previous.rds"))

}

 if (exists("m")) {


#-----------------------------------------------change page 1003------------------------------------------------------------------------#
  
customize_AI_model_viewer_switch_input <- FALSE
customize_AI_model_viewer_switch <- FALSE


Annotation_group <- input$Annotation_group
Select_function_to_use <- input$Select_function_to_use

Select_customuze_AI_to_use <- readRDS( "www/Select_customuze_AI_to_use.RDS")

   interface_information <- data.frame(
      Annotation_group = input$Annotation_group,
      Select_function_to_use = input$Select_function_to_use
    )
	
saveRDS(interface_information, file = "www/interface_information.RDS")


interface_information <- readRDS("www/interface_information.RDS")


if(is.null(input$customize_AI_model_viewer_switch)== FALSE ){

  customize_AI_model_viewer_switch_input <- input$customize_AI_model_viewer_switch
  
  interface_information <- readRDS("www/interface_information.RDS")
  
  Annotation_group <- interface_information$Annotation_group
  Select_function_to_use <- interface_information$Select_function_to_use
  
  
  if(input$customize_AI_model_viewer_switch==FALSE){

      customize_AI_model_viewer_switch <- FALSE
  }
  if( Select_function_to_use =="Run AI" & Annotation_group =="RUN_AI"){

      customize_AI_model_viewer_switch <- TRUE
	  #Select_customuze_AI_to_use  <- input$Select_customuze_AI_to_use
  }
  if(input$Select_function_to_use == "Annotation" ){
      
      customize_AI_model_viewer_switch <- FALSE
     }
}

  if((Select_function_to_use == "Annotation" & input$Annotation_group == "all")| Select_function_to_use == "Viewer" ){
    
	saveRDS(" ", file = "www/Select_customuze_AI_to_use.RDS")
	Select_customuze_AI_to_use <- readRDS( "www/Select_customuze_AI_to_use.RDS")

 }
 Select_customuze_AI_to_use <- readRDS( "www/Select_customuze_AI_to_use.RDS")
 interface_information <- readRDS( "www/interface_information.RDS")

if ((input$Select_function_to_use == "Annotation" &  Select_customuze_AI_to_use != "good_morning01") | (Select_customuze_AI_to_use == "good_morning01" & customize_AI_model_viewer_switch==TRUE & customize_AI_model_viewer_switch_input==TRUE)){

      polygon_show_name <- names(show_polygon_list)
      if (length(polygon_show_name) > 0) {
        polygon_show_color_code <- str_replace_all(polygon_show_name, paste0(paste0(show_polygon_class, "_G_")), "")



#-----------------------------------------------change page 1003------------------------------------------------------------------------#
isolate({
   
	if(length(Polygons_data_current_on_map$list[[polygon_show_name]]) <= 2000){
	      

          for (k in polygon_show_name) {
            polygon_names <- Polygons_data_current_on_map$list[[k]] %>% names()
            color_number <- str_replace_all(k, paste0(paste0(show_polygon_class, "_G_")), "")
            geocolor <- input$Change_polygon_color_order[as.integer(color_number)]
            geoopcity <- input$Polygon_opacity_value
            
           for (i in polygon_names) {
			
			  if((length(Polygons_data_current_on_map$list[[k]][[i]]$geometry$coordinates[[1]])==1 & length(Polygons_data_current_on_map$list[[k]][[i]]$geometry$coordinates) > 10) | length(Polygons_data_current_on_map$list[[k]][[i]]$geometry$coordinates[[1]]) > 1){ 
              m <- m %>% 
                addGeoJSON(Polygons_data_current_on_map$list[[k]][[i]], 
                           layerId = i, 
                           group = paste0("G_", color_number),
                           fillOpacity = geoopcity, 
                           color = geocolor, 
                           options = leafletOptions(pane = "Polygons"))
						   
				
            }
            
          }
	
	  }
	}	
 
 if(length(Polygons_data_current_on_map$list[[polygon_show_name]]) > 2000){
 
    count_small_polygon <- 0 
	count_list <- c()
	lon <- c()
	lat <- c()
	  
     for (k in polygon_show_name) {
            polygon_names <- Polygons_data_current_on_map$list[[k]] %>% names()
            color_number <- str_replace_all(k, paste0(paste0(show_polygon_class, "_G_")), "")
            geocolor <- input$Change_polygon_color_order[as.integer(color_number)]
            geoopcity <- input$Polygon_opacity_value
			
            for (i in polygon_names) {
              
			  if((length(Polygons_data_current_on_map$list[[k]][[i]]$geometry$coordinates[[1]])==1 & length(Polygons_data_current_on_map$list[[k]][[i]]$geometry$coordinates) > 16) | length(Polygons_data_current_on_map$list[[k]][[i]]$geometry$coordinates[[1]]) > 1){ 
				  m <- m %>% 
                     addGeoJSON(Polygons_data_current_on_map$list[[k]][[i]], 
                           layerId = i, 
                           group = paste0("G_", color_number),
                           fillOpacity = geoopcity, 
                           color = geocolor, 
                           options = leafletOptions(pane = "Polygons"))
						   
				}
             }
         } 
	}



 })
 

#-----------------------------------------------change page 1003------------------------------------------------------------------------#
  
}

if (length(modified_point_table$point_id) > 20000 ){

    z <- length(modified_point_table$point_id)/ceiling(length(modified_point_table$point_id)/20000)
	sample_points <- sample(1:length(modified_point_table$point_id),z, replace=FALSE)
      
      isolate({
        pointcolor <- input$Change_point_color_order[as.integer(modified_point_table$point_group[sample_points] %>% str_replace_all("P_", ""))]
        pointopcity <- (1 - sum(input$Point_circle_switch))
        pointradius <- sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting

      }) 

m <- m  %>%
        addCircleMarkers(lng = as.numeric(modified_point_table$point_lon[sample_points]), 
                         lat = as.numeric(modified_point_table$point_lat[sample_points]), 
                         radius = pointradius, 
                         fillOpacity = pointopcity, 
                         color = pointcolor, 
                         group = modified_point_table$point_group[sample_points], 
                         layerId = modified_point_table$point_id[sample_points], 
                         options = leafletOptions(pane = "Points")) #%>% 

}else{

     isolate({
        pointcolor <- input$Change_point_color_order[as.integer(modified_point_table$point_group %>% str_replace_all("P_", ""))]
        pointopcity <- (1 - sum(input$Point_circle_switch))
        pointradius <- sum(input$Point_circle_switch) * input$Circle_radius_setting + sum(1 - input$Point_circle_switch) * input$Point_radius_setting

      }) 

    m <- m  %>%
        addCircleMarkers(lng = as.numeric(modified_point_table$point_lon), 
                         lat = as.numeric(modified_point_table$point_lat), 
                         radius = pointradius, 
                         fillOpacity = pointopcity, 
                         color = pointcolor, 
                         group = modified_point_table$point_group, 
                         layerId = modified_point_table$point_id, 
                         options = leafletOptions(pane = "Points")) #%>% 

}
        #addHeatmap(
        #  lng = as.numeric(modified_point_table$point_lon), lat = as.numeric(modified_point_table$point_lat),
        #  blur = 60, max = 0.05, radius = 20
        #)
      heat_show_name <- names(show_heat_list)
      if (length(heat_show_name) > 0) {
        #polygon_show_color_code <- str_replace_all(polygon_show_name, paste0(paste0(show_polygon_class, "_G_")), "")

        isolate({
          for (k in heat_show_name) {

            size_heat <- Heats_data_current_on_map$list[[k]][[2]]
            #print(size_heat)
            m <- m  %>%
              addTiles(urlTemplate = paste0("Slide/", 
                                            input$Annotation_group, "/", 
                                           input$Image_input, "/temp/",
                                            k,
                                           "_{x}_{y}.png?r=",
                                            sample(100000,1)), 
                       options = leafletOptions(tileSize = size_heat, 
                                                zIndex = 405,
                                                maxNativeZoom = 5, 
                                                minNativeZoom = 5,
                                                opacity = input$Heat_opacity_value
                       ), 
                       group = (k %>% str_split("_") %>% unlist())[4:5] %>% paste0(collapse = "_"),
                       layerId = k)
            
                   }
		  
              })
		
           }
	    }
			

    m # %>%
      #addTiles(urlTemplate = "www/Slide/test.png", options = tileOptions(tileSize = 1024, maxNativeZoom = 12, minNativeZoom = 5)) 
   
  }

})


observeEvent(input$test_coords, {
  
  coor_data <- input$test_coords %>% as.integer() %>% paste0(collapse = "-")
  location_tile <- input$test_coords %>% as.integer()
  
  text_file_name <- paste0(input$Slide_or_image, "/", input$Annotation_group, "/", input$Image_input)
  slide_name <- list.files(paste0("www/", text_file_name), pattern = ".ndpi|.svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif|.isyntax", full.names = TRUE)
  
  if (length(slide_name) != 0) {
    if (str_detect(slide_name, pattern = ".ndpi")){
      if (input$test_coords$z <= 12) {
        x_max <- (current_slide_size_info$x_value/2^(12 - input$test_coords$z)) %>% floor()
        y_max <- (current_slide_size_info$y_value/2^(12 - input$test_coords$z)) %>% floor()
        if (input$test_coords$x <= x_max & input$test_coords$y <= y_max) {
          if (input$Slide_viewer_option == "RAW") {

            future({
              NDPI_BGR_raw(file_name = slide_name, x = location_tile[1], y = location_tile[2], z = location_tile[3], tilesize = 1024)
            }, seed=NULL) %...>%
              session$sendBinaryMessage(type = coor_data, 
                                        message = .) 
            #session$sendBinaryMessage(type = coor_data, 
            #                          message = NDPI_BGR_raw(file_name = slide_name, x = location_tile[1], y = location_tile[2], z = location_tile[3], tilesize = 1024)
            #)
          } else if (input$Slide_viewer_option == "JPG") {
            future({
              NDPI_jpg_string(file_name = slide_name, x = location_tile[1], y = location_tile[2], z = location_tile[3], tilesize = 1024)
            }, seed=NULL) %...>%
              session$sendCustomMessage(type = coor_data, 
                                        message = .)
										            
			#session$sendCustomMessage(type = coor_data, 
            #                          message = NDPI_jpg_string(file_name = slide_name, x = location_tile[1], y = location_tile[2], z = location_tile[3], tilesize = 1024)
            #) 
          }
          #print(str(data_send))
        } else {
          #session$sendCustomMessage(type = coor_data,
          #                          message = "blank_bin_256.txt")
        }
      }
    } else if (str_detect(slide_name, pattern = ".svs|.tif|.vms|.vmu|.scn|.mrxs|.tiff|.svslide|.bif")) {
	
      if (input$test_coords$z <= 12) {
        if (current_openslide_info$openslide_name == slide_name) {

          if (current_openslide_info$level_tile_count[location_tile[3] + current_openslide_info$level_calculate + 1, 1] >= location_tile[1] + 1 &    current_openslide_info$level_tile_count[location_tile[3] + current_openslide_info$level_calculate + 1, 2] >= location_tile[2] + 1) {
		  
              tile_region <- current_openslide_info$openslide_generator$get_tile(level = (location_tile[3] + current_openslide_info$level_calculate ) %>% as.integer(),address = c(location_tile[1],location_tile[2]) %>% as.integer())	
			  
			  
            if (input$Slide_viewer_option == "JPG") {
			
			  jpeg_image_buffer = IO$BytesIO()
              tile_region$save(jpeg_image_buffer, format="JPEG")
              session$sendCustomMessage(type = coor_data, 
                                        message = jpeg_image_buffer$getvalue() %>% BASE64$encodebytes() %>% as.character()
              )
            }
			
			###
			   if (input$Slide_viewer_option == "RAW") {

			      tile_region <- tile_region$convert(mode = "RGB")$transpose(as.integer(5))
                  blank_image <- PIL$Image$new("RGB", size = c(1024,1024) %>% as.integer(),color= "white")
                  blank_image$paste(im = tile_region)  
				  picture_rgb <- np$asarray(blank_image)

                  session$sendBinaryMessage(type = coor_data, 
                                        message = as.raw(picture_rgb)
                 )
			  }
			 ###
          }
        } else {
          current_openslide_info$openslide_name <- slide_name
          current_slide_in_openslide <- openslide$OpenSlide(slide_name)
          current_openslide_info$openslide_generator <- openslide$deepzoom$DeepZoomGenerator(osr = current_slide_in_openslide,
                                                                                             tile_size = as.integer(1024), 
                                                                                             overlap = as.integer(0))
		  
		  current_openslide_info$level_tile_count <- current_openslide_info$openslide_generator$level_tiles %>% unlist() %>% matrix(nrow = 2)

	      current_openslide_info$level_tile_count <- as.data.frame(current_openslide_info$level_tile_count)%>% transpose()
          current_openslide_info$level_calculate <- nrow(current_openslide_info$level_tile_count) - 13
		  
		  
          if (current_openslide_info$level_tile_count[location_tile[3] + current_openslide_info$level_calculate + 1, 1] >= location_tile[1] + 1 &    current_openslide_info$level_tile_count[location_tile[3] + current_openslide_info$level_calculate + 1, 2] >= location_tile[2] + 1) {

            tile_region <- current_openslide_info$openslide_generator$get_tile(level = (location_tile[3] + current_openslide_info$level_calculate) %>% as.integer(),address = c(location_tile[1],location_tile[2]) %>% as.integer())
            
            if (input$Slide_viewer_option == "JPG") {
			  jpeg_image_buffer = IO$BytesIO()
              tile_region$save(jpeg_image_buffer, format="JPEG")
			  
              session$sendCustomMessage(type = coor_data, 
                                        message = jpeg_image_buffer$getvalue() %>% BASE64$encodebytes() %>% as.character()
              ) 
            }
          }
        }
      }
    } else if (str_detect(slide_name, pattern = ".isyntax")) {
      
    }
  }
 
  #session$sendCustomMessage(type = coor_data,
  #                          message = coor_data)
})


current_slide_size_info <- reactiveValues(x_value = 0,
                                          y_value = 0,
                                          z_value = 0)

current_openslide_info <- reactiveValues(
  openslide_name = 0,
  openslide_generator = 0,
  level_tile_count = 0,
  level_calculate = 0
)


##################################################################

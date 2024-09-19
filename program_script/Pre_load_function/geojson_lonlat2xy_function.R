geojson_lonlat2xy_function <- function(lonlat_geojson) {
  if (lonlat_geojson$geometry$type == "Polygon") {
    if (lonlat_geojson$geometry$coordinates %>% class() == "list") {
      for (p_list in 1:length(lonlat_geojson$geometry$coordinates)) {
        origin_location <- lonlat_geojson$geometry$coordinates[[p_list]]
        origin_location <- LonLat2XY(lon_deg = origin_location[,1],
                                     lat_deg = origin_location[,2],
                                     zoom = 12 + 1)
        lonlat_geojson$geometry$coordinates[[p_list]] <- origin_location %>% as.matrix()
      }
      return(lonlat_geojson)
    } else if (lonlat_geojson$geometry$coordinates %>% class() == "array") {
      for (p_list in 1:(dim(lonlat_geojson$geometry$coordinates)[1])) {
        origin_location <- lonlat_geojson$geometry$coordinates[p_list,,]
        origin_location <- LonLat2XY(lon_deg = origin_location[,1],
                                     lat_deg = origin_location[,2],
                                     zoom = 12 + 1)
        lonlat_geojson$geometry$coordinates[p_list,,] <- origin_location %>% as.matrix()
      }
      return(lonlat_geojson)
    }
  } else if (lonlat_geojson$geometry$type == "MultiPolygon") {
    for (m_list in 1:length(lonlat_geojson$geometry$coordinates)) {
      if (lonlat_geojson$geometry$coordinates[[m_list]] %>% class() == "list") {
        for (p_list in 1:length(lonlat_geojson$geometry$coordinates[[m_list]])) {
          origin_location <- lonlat_geojson$geometry$coordinates[[m_list]][[p_list]]
          origin_location <- LonLat2XY(lon_deg = origin_location[,1],
                                       lat_deg = origin_location[,2],
                                       zoom = 12 + 1)
          lonlat_geojson$geometry$coordinates[[m_list]][[p_list]] <- origin_location %>% as.matrix()
        }
        return(lonlat_geojson)
      } else if (lonlat_geojson$geometry$coordinates[[m_list]] %>% class() == "array") {
        for (p_list in 1:(dim(lonlat_geojson$geometry$coordinates[[m_list]])[1])) {
          origin_location <- lonlat_geojson$geometry$coordinates[[m_list]][p_list,,]
          origin_location <- LonLat2XY(lon_deg = origin_location[,1],
                                       lat_deg = origin_location[,2],
                                       zoom = 12 + 1)
          lonlat_geojson$geometry$coordinates[[m_list]][p_list,,] <- origin_location %>% as.matrix()
        }
        return(lonlat_geojson)
      }
    } 
  }
}
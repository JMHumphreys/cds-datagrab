calculate_bounding_box <- function(df, buffer_miles) {
  buffer_degrees <- buffer_miles / 69 
  
  min_lat <- min(df$coord_x) - buffer_degrees
  max_lat <- max(df$coord_x) + buffer_degrees
  min_lon <- min(df$coord_y) - buffer_degrees
  max_lon <- max(df$coord_y) + buffer_degrees
  
  return(list(min_lon = min_lon, max_lon = max_lon, min_lat = min_lat, max_lat = max_lat))
}
### function for the calculation of the maximum distances in groups of migration and stationary

block_size_fun <- function(x) {
  bbox <- st_bbox(x$geometry)
  rec_dist_geom <- x %>% 
    filter(Long < bbox$xmin+0.001 | Long > bbox$xmax-0.001 | Lat < bbox$ymin+0.001 | Lat > bbox$ymax-0.001)
  # maximum migration distance (in km)
  max_migr_dst <- max(geodist(x = rec_dist_geom %>% select("Long", "Lat"), measure = "cheap", paired = TRUE, quiet = TRUE))/1000
  return(max_migr_dst)
}
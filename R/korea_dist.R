#' Distances in kilometers between centroids of Korean administrative regions
#'
#' Distance matrix of kilometers between centroids of first level administrative regions of South Korea.
#' 
#' Data derived in R using the GADM shape files (downloaded using raster::getData), sf::st_centroid to calculate region centroids, and geosphere::distm to calculate great circle distance between centroid points
"korea_dist"
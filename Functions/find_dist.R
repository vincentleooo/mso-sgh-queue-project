find_dist <- function(data, dist) {
  data <- na.omit(data)
  data <- as.integer(data)
  
  dist_data <- fitdistrplus::fitdist(data, dist)
  plot(dist_data)
  list(gofstat(dist_data), summary(dist_data))
}
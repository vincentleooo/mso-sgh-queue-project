find_dist <- function(data, dist, method) {
  data <- na.omit(data)
  data <- as.integer(data)
  
  dist_data <- fitdistrplus::fitdist(data, dist, method = method)
  plot(dist_data)
  list(gofstat(dist_data), summary(dist_data))
}
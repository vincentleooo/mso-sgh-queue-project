chi_squared_gamma <- function(df, data, breaks, shape, rate) {
  gamma_cdf <- c()
  
  # mu <- log(consult_mean^2/sqrt(consult_mean^2 + consult_sd^2))
  # sigma <- log(1 + consult_sd^2 / consult_mean^2)
  
  df %>% 
    mutate(
      Group = cut(data, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE)
    ) -> df
  
  for (i in 2:length(breaks)) {
    if (i == length(breaks)) {
      gamma_cdf <- c(gamma_cdf, 1-sum(gamma_cdf))
    } else {
      gamma_cdf <- c(gamma_cdf, 
                   pgamma(breaks[i], shape = shape, rate = rate) -
                     pgamma(breaks[i-1], shape = shape, rate = rate))
    }
  }
  
  summary_data <- summary(df$Group)
  
  expected <- gamma_cdf * sum(summary_data)
  observed <- summary_data
  
  error <- (observed-expected)^2 / expected
  
  critical_value <- qchisq(1-0.05, length(breaks)-1-3)
  
  return_df <- dplyr::tibble(
    Pass = if_else(sum(error) <= critical_value, "Not rejected", "Rejected"),
    Error = sum(error),
    `Critical Value` = critical_value
  )
  
  return(return_df)
}

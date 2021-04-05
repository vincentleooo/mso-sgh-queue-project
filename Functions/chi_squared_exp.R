chi_squared_exp <- function(df, data, breaks, rate) {
  exp_cdf <- c()
  
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
      exp_cdf <- c(exp_cdf, 1-sum(exp_cdf))
    } else {
    exp_cdf <- c(exp_cdf, 
                 pexp(breaks[i], rate = rate) -
                   pexp(breaks[i-1], rate = rate))
    }
  }
  
  summary_data <- summary(df$Group)

  expected <- exp_cdf * sum(summary_data)
  observed <- summary_data
  
  error <- (observed-expected)^2 / expected
  
  critical_value <- qchisq(1-0.05, length(breaks)-1-2)
  
  return_df <- dplyr::tibble(
    Pass = if_else(sum(error) <= critical_value, "Not rejected", "Rejected"),
    Error = sum(error),
    `Critical Value` = critical_value
  )

  return(return_df)
}

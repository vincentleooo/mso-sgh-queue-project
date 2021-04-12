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
  
  return_df <- kable(return_df, caption = paste("Chi-Squared Test for Gamma Distribution with Shape",shape,"and Rate",rate))
  return(return_df)
}

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
  
  return_df <- kable(return_df, caption = paste("Chi-Squared Test for Exponential Distribution with Rate",rate))
  
  return(return_df)
}

chi_squared_lnorm <- function(df, data, breaks, meanlog, sdlog) {
  lnorm_cdf <- c()
  
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
      lnorm_cdf <- c(lnorm_cdf, 1-sum(lnorm_cdf))
    } else {
      lnorm_cdf <- c(lnorm_cdf, 
                     plnorm(breaks[i], meanlog = meanlog, sdlog = sdlog) -
                       plnorm(breaks[i-1], meanlog = meanlog, sdlog = sdlog))
    }
  }
  
  summary_data <- summary(df$Group)
  
  expected <- lnorm_cdf * sum(summary_data)
  observed <- summary_data
  
  error <- (observed-expected)^2 / expected
  
  critical_value <- qchisq(1-0.05, length(breaks)-1-2)
  
  return_df <- dplyr::tibble(
    Pass = if_else(sum(error) <= critical_value, "Not rejected", "Rejected"),
    Error = sum(error),
    `Critical Value` = critical_value
  )
  
  return_df <- kable(return_df, caption = paste("Chi-Squared Test for Lognormal Distribution with MeanLog",meanlog,"and SdLog",sdlog))
  return(return_df)
}

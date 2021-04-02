histogram_maker <- function(df) {
  df1 <- df %>% 
    filter(PAC_consult_duration >= 15) %>% 
    mutate(PAC_consult_duration = PAC_consult_duration - 15)
  
  a <- ggplot(df) + geom_histogram(aes(PAC_to_listing_days))
  b <- ggplot(df1) + geom_histogram(aes(PAC_consult_duration))
  c <- ggplot(df) + geom_histogram(aes(PAC_duration_total))
  d <- ggplot(df) + geom_histogram(aes(surgery_lead_time))
  
  return((a+b)/(c+d))
}
cf_graph <- function(df) {
  duration <- df$PAC_duration_total
  duration <- na.omit(duration)
  duration <- as.integer(duration)
  list(
    PAC_to_listing_days = descdist(as.integer(na.omit(df$PAC_to_listing_days)), boot = 500),
    PAC_consult_duration = descdist(as.integer(na.omit(df$PAC_consult_duration)), boot = 500),
    PAC_duration_total = descdist(as.integer(duration),boot = 500),
    surgery_lead_time = descdist(as.integer(na.omit(df$surgery_lead_time)), boot = 500)
  )
  
}
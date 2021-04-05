#' Initial cleaner for PAC data
#'
#' Combines date and time, renames variables and changing variable types
#' to proper ones.
#' 
#' @param file_name The string file name of the PAC data based on year and type
#' 
#' @return The cleaned tibble of the PAC data
#' 
#' @example PAC_initial_cleaner("2019_planned.csv")
PAC_initial_cleaner <- function(file_name) {
  raw_data <- read_csv(file_name)
  
  new_data <- raw_data %>% 
    rename(
      PAC_date = `PAC Date`,
      visit_time = `Visit Time`,
      identifier = Identifier,
      age = Age,
      visit_dept = `Visit dept. OU`,
      procedure_code = `Procedure Code`,
      attended = `No Show / Attended`,
      listing_date = `Listing Date`,
      PAC_to_listing_days = `No. of days between PAC date and listing date`,
      surgery_date = `Surgery Date`,
      surgery_lead_time = `Lead time to Surgery`,
      admit_specialty = `Admit Specialty`,
      asa = `ASA Score`,
      PAC_register_time = `PAC Registration Time`,
      PAC_end_time = `PAC End Time`,
      PAC_consult_duration = `PAC Consult Duration`,
      TOSP_code = `TOSP Table Code`,
      surgery_complexity = `Complexity of Surgery`
    ) %>% 
    mutate(
      PAC_assigned_date_time = dmy_hms(paste(PAC_date, visit_time)),
      listing_date = dmy(listing_date),
      PAC_to_listing_days = as.integer(PAC_to_listing_days),
      surgery_date = dmy(surgery_date),
      surgery_lead_time = as.integer(surgery_lead_time),
      PAC_register_date_time = dmy_hms(paste(PAC_date, PAC_register_time)),
      PAC_end_date_time = dmy_hms(paste(PAC_date, PAC_end_time)),
      PAC_consult_duration = as.integer(PAC_consult_duration),
      age = as.integer(age),
      arrival_interval = PAC_register_date_time - lag(PAC_register_date_time)
    ) %>% 
    arrange(PAC_register_date_time) %>% 
    mutate(
      PAC_duration_total = PAC_end_date_time - PAC_register_date_time,
      arrival_interval = PAC_register_date_time - lag(PAC_register_date_time)
    ) %>% 
    dplyr::select(
      identifier,
      age,
      asa,
      listing_date,
      attended,
      PAC_to_listing_days,
      PAC_assigned_date_time,
      PAC_register_date_time,
      PAC_end_date_time,
      arrival_interval,
      PAC_consult_duration,
      PAC_duration_total,
      visit_dept,
      procedure_code,
      admit_specialty,
      surgery_lead_time,
      surgery_date,
      TOSP_code,
      surgery_complexity
    ) %>% 
    filter(
      PAC_to_listing_days >= 0,
      PAC_to_listing_days <= 40000,
      surgery_lead_time <= 40000
    )
  
  
  return(new_data)
}
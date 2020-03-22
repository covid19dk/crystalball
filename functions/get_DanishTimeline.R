get_DanishTimeline <- function(){
  
  ssi_hospitalized <- jsonlite::fromJSON("https://api.covid19data.dk:443/ssi_hospitalized") %>% as.data.table()
  ssi_cases_ts <- jsonlite::fromJSON("https://api.covid19data.dk:443/ssi_cases_ts") %>% as.data.table()
  

  # ssi_hospitalized --------------------------------------------------------
  
  # Remove row that contains row-class values
  ssi_hospitalized <- ssi_hospitalized[!(timestamp == "string" | region == "string")]
  ssi_hospitalized[, timestamp := as.POSIXct(timestamp)]
  
  # Only select on timestamp a day (if the future will bring more)
  ssi_hospitalized <- ssi_hospitalized[format(timestamp, "%H") == "08"]
  
  # Convert to date
  ssi_hospitalized[, timestamp := as.Date(timestamp)]
  
  # Sum by timestamp
  ssi_hospitalized <- 
    ssi_hospitalized[, .(hospitalized_total = sum(hospitalized, na.rm = TRUE),
                         critical_total = sum(critical, na.rm = TRUE),
                         respirator_total  = sum(respirator, na.rm = TRUE)),
                     by = "timestamp"]
  
  # Get daily counts
  ssi_hospitalized[, hospitalized_daily := c(hospitalized_total[1], diff(hospitalized_total))]
  ssi_hospitalized[, critical_daily := c(critical_total[1], diff(critical_total))]
  ssi_hospitalized[, respirator_daily := c(respirator_total[1], diff(respirator_total))]
  
  
  
  # ssi_cases_ts ------------------------------------------------------------
  
  # Convert to date
  ssi_cases_ts[, test_date := as.Date(test_date)]
  
  # Remove notes col
  ssi_cases_ts <- ssi_cases_ts[,-"notes"]
  
  # Rename
  setnames(ssi_cases_ts, c("cases", "tests"), c("cases_daily", "tests_daily"))
  
  # Make total cols
  ssi_cases_ts[, cases_total := cumsum(cases_daily)] 
  ssi_cases_ts[, tests_total := cumsum(tests_daily)] 
  

  # get_DanishTimeline -------------------------------------------------------

  # Merge data on date
  get_DanishTimeline <- merge(ssi_hospitalized, ssi_cases_ts, by.x = "timestamp", by.y = "test_date", all = TRUE)

  return(get_DanishTimeline)
  
}
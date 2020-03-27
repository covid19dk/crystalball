get_JohnHopkins <- function(){
  
  JohnHopkins <- jsonlite::read_json("https://api.covid19data.dk/john_hopkins_data")
  
  #JohnHopkins <- rbindlist(JohnHopkins, fill = TRUE, use.names = TRUE)

  JohnHopkins[, dates := as.IDate(dates, "%Y-%m-%d")]
  
  JohnHopkinsSum <- 
    JohnHopkins[, .(
      confirmed = sum(confirmed, na.rm = TRUE),
      deaths = sum(deaths, na.rm = TRUE),
      recovered = sum(recovered, na.rm = TRUE)),
      by = c("country_region", "dates")]
  
  # worldometers <- 
  #   jsonlite::read_json("https://api.covid19data.dk/worldometers_overview") %>%
  #   rbindlist(fill = TRUE, use.names = TRUE)
  
}


get_GlobalTimeline <- function(){
  
  GlobalTimeline <- jsonlite::read_json("https://thevirustracker.com/timeline/map-data.json")
  
  GlobalTimeline <- do.call(rbind,lapply(GlobalTimeline[[1]], function(x) do.call(data.table,x)))
  
  #GlobalTimeline <- rbindlist(GlobalTimeline)
  
  GlobalTimeline <- GlobalTimeline[!(countrycode %in% c("", " "))]
  
  GlobalTimeline[, date := as.IDate(date, "%m/%d/%y")]
  GlobalTimeline[, totalcases := as.integer(cases)]
  GlobalTimeline[, totalcases := as.integer(totalcases)]
  GlobalTimeline[, totaldeaths := as.integer(deaths)]
  GlobalTimeline[, totalrecovered := as.integer(recovered)]
  
  GlobalTimeline[, countrycode := as.factor(countrycode)]
  GlobalTimeline[, countrylabel := as.factor(countrycode)]
  
  setorder(GlobalTimeline, countrycode, date)
  
  return(GlobalTimeline)
  
}


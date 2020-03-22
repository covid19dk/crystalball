get_GlobalTimeline <- function(){
  
  GlobalTimeline <- jsonlite::read_json("https://thevirustracker.com/timeline/map-data.json")
  
  GlobalTimeline <- lapply(GlobalTimeline, function(x) cbind(data.table(date = x$date), rbindlist(x$data, use.names = TRUE, fill = TRUE)))
  
  GlobalTimeline <- rbindlist(GlobalTimeline)
  
  GlobalTimeline <- GlobalTimeline[!(countrycode %in% c("", " "))]
  
  GlobalTimeline[, date := as.IDate(date, "%m/%d/%y")]
  
  GlobalTimeline[, totalcases := as.integer(totalcases)]
  GlobalTimeline[, totaldeaths := as.integer(totaldeaths)]
  GlobalTimeline[, totalrecovered := as.integer(totalrecovered)]
  
  GlobalTimeline[, countrycode := as.factor(countrycode)]
  GlobalTimeline[, countrylabel := as.factor(countrylabel)]
  
  return(GlobalTimeline)
  
}


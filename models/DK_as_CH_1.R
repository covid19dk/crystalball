library(magrittr)
library(data.table)
diff2 = function(x) {x=diff(x);c(x[1],x)}
list.files("./functions/",full.names = TRUE) %>%  sapply(source) %>% invisible()

s = read_sst()
a = get_DanishTimeline() 
g = get_GlobalTimeline()
h = g[g$countrycode=="CN",]

##data from https://www.worldometers.info/coronavirus/country/denmark/
# wo = wo_dk_deaths = data.table(
#   time = seq(as.Date("2020-03-11"),as.Date("2020-03-23"),by = "days"),
#   deaths_old = c(0,0,0,1,1,2,0,0,2,3,4,NA,NA),
#   deaths =     c(1,0,0,1,2,1,1,3,5,4,3,3,NA)
# )

png("model_with_latest_numbers.png",width = 1920,height = 1080)

plot(h$date,diff2(h$totaldeaths),log="y",col="grey",
     main="If DK lockdown follows pattern of CH's lockdown ", 
     xlab="Timeline, after CH lockdown",
     ylab ="deaths/hospitalizations")
points(h$date,frollmean(diff2(h$totaldeaths),n=7,align = "center"),type="l",col="grey")
points(
  (s$hospitalized_any$time-(52-1)*24*3600) %>% rev %>% as.Date(tz="CET"),
  (s$hospitalized_any$all %>% rev %>% diff2),col="green"
)

points(
  s$deaths$time %>% as.POSIXlt %>% (function(x) x-(52-1)*24*3600) %>% as.Date(tz="CET"),
  s$deaths$deaths,col="red",pch=19,cex=1.5  
)

points(h$date,frollmean(diff2(h$totaldeaths),n=7,align = "center")/9,type="l",col="red")
points(h$date,frollmean(diff2(h$totaldeaths),n=7,align = "center")/13,type="l",col="red")
points(h$date,frollmean(diff2(h$totaldeaths)[11:(nrow(h)+10)],n=7,align = "center")/3,type="l",col="green")
legend("bottomright",
  legend=c(
  "China, daily deaths",
  "CH rollmean width=7",
  "Denmark, daily deaths, 51days offset",
  "same CH rollmean divided by 9-13",
  "Denmark, daily hospitalizations, 51days offset",
  "same CH rollmean divided by 3, offset by 10 days ",
  "DK daily increase ICU, offset 51 days",
  "DK daily increase respirator, offset 51 days"
  ), 
  col=c("grey","grey","red","red","green","green","orange","#FF22AA"),
  #lwd = c(1,1,1,1,1,1),
  pch = c(1, NA,19,NA,1 ,NA,2 ,2 ),
  lty = c(NA,1 ,NA,1 ,NA,1 ,NA,NA),cex=.8
)

#hospitalized 7 days after infection
#die 14-19 days
#recover 2.5 week (1.5 week in hospital)
#science in

points(
  (s$hospitalized_icu$time-(52-1)*24*3600) %>% rev %>% as.Date(tz="CET"),
  (s$hospitalized_icu$all %>% rev %>% diff2),col="orange",pch=2,cex=.5
)
points(
  (s$hospitalized_resp$time-(52-1)*24*3600) %>% rev %>% as.Date(tz="CET"),
  (s$hospitalized_resp$all %>% rev %>% diff2),col="#FF22AA",pch=2,cex=.5
)


danish_deaths_13 =  sum(frollmean(diff2(h$totaldeaths),n=7,align = "center")/13,na.rm=TRUE)
danish_deaths_9  =  sum(frollmean(diff2(h$totaldeaths),n=7,align = "center")/9,na.rm=TRUE)

legend(
  "topleft",
  paste0(
  "redlines sums to ",
  danish_deaths_13 %>% round," - ",danish_deaths_9 %>% round,
  " deaths in DK during 1st wave given scenario holds (this is not a confidence interval)"),
  col="red",
  bty="n"
)

dev.off()




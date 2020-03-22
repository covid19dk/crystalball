library(data.table)
library(magrittr)
library(httr)
library(xml2)
library(rvest)
source("./sst_source.R")

#get sst data
res = httr::GET("https://www.sst.dk/da/corona/tal-og-overvaagning")
htmls = xml2::read_html(res)
sst.df.list = rvest::html_table(htmls,header=TRUE,dec="|")
sst.dt.list = sst.df.list %>% lapply(as.data.table)
s = sst.dt.list

#read table config file and tiy tables
conf = within(list(),source("./table_config.R",local = TRUE))

names(s) = names(conf$tables)
s2 = tidy_sst_tables(s,conf) 

#example plot
s2$hospitalized_any[plot(time,all,type="b",ylim=c(0,max(all)))]
s2$hospitalized_any[points(time,reg_HS,type="b",col=2)]
s2$hospitalized_icu[points(time,reg_HS,type="b",col=3)]
legend("topleft",leg=c("all DK","only CPH","only CPH - only ICU"),col=1:3,lwd=1)

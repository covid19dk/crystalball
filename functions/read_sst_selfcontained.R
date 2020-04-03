
library(data.table)
library(magrittr)
library(httr)
library(xml2)
library(rvest)

read_sst = function() {
  tidy_sst_tables = function(s,conf) {
    
    #iterator tidying tables as defined in config
    s2 = lapply(seq_along(s), function(i){
      print(i)
      dt = s[[i]]
      cfg = conf$tables[[i]]
      if(!is.null(cfg$droprows)) dt = dt[-cfg$droprows]
      if(!is.null(cfg$droprows_rev)) dt=dt[(nrow(dt):1)[-cfg$droprows_rev]]
      
      colnames(dt) = cfg$colnames
      dt[] = lapply(seq_len(ncol(dt)),function(j_col) {
        as_type_func = (conf$types[[cfg$coltypes[j_col]]])
        converted_column = as_type_func(dt[[j_col]])
        return(converted_column)
      })
      return(dt)
    })
    names(s2) = names(conf$tables)
    
    return(s2)
  }
  
  #the configuration
  conf = list(
    tables =list(
      overall = list(
        colnames=c("where","tested","positives","remission","deaths","deaths_pct"),
        coltypes=c("reg2" ,"int"   ,"int"      ,"int","int"   , "pct"      )
      ),
      # deaths= list(
      #   colnames=c("time","deaths"),
      #   coltypes =c("ts1","int"),
      #   droprows_rev = c(1)
      # ),
      
      alders_fordeling_test= list(
        colnames=c("agegroup","total","h_females","h_males","h_total","h_subratio","h_subillness","h_subillness_raio"),
        coltypes=c("str"     ,"int"      ,"int"    ,"int"  ,"int"    ,"int"       ,"int"         ,"int"),
        droprows = 1
      ),
      
      tests = list(
        colnames = c("time","positives","tested","positve_pct"),
        coltypes = c("ts1" ,"int"    ,"int",    "pct"),
        droprows_rev =c(1) 
      ),
      hospitalized_region = list(
        colnames = c("region","all","icu","resp"),
        coltypes = c("reg1"  ,"int","int","int" )
      ),
      hospitalized_any = list(
        colnames=c("time","reg_NJ","reg_MJ","reg_SD","reg_HS","reg_SJ","all"),
        coltypes=c("ts1", "int"   ,"int"   ,"int"   , "int"   ,"int"   ,"int"),
        droprows = 1
      ),
      hospitalized_icu = list(
        colnames=c("time","reg_NJ","reg_MJ","reg_SD","reg_HS","reg_SJ","all"),
        coltypes=c("ts1", "int"   ,"int"   ,"int"   , "int"   ,"int"   ,"int"),
        droprows = 1
      ),
      hospitalized_resp= list(
        colnames=c("time","reg_NJ","reg_MJ","reg_SD","reg_HS","reg_SJ","all"),
        coltypes=c("ts1", "int"   ,"int"   ,"int"   , "int"   ,"int"   ,"int"),
        droprows = 1
      ),
      
      alders_fordeling= list(
        colnames=c("agegroup","total","h_females","h_males","h_total","h_subratio","h_subillness","h_subillness_raio"),
        coltypes=c("str"     ,"int"      ,"int"    ,"int"  ,"int"    ,"int"       ,"int"         ,"int"),
        droprows = 1
      ),

      # where_infected= list(
      #   colnames = c("country"  ,"positives"),
      #   coltypes = c("cou1" ,"int")
      # )
      
      alders_fordeling= list(
        colnames=c("week_no","positives","remission","remission_pct", "still_hospitalized","still_hospitalized_ratio","deaths","deaths_ratio"),
        coltypes=c("str"     ,"int"      ,"int"     ,"pct"          , "int"               ,"pct"                     ,"int"   ,"pct"),
        droprows_rev = c(1)
      )
      
    ),
    
    types = list(
      int = function(x) {
        if(is.character(x)) {
          x %<>% gsub(pat="\\.",repl="") %>% gsub(pat=",",repl=".") %>% gsub(pat="[^0-9.-]", repl="",)
          x[nchar(x)==1 & x=="-"] = NA
        } else {
          if(!is.numeric(x)) stop("int type not recognized")
        } 
        return(as.numeric(x))
      },
      
      str = function(x) as.character(x),
      
      ts1 = function(x) {
        dkmonths = c("januar","februar","marts","april","maj","juni","juli","august","september","oktober","november","december")
        dkmnum = formatC(1:12,width = 2,flag="0")
        for(i in seq_along(dkmonths)) x %<>% gsub(pat=dkmonths[i],repl=dkmnum[i]) 
        Time = as.POSIXct(x,format="%d. %m")
        return(Time)
      },
      
      reg1 = function(x) {
        regs = c("Region Nordjylland", "Region Midtjylland", "Region Syddanmark", 
                 "Region Hovedstaden", "Region Sjælland", "Hele landet")
        regsshort = c("reg_NJ","reg_MJ","reg_SD","reg_HS","reg_SJ","all")
        for(i in seq_along(regs)) x %<>% gsub(pat=regs[i],repl=regsshort[i]) 
      },
      
      reg2 = function(x) {
        regs = c("Danmark","Færøerne","EU,EØS og UK", "Globalt" )
        regsshort = c("DK","FI","EU","global")
        for(i in seq_along(regs)) x %<>% gsub(pat=regs[i],repl=regsshort[i]) 
        x
      },
      
      cou1 = function(x) {
        x
      }
    )
  )
  conf$types$pct = function(x) {conf$types$int(x)}
  
  
  #get sst data
  res = httr::GET("https://www.sst.dk/da/corona/tal-og-overvaagning")
  htmls = xml2::read_html(res)
  sst.df.list = rvest::html_table(htmls,header=TRUE,dec="|")
  sst.dt.list = sst.df.list %>% lapply(as.data.table)
  s = sst.dt.list
  names(s) = names(conf$tables)
  s2 = tidy_sst_tables(s,conf) 
  
  return(s2)
}

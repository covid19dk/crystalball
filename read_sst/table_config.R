tables =list(
  overall = list(
    colnames=c("where","tested","positives","deaths","deaths_pct"),
    coltypes=c("reg2" ,"int"   ,"int"      ,"int"   , "pct"      )
  ),
  tests = list(
    colnames = c("time","tested","positives","positve_pct"),
    coltypes = c("ts1" ,"int"     ,"int",    "pct")
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
  where_infected= list(
    colnames = c("country"  ,"positives"),
    coltypes = c("cou1" ,"int")
  )
)

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
types$pct = function(x) {types$int(x)}



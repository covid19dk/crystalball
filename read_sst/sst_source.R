
#' tidy sst tables
#'
#' @param s "https://www.sst.dk/da/corona/tal-og-overvaagning"
#' @param conf config list as sourced by ./table_config.R
#'
#' @return same struc as s, but new colnames and interpreted types
#' @export
#'
#' @examples
tidy_sst_tables = function(s,conf) {
  
  s2 = lapply(seq_along(s), function(i){
    dt = s[[i]]
    cfg = conf$tables[[i]]
    if(!is.null(cfg$droprows)) dt = dt[-cfg$droprows,]
    
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

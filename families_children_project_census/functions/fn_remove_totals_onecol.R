

remove_totals_onecol <- function(in_vector, total_names){
  
  pattern <- paste(total_names, collapse = "|")
  
  totals <- grepl(pattern, in_vector, ignore.case = TRUE)
  
  to_keep <- !(totals)
  
  out_vector <- in_vector[to_keep]
  
  return(out_vector)
  
}
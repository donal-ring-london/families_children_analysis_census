## function to remove all totals in a dataset
## there's a for loop here. Would be nice to remove it. 

remove_totals <- function(dataset, total_names){
  
  cols <- colnames(dataset)
  
  pattern <- paste(total_names, collapse = "|")
  
  for(i in 1:length(cols)){
    
    col_name <- cols[i]
    
    totals <- grepl(pattern, unlist(dataset[,..col_name]), ignore.case = TRUE)
    
    to_keep <- !(totals)
    
    dataset <- dataset[to_keep,]
    
  }
  
  return(dataset)
  
}

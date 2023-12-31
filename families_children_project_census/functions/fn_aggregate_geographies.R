
aggregate_geographies <- function(data, lookup,
                                  geog_from_data, geog_from_lookup,
                                  geog_to_lookup,count_name){
  
  ## making sure everything is in a data.table
  data <- data.table(data)
  lookup <- data.table(lookup)
  
  ## in the lookup, only keeping geography columns that we're using
  look_keep <- c(geog_from_lookup, geog_to_lookup)
  lookup <- lookup[,..look_keep]
  
  
  ## joining the dataset with the geographical lookup
  setkeyv(data,geog_from_data)
  
  setkeyv(lookup,geog_from_lookup)
  
  data <- lookup[data]
  
  data <- data[,-..geog_from_lookup]
  
  ## the final aggregation, from lower geographies to higher geographies
  to_rem <- c(count_name) # name of the count variable
      
  by_cols <- colnames(data)[!(colnames(data) %in% to_rem)] # getting a vector of columns to aggregate over. This is just all of the columns other than the count column(s). 
      
  data <- data[,lapply(.SD, sum, na.rm = TRUE), # applying sum function to each column specified in SD cols
                   .SDcols = c(count_name), # setting SDcols. In this case, only the count column(s)
                   by = by_cols] # the columns that we want to aggregate over/by. 
      
  return(data)
  
}

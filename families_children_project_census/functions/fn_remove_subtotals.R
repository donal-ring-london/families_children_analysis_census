
remove_sub_totals <- function(dataset, var_name){
  
  ### A quick note on the general approach to removing subtotals in this function. All subtotals in Census categories are defined by a ":" - e.g. "United Kingdom: England". 
  ### In this case, as well as there being values for England, Scotland...etc there will be a subtotal value for "United Kingdom". This is the subtotal we want to remove. 
  ### Using the fact that subtotals are marked out by a ":", we split the categorical vector on the colons and remove entries that contain values that appear only before the colon. 
  
  
  ## getting the unique values in the input vector
  input_vector <- unlist(dataset[,..var_name])
  
  input_vector_unique <- unique(input_vector)
  
  ## extracting the names of the sub-total
  split_vec <- strsplit(input_vector_unique, split = ":") # splitting on the colon # NOTE - I changed this from ": " to ":". If any errors shows up suddenly, it could be this. But it should be fine. 
  
  split_vec_lengths <- unlist(lapply(split_vec, length)) # getting the length of each element
  
  split_vec <- split_vec[which(split_vec_lengths > 1)] # extracting categories that have more than one entry. That's because these are the ones that show which will have sub totals. 
  
  split_vec <- unique(lapply(split_vec, function(x){x[-length(x)]})) # getting rid of the last element (because we don't want to remove that, only the levels before), then getting the unique values.
  
  split_vec <- lapply(split_vec, function(x){paste0(x, collapse = ": ")}) # pasting any elements which have more than one entry together with a ": ". This is because some sub-totals will still have two tiers, e.g. "Europe: EU countries"
  
  sub_totals <- unlist(split_vec)
  
  
  ## removing rows with sub-totals
  to_keep <- !(input_vector %in% sub_totals) # only keeping those values that are not sub-totals
  
  output_dataset <- dataset[to_keep,]
  
  return(output_dataset)
  
  }





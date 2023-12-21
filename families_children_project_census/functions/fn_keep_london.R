## function to only keep london local authorities in a dataset that has all local authorities (could just narrow to all that have an E09 GSS code...)

lond_keep <- function(data, geog_name){
  
  ## creating a vector with all London local authorities
  lond <- c("E09000001","E09000002","E09000003","E09000004","E09000005","E09000006","E09000007","E09000008","E09000009","E09000010","E09000011",
            "E09000012","E09000013","E09000014","E09000015","E09000016","E09000017","E09000018","E09000019","E09000020","E09000021","E09000022",
            "E09000023","E09000024","E09000025","E09000026","E09000027","E09000028","E09000029","E09000030","E09000031","E09000032","E09000033")
  
  ## creating a logical vector - true if the row is a London local authority, false if it is not
  lond_cond <- unlist(data[,..geog_name]) %in% lond
  
  ## filtering the dataset by this logical vector
  data <- data[lond_cond,]
  
  return(data)
  
}


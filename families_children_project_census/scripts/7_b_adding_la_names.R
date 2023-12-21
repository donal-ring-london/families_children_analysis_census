## quick script to add local authority names to all of the existing datasets that have them. Before running this script they only have codes. 
## This script really shouldn't exist, and local authority names and codes should be added in a previous script, like when I read them in or clean them, or aggregate to inner/outer. Fix this later. 


## 0. libraries and functions, and any other bits e.g. lookups

library(data.table)

source("functions/fn_aggregate_geographies.R")
source("functions/fn_aggregate_geographies_2.R")

inner_outer_lookup <- fread("data_resources/inner_outer_lookup.csv") # the inner/outer lookup contains both local authority names and codes, so using this as the lookup. 


## 1. reading in the datasets

list.files("//profileshare1/rdrfoldersA-D/DRing/Desktop/families_analysis_initial/data/category_matched_at_la")

files_to_read <- c("ftb_cob_age_sex_21_dc2103_11.csv", "ftb_hhcomp_21_qs118_11_uv006_01.csv", 
                   "ftb_hhcomp_ethgroup_21_dc1201_11_st106_01.csv","ftb_hhcomp_nssec_21_dc6115_11_st044_01.csv",
                   "rm006_21_dc1113_11_st014_01.csv", "rm132_21_LC4412_11_st053_01.csv",
                   "ftb_cob_age_sex_21_dc2103_11_st015_01.csv",
                   "ftb_hhcomp_ethgroup_21_dc1201_11_st106_01_without.csv", 
                   "ftb_hhcomp_nssec_21_dc6115_11_st044_01_without.csv",
                   "rm132_21_LC4412_11_st053_01_without.csv")

paths_to_read <- paste0("data/category_matched_at_la/", files_to_read)


datasets_to_process <- lapply(
  X = paths_to_read,
  FUN = fread
)


## 2. extracting the geography names from the datasets, as they are needed for the function

geography_names <- lapply(
  X = datasets_to_process,
  FUN = function(x){return(colnames(x)[1])}
)

geography_names <- unlist(geography_names)


## 3. adding the local authority names to each of the datasets

la_names_lookup <- inner_outer_lookup[,c("lad22cd", "lad22nm")]

  ### defining the function that will just add names onto datasets that have codes
  add_la_names <- function(dataset, geography_name, lookup, lookup_geog_name){
  
  ## setting the keys to join on
  setkeyv(dataset, geography_name)
  setkeyv(lookup, lookup_geog_name)
  
  ## joining the datasets
  data_with_names <- lookup[dataset]
  
  ## returning the final dataset from the function
  return(data_with_names)
  
}

la_names_lookup_list <- list(la_names_lookup)

  ### applying the function to each dataset. The output is a list of datasets that now have local authority names
datasets_with_names <- mapply(
  FUN = add_la_names,
  dataset = datasets_to_process,
  geography_name = geography_names,
  lookup = la_names_lookup_list,
  lookup_geog_name = "lad22cd"
)


## 4. saving the datasets

files_to_write <- files_to_read

for(i in 1:length(datasets_with_names)){
  
  fwrite(
    x = datasets_with_names[[i]],
    file = paste0("data/category_matched_at_la/", files_to_write[i])
  )
  
}





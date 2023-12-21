## file to aggregated datasets from local authority level detail to inner/outer London detail

## 0. libraries and functions, and any other bits e.g. lookups

library(data.table)

source("functions/fn_aggregate_geographies.R")
source("functions/fn_aggregate_geographies_2.R")

inner_outer_lookup <- fread("data_resources/inner_outer_lookup.csv")


## 1. reading in the datasets

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



## 2. extracting the geography names for the columns, as they are needed for the aggregation function, and saving them in a character vector

geography_names <- lapply(
  X = datasets_to_process,
  FUN = function(x){return(colnames(x)[1])}
)

geography_names <- unlist(geography_names)


## 2b. extracting the value column names for the columns, as they are needed as an input into the function, and they're not the same across all datasets


count_name_columns <- lapply(
  X = datasets_to_process,
  FUN = function(x){return(colnames(x)[grep("value", colnames(x))])}
)



## 3. aggregating to inner/outer london

lookup_list <- list(inner_outer_lookup) # because a single data.table as an input will be read in a list...so will iterate over each element of the list as an input into the argument. And each element of a data.table list is a column, so it will iterate over each variable rather than including the lookup whole in each iteration. Putting the data.table into a list of length 1 gets over this problem. 

  ### getting rid of the local authority name column
datasets_to_process <- lapply(
                          X = datasets_to_process,
                          FUN = function(x){return(x[,-"lad22nm"])}
                                  )

  ### applying the aggregate_geographies_2 function to each dataset, which will aggregate the geography from local authority level to inner/ouer London (because this is the lookup we give it). 
datasets_inner_outer <- mapply(
      FUN = aggregate_geographies_2,
      data = datasets_to_process,
      lookup = lookup_list,
      geog_from_lookup = "lad22cd", 
      geog_to_lookup = "inner_outer",
      geog_from_data = geography_names,
      count_names = count_name_columns
    )


## 4. saving the final datasets


files_to_write <- files_to_read


for(i in 1:length(datasets_inner_outer)){
  
  fwrite(
    x = datasets_inner_outer[[i]],
    file = paste0("data/matched_inner_outer/", files_to_write[i])
  )
  
}




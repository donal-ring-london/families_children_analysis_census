
## 0. libraries and functions


source("functions/fn_get_data_from_nomis.R")

library(nomisr)
library(data.table)


## 1. reading in the data resources needed for the get_data_from_nomis_function

nomis_id_name_lookup <- fread("data_resources/nomis_id_name_lookup.csv") # to match Census table codes with the special nomisr table codes

geog_vec <- fread("data_resources/london_la_ids_2021.csv") # reading in local authority 2021 nomisr codes
geog_vec <- unlist(geog_vec)


## 2. reading in the datasets

### vectors of the names of the datasets to be read in from nomisr
datasets_to_read_in <- c("TS003", "RM132", "RM009", "RM012", "RM034", "RM006", "RM034", "RM095",
                         "RM006")


### reading in the mulivariate datasets
datasets_2021 <- lapply(
  X = datasets_to_read_in,
  FUN = get_data_from_nomis,
  geog_vec = geog_vec,
  id_name_lookup = nomis_id_name_lookup,
  year = "2021",
  multi = "yes" # one of them is univariate, but it doesn't matter because it's a 2021 dataset. It only matters if it's 2011 and univariate - those are the datasets with a different structure that needed slightly different code in the function to work
)

names(datasets_2021) <- datasets_to_read_in


## 3. saving the datasets

for(i in 1:length(datasets_2021)){
  
  fwrite(x = datasets_2021[[i]],
         file = paste0("data/2021/", datasets_to_read_in[i], ".csv"))
  
}




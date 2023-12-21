
## 0. libraries and functions

source("functions/fn_get_data_from_nomis.R")

library(nomisr)
library(data.table)


## 1. reading in the data resources needed for the get_data_from_nomis_function

nomis_id_name_lookup <- fread("data_resources/nomis_id_name_lookup.csv") # to match Census table codes with the special nomisr table codes

geog_vec <- fread("data_resources/london_la_ids_2001.csv") # reading in local authority 2021 nomisr codes
geog_vec <- unlist(geog_vec)


## 2. reading in the datasets

### vectors of the names of the datasets to be read in from nomisr
datasets_to_read_in <- c("UV006", "ST007 - Age of Family", "ST031", "ST015", "ST106", "ST044", "ST151",
                         "ST014",
                         "ST053") # extra info needed for ST007. This is because, for some reason, there are two tables from 2001 with the same code, ST007. If you just search for ST007 then two matches come up and the function can't handle that, so we need to add more information to narrow it to the table that we want. 


### reading in the mulivariate datasets
datasets_2001 <- lapply(
  X = datasets_to_read_in,
  FUN = get_data_from_nomis,
  geog_vec = geog_vec,
  id_name_lookup = nomis_id_name_lookup,
  year = "2001",
  multi = "yes" # one of them is univariate, but it doesn't matter because it's 2001. It only matters if it's 2011 and univariate - those are the datasets with a different structure that needed slightly different code in the function to work
)

names(datasets_2001) <- datasets_to_read_in


## 3. saving the datasets

for(i in 1:length(datasets_2001)){
  
  fwrite(x = datasets_2001[[i]],
         file = paste0("data/2001/", datasets_to_read_in[i], ".csv"))
  
}




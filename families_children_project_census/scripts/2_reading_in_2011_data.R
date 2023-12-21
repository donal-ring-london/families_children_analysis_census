
## 0. libraries and functions

source("functions/fn_get_data_from_nomis.R")

library(nomisr)
library(data.table)


## 1. reading in the data resources needed for the get_data_from_nomis_function

nomis_id_name_lookup <- fread("data_resources/nomis_id_name_lookup.csv") # to match Census table codes with the special nomisR table codes. 

geog_vec <- fread("data_resources/london_la_ids_2011.csv") # reading in local authority 2011 nomisr codes
geog_vec <- unlist(geog_vec)

geog_vec_merged <- fread("data_resources/london_merged_la_ids_2011.csv") # reading in merged local authority 2011 nomisr codes. One dataset was on merged local authority codes, which for London is the same except for the merging of the City and Westminster. But all with different codes. 
geog_vec_merged <- unlist(geog_vec_merged)


## 2. reading in the datasets

  ### vectors of the names of the datasets to be read in from nomisr
uni_datasets_to_read_in <- c("QS118EW")

multi_datasets_to_read_in <- c("DC1114EW", "DC1203EW", "LC1118EW", "LC1601EW", "LC4102EW", "DC2103EW", 
                               "DC1201EW", "DC6115EW", "DC1202EW",
                               "DC1113EW")

merged_multi_datasets_to_read_in <- c("DC1601EWla", "DC5104EWla")


  ### reading in the univariate datasets
univariates_2011 <- lapply(
    X = uni_datasets_to_read_in,
    FUN = get_data_from_nomis,
    geog_vec = geog_vec,
    id_name_lookup = nomis_id_name_lookup,
    year = "2011",
    multi = "no"
  )

names(univariates_2011) <- uni_datasets_to_read_in

  ### reading in the multivariate datasets
multivariates_2011 <- lapply(
  X = multi_datasets_to_read_in,
  FUN = get_data_from_nomis,
  geog_vec = geog_vec,
  id_name_lookup = nomis_id_name_lookup,
  year = "2011",
  multi = "yes"
)

names(multivariates_2011) <- multi_datasets_to_read_in


  ### reading in the multivariate datasets that are only available on merged local authority boundaries
merged_multivariates_2011 <- lapply(
  X = merged_multi_datasets_to_read_in,
  FUN = get_data_from_nomis,
  geog_vec = geog_vec_merged,
  id_name_lookup = nomis_id_name_lookup,
  year = "2011",
  multi = "yes"
)

names(merged_multivariates_2011) <- merged_multi_datasets_to_read_in


## 3. saving the datasets

for(i in 1:length(univariates_2011)){
  
  fwrite(x = univariates_2011[[i]],
         file = paste0("data/2011/", uni_datasets_to_read_in[i], ".csv"))
  
}


for(i in 1:length(multivariates_2011)){
  
  fwrite(x = multivariates_2011[[i]],
         file = paste0("data/2011/", multi_datasets_to_read_in[i], ".csv"))
  
}


for(i in 1:length(merged_multivariates_2011)){
  
  fwrite(x = merged_multivariates_2011[[i]],
         file = paste0("data/2011/", merged_multi_datasets_to_read_in[i], ".csv"))
  
}




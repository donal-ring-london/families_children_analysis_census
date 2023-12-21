
## matching household composition by tenure, for couple families without children

## this script will be different from the others. I already have this matched at from 2011 to 2021 at LSOA level. 
## So I need to read that dataset in, aggregate the geographies to local authority, and reformat it to the same sort of dataset as the other matched datasets

## then I need to add on 2001...in the same way as all of the other datasets


## 1. libraries and functions

library(data.table)



## 2. fixing up pre-matched 2011 - 2021 dataset

### reading in dataset
ten_hhcomp_21_11 <- fread("data/matched_elsewhere/RM132_LC4412EW.csv")


### adding the la information
la_lsoa_lookup <- fread("data_resources/2021_oa_lsoa_msoa_la.csv")

keep <- c("lsoa21cd", "lad22cd")

la_lsoa_lookup <- unique(la_lsoa_lookup[,..keep])

setkey(ten_hhcomp_21_11, "lsoa21cd") # setting the variable to join on

setkey(la_lsoa_lookup, "lsoa21cd") # setting the variable to join on

ten_hhcomp_21_11 <- la_lsoa_lookup[ten_hhcomp_21_11] # joining the two datasets


### aggregating to la 
ten_hhcomp_21_11 <- ten_hhcomp_21_11[, .(value_21 = sum(value_21), value_11 = sum(value_11)),
                                     by = list(lad22cd, dependent_children, tenure)]


### aggregating tenure. Final categories are "Owned", "Private rented or lives rent free", "Social rented"
mapping_21 <- data.table(
  cat_21 = c("Owned: Owns outright", "Owned: Owns with a mortgage or loan or shared ownership",
             "Private rented or lives rent free", "Rented: Social rented"),
  ten_common = c("Owned", "Owned", "Private rented or lives rent free", "Social rented")
)


setkey(mapping_21, "cat_21")

setkey(ten_hhcomp_21_11, "tenure")

ten_hhcomp_21_11 <- ten_hhcomp_21_11[mapping_21]

ten_hhcomp_21_11 <- ten_hhcomp_21_11[, .(value_21 = sum(value_21), value_11 = sum(value_11)),
                                     by = list(lad22cd, dependent_children, ten_common)]

### only keeping families without dependent children

ten_hhcomp_21_11 <- ten_hhcomp_21_11[dependent_children == "No dependent children in household",]


## 3. matching the 2001 dataset

st053_01 <- fread("data/2001/ ST053.csv")

unique(st053_01$c_tenhuk11_name)

unique(ten_hhcomp_21_11$tenure)


### only keeping families without dependent children
unique(st053_01$c_hhchuk11_name)

keep <- c("No children", "All children non-dependent")

st053_01 <- st053_01[c_hhchuk11_name %in% keep,]

st053_01$c_hhchuk11_name <- "No dependent children in household"

st053_01 <- st053_01[, .(obs_value = sum(obs_value)),
         by = list(date, geography_code, c_hhchuk11_name, c_tenhuk11_name, occrat_name)]

### matching tenure. Final categories are "Owned", "Private rented or lives rent free", "Social rented"

mapping_01 <- data.table(
  cat_01 = c("Owned", "Rented from council", "Other social rented", "Private rented or living rent free"),
  ten_common = c("Owned", "Social rented", "Social rented", "Private rented or lives rent free")
)

setkey(mapping_01, "cat_01")
setkey(st053_01, "c_tenhuk11_name")

st053_01_matched <- st053_01[mapping_01]

st053_01_matched <- st053_01_matched[, .(value_01 = sum(obs_value)),
                                     by = list(geography_code, c_hhchuk11_name, ten_common)]

sum(st053_01_matched[ten_common == "Private rented or lives rent free",]$value_01)

## 4. joining the datasets

colnames(ten_hhcomp_21_11)
colnames(st053_01_matched)

setkey(ten_hhcomp_21_11, "lad22cd", "ten_common")
setkey(st053_01_matched, "geography_code", "ten_common")

rm132_21_LC4412_11_st053_01 <- ten_hhcomp_21_11[st053_01_matched]

##### There is something really wrong with the 2001 dataset, and I can't quite figure it out. For the purposes of this analysis, I'm going to leave it and carry on with 2011 - 2021. 


## 5. writing the dataset

colnames(rm132_21_LC4412_11_st053_01)

col_ords <- c("lad22cd", "dependent_children", "ten_common", "value_21", "value_11", "value_01")

rm132_21_LC4412_11_st053_01 <- rm132_21_LC4412_11_st053_01[, ..col_ords]

fwrite(x = rm132_21_LC4412_11_st053_01,
       file = "data/category_matched_at_la/rm132_21_LC4412_11_st053_01_without.csv")


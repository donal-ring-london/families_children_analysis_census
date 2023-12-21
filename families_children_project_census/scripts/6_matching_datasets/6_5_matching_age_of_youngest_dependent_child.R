

setwd("//profileshare1/rdrfoldersA-D/DRing/Desktop/families_analysis_initial") # again, get rid of this once it's a project


## 1. libraries and functions

library(data.table)


## 2. Reading in the datasets

st014_01 <- fread("data/2001/ ST014.csv")

dc1113_11 <- fread("data/2011/ DC1113EW.csv")

rm006_21 <- fread("data/2021/ RM006.csv")


unique(st014_01$c_age_name)
unique(dc1113_11$c_dpchuk11_name)
unique(rm006_21$c2021_hh_depchild_6b_name)


unique(st014_01$c_age_name)
unique(st014_01$hhtype_name)

## 3. matching age of youngest dependent child. Final categories will be "Aged 0 to 4", "Aged 5 to 9", "Aged 10 to 15", "Aged 16 to 18"

### need to remove "all adults" from hhtype! This is messing up the numbers
st014_01 <- st014_01[hhtype_name != "All adults",]

unique(st014_01$hhtype_name)

  ### matching 2001
mapping_01 <- data.table(
  cat_01 = c("dependent children in household, youngest aged 0 to 4", "dependent children in household, youngest aged 5 to 9", 
             "dependent children in household, youngest aged 10 to 15", "dependent children in household, youngest aged 16 to 18"),
  age_common = c("Aged 0 to 4", "Aged 5 to 9", "Aged 10 to 15", "Aged 16 to 18")
)

setkey(mapping_01, "cat_01")
setkey(st014_01, "c_age_name")

st014_01_matched <- st014_01[mapping_01]

st014_01_matched <- st014_01_matched[, .(value = sum(obs_value)),
                                     by = list(geography_code, age_common)]


  ### matching 2011
mapping_11 <- data.table(
  cat_11 = c("Youngest dependent child: Age 0 to 4", "Youngest dependent child: Age 5 to 9", "Youngest dependent child: Age 10 to 15", "Youngest dependent child: Age 16 to 18"),
  age_common = c("Aged 0 to 4", "Aged 5 to 9", "Aged 10 to 15", "Aged 16 to 18")
)

setkey(mapping_11, "cat_11")

setkey(dc1113_11, "c_dpchuk11_name")

dc1113_11_matched <- dc1113_11[mapping_11]

dc1113_11_matched <- dc1113_11_matched[, .(value = sum(obs_value)),
                                       by = list(geography_code, age_common)]


  ### matching 2021
mapping_21 <- data.table(
  cat_21 = c("Youngest dependent child: Aged 0 to 4 years", "Youngest dependent child: Aged 5 to 9 years",
             "Youngest dependent child: Aged 10 to 15 years", "Youngest dependent child: Aged 16 to 18 years"),
  age_common = c("Aged 0 to 4", "Aged 5 to 9", "Aged 10 to 15", "Aged 16 to 18")
)

setkey(mapping_21, "cat_21")

setkey(rm006_21, "c2021_hh_depchild_6b_name")

rm006_21_matched <- rm006_21[mapping_21]

rm006_21_matched <- rm006_21_matched[, .(value = sum(obs_value)),
                                     by = list(geography_code, age_common)]


## 4. joining the datasets

  ### renaming the values

colnames(st014_01_matched)[3] <- "value_01"

colnames(dc1113_11_matched)[3] <- "value_11"

colnames(rm006_21_matched)[3] <- "value_21"


  ### setting the joining keys

setkey(st014_01_matched, "geography_code", "age_common")

setkey(dc1113_11_matched, "geography_code", "age_common")

setkey(rm006_21_matched, "geography_code", "age_common")

rm006_21_dc1113_11 <- rm006_21_matched[dc1113_11_matched]

setkey(rm006_21_dc1113_11, "geography_code", "age_common")

rm006_21_dc1113_11_st014_01 <- rm006_21_dc1113_11[st014_01_matched]


## 5. writing the dataset

fwrite(x = rm006_21_dc1113_11_st014_01,
       file = "data/category_matched_at_la/rm006_21_dc1113_11_st014_01.csv")


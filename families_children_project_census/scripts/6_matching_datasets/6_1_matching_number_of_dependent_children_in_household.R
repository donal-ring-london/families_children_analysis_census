

setwd("//profileshare1/rdrfoldersA-D/DRing/Desktop/families_analysis_initial") # again, get rid of this once it's a project


## 1. libraries and functions

library(data.table)


## 2. Reading in the datasets

uv006_01 <- fread("data/2001/ UV006.csv")

qs118_11 <- fread("data/2011/ QS118EW.csv")

ftb_hhcomp_21 <- fread("data/2021/ ftb_household_composition_37.csv")


## 3. matching the number of children in each household across censuses. Final categories will be "One dependent child" and "Two or more dependent children"

  ### 3.1. 2001 matching
mapping_01 <- data.table(
  cat_01 = c("One dependent child in family aged 0 to 4", "One dependent child in family aged 5 to 11", "One dependent child in family aged 12 to 18", 
             "Two or more dependent children in family; youngest aged 0 to 4", "Two or more dependent children in family; youngest aged 5 to 11", "Two or more dependent children in family; youngest aged 12 to 18"),
  cat_common = c("One dependent child", "One dependent child", "One dependent child", 
                 "Two or more dependent children", "Two or more dependent children", "Two or more dependent children")
)

setkey(mapping_01, "cat_01")
setkey(uv006_01, "cell_name")

uv006_01_matched <- uv006_01[mapping_01]

uv006_01_matched <- uv006_01_matched[,-"cell_name"]


uv006_01_matched <-  uv006_01_matched[,.(value = sum(obs_value)),
                                         by = list(geography_code, cat_common)]


  ### 3.2. 2011 matching
mapping_11 <- data.table(
  cat_11 = c("One dependent child in family aged 0 to 4", "One dependent child in family aged 5 to 11", "One dependent child in family aged 12 to 18", 
             "Two dependent children in family; youngest aged 0 to 4", "Two dependent children in family; youngest aged 5 to 11", "Two dependent children in family; youngest aged 12 to 18",
             "Three or more dependent children in family; youngest aged 0 to 4", "Three or more dependent children in family; youngest aged 5 to 11", "Three or more dependent children in family; youngest aged 12 to 18"),
  cat_common = c("One dependent child", "One dependent child", "One dependent child",
                 "Two or more dependent children", "Two or more dependent children", "Two or more dependent children",
                 "Two or more dependent children", "Two or more dependent children", "Two or more dependent children")
)


setkey(mapping_11, "cat_11")
setkey(qs118_11, "cell_name")

qs118_11_matched <- qs118_11[mapping_11]

qs118_11_matched <- qs118_11_matched[,-"cell_name"]


qs118_11_matched <-  qs118_11_matched[,.(value = sum(obs_value)),
                                      by = list(geography_code, cat_common)]



  ### 3.3. 2021 matching
mapping_21 <- data.table(
  cat_21 = c("Single family household: Opposite-sex married couple family: With one dependent child", "Single family household: Opposite-sex married couple family: With two or more dependent children",
             "Single family household: Same-sex married couple family: With one dependent child", "Single family household: Same-sex married couple family: With two or more dependent children",
             "Single family household: Opposite-sex civil partnership couple family: With one dependent child", "Single family household: Opposite-sex civil partnership couple family: With two or more dependent children",
             "Single family household: Same-sex civil partnership couple family: With one dependent child", "Single family household: Same-sex civil partnership couple family: With two or more dependent children",
             "Single family household: Opposite-sex cohabiting couple family: With one dependent child", "Single family household: Opposite-sex cohabiting couple family: With two or more dependent children",
             "Single family household: Same-sex cohabiting couple family: With one dependent child", "Single family household: Same-sex cohabiting couple family: With two or more dependent children",
             "Single family household: Lone parent family: With one dependent child", "Single family household: Lone parent family: With two or more dependent children",
             "Other household types: With one dependent child", "Other household types: With two or more dependent children"),
  cat_common = c("One dependent child","Two or more dependent children",
                 "One dependent child","Two or more dependent children",
                 "One dependent child","Two or more dependent children",
                 "One dependent child","Two or more dependent children",
                 "One dependent child","Two or more dependent children",
                 "One dependent child","Two or more dependent children",
                 "One dependent child","Two or more dependent children",
                 "One dependent child","Two or more dependent children")
)


setkey(mapping_21, "cat_21")
setkey(ftb_hhcomp_21, "household_composition_37")

ftb_hhcomp_21_matched <- ftb_hhcomp_21[mapping_21]


ftb_hhcomp_21_matched <- ftb_hhcomp_21_matched[,-"household_composition_37"]


ftb_hhcomp_21_matched <-  ftb_hhcomp_21_matched[,.(value = sum(observation)),
                                      by = list(lower_tier_local_authorities_code, cat_common)]


                                
## 4. joining all of the matched datasets

colnames(ftb_hhcomp_21_matched)[3] <- "value_21"

colnames(qs118_11_matched)[3] <- "value_11" 

colnames(uv006_01_matched)[3] <- "value_01"


setkey(ftb_hhcomp_21_matched, "lower_tier_local_authorities_code", "cat_common")
setkey(qs118_11_matched, "geography_code", "cat_common")
setkey(uv006_01_matched, "geography_code", "cat_common")

ftb_hhcomp_21_qs118_11 <- ftb_hhcomp_21_matched[qs118_11_matched]

setkey(ftb_hhcomp_21_qs118_11, "lower_tier_local_authorities_code", "cat_common")

ftb_hhcomp_21_qs118_11_uv006_01 <- ftb_hhcomp_21_qs118_11[uv006_01_matched]


## 5. writing the dataset

fwrite(x = ftb_hhcomp_21_qs118_11_uv006_01,
       file = "data/category_matched_at_la/ftb_hhcomp_21_qs118_11_uv_006_01.csv")






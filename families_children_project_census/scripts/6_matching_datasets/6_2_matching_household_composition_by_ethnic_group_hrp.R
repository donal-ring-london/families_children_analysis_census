
## matching household composition by ethnic group of hrp

setwd("//profileshare1/rdrfoldersA-D/DRing/Desktop/families_analysis_initial") # again, get rid of this once it's a project


## 1. libraries and functions

library(data.table)


## 2. Reading in the datasets

st106_01 <- fread("data/2001/ ST106.csv")

dc1201_11 <- fread("data/2011/ DC1201EW.csv")

ftb_hhcomp_ethgroup_21 <- fread("data/2021/ ftb_household_composition_by_ethnic_group_hrp.csv")



## 3. matching the number of children in each household across censuses. Only one final category - "Household has dependent children"

unique(st106_01$c_hhchuk11_name)

unique(dc1201_11$c_hhchuk11_name)

unique(ftb_hhcomp_ethgroup_21$household_composition_15)


  ### 3.1. matching 2001

mapping_01 <- data.table(
  cat_01 = c("One family : Married couple family households : With one dependent child", "One family : Married couple family households : With two or more dependent children",
             "One family : Cohabiting couple family households : With one dependent child","One family : Cohabiting couple family households : With two or more dependent children",
             "One family : Lone parent households : With one dependent child","One family : Lone parent households : With two or more dependent children",
             "Other households : With one dependent child","Other households : With two or more dependent children"),
  cat_common = c("Household has dependent children", "Household has dependent children", "Household has dependent children", "Household has dependent children",
                 "Household has dependent children", "Household has dependent children", "Household has dependent children", "Household has dependent children")
)


setkey(mapping_01, "cat_01")
setkey(st106_01, "c_hhchuk11_name")

st106_01_matched <- st106_01[mapping_01]

st106_01_matched <- st106_01_matched[,-"c_hhchuk11_name"]

st106_01_matched <-  st106_01_matched[,.(value = sum(obs_value)),
                                      by = list(geography_code, c_ethpuk11_name, cat_common)]

colnames(st106_01_matched)[3] <- "hhcomp_common"                         


  ### 3.2. matching 2011

mapping_11 <- data.table(
  cat_11 = c("One family only: Married or same-sex civil partnership couple: Dependent children", "One family only: Cohabiting couple: Dependent children",
              "One family only: Lone parent: Dependent children","Other household types: With dependent children")   ,
  cat_common = c("Household has dependent children", "Household has dependent children", "Household has dependent children", "Household has dependent children")
)


setkey(mapping_11, "cat_11")
setkey(dc1201_11, "c_hhchuk11_name")

dc1201_11_matched <- dc1201_11[mapping_11]

dc1201_11_matched <- dc1201_11_matched[,-"c_hhchuk11_name"]

dc1201_11_matched <-  dc1201_11_matched[,.(value = sum(obs_value)),
                                      by = list(geography_code, c_ethhuk11_name, cat_common)]

colnames(dc1201_11_matched)[3] <- "hhcomp_common"                         


  ### 3.3. matching 2021

mapping_21 <- data.table(
  cat_21 = c("Single family household: Married or civil partnership couple: Dependent children", "Single family household: Cohabiting couple family: With dependent children",
             "Single family household: Lone parent family: With dependent children", "Other household types: With dependent children"),
  cat_common = c("Household has dependent children", "Household has dependent children", "Household has dependent children", "Household has dependent children")
)

setkey(mapping_21, "cat_21")

setkey(ftb_hhcomp_ethgroup_21, "household_composition_15")

ftb_hhcomp_ethgroup_21_matched <- ftb_hhcomp_ethgroup_21[mapping_21]

ftb_hhcomp_ethgroup_21_matched <- ftb_hhcomp_ethgroup_21_matched[,-"household_composition_15"]

ftb_hhcomp_ethgroup_21_matched <-  ftb_hhcomp_ethgroup_21_matched[,.(value = sum(observation)),
                                        by = list(lower_tier_local_authorities_code, ethnic_group_20, cat_common)]


colnames(ftb_hhcomp_ethgroup_21_matched)[3] <- "hhcomp_common"


## 4. matching ethnic group across each of the datasets

## final categories will be - "White: British", "White: Irish", "White: Other", 
## "Mixed: White and Black Caribbean", "Mixed: White and Black African", "Mixed: White and Asian", "Mixed: Other", 
## "Asian: Indian", "Asian: Pakistani", "Asian: Bangladeshi", "Asian: Other", "Asian: Chinese",
## "Black: Caribbean", "Black: African" , "Black: Other"
## "Other"


unique(st106_01_matched$c_ethpuk11_name)

unique(dc1201_11_matched$c_ethhuk11_name)

unique(ftb_hhcomp_ethgroup_21_matched$ethnic_group_20)


  ### 4.1. matching 2001
mapping_01 <- data.table(
  cat_01 = c("White: British", "White: Irish", "White: Other", 
             "Mixed: White and Black Caribbean", "Mixed: White and Black African", "Mixed: White and Asian", "Mixed: Other",
              "Asian/Asian British: Indian", "Asian/Asian British: Pakistani", "Asian/Asian British: Bangladeshi", "Asian/Asian British: Other", 
              "Black/Black British: Black Caribbean", "Black/Black British: Black African", "Black/Black British: Other", 
             "Chinese/Other: Chinese", "Chinese/Other: Other"),
  cat_common = c("White: British", "White: Irish",  "White: Other",
                 "Mixed: White and Black Caribbean", "Mixed: White and Black African", "Mixed: White and Asian", "Mixed: Other",
                 "Asian: Indian", "Asian: Pakistani", "Asian: Bangladeshi", "Asian: Other",
                 "Black: Caribbean", "Black: African", "Black: Other",
                 "Asian: Chinese", "Other")
)

setkey(mapping_01, "cat_01")
setkey(st106_01_matched, "c_ethpuk11_name")

st106_01_matched2 <- st106_01_matched[mapping_01]

st106_01_matched2 <- st106_01_matched2[,-"c_ethpuk11_name"]

st106_01_matched2 <-  st106_01_matched2[,.(value = sum(value)),
                                      by = list(geography_code, hhcomp_common, cat_common)]


colnames(st106_01_matched2)[3] <- "eth_common"


  ### 4.2. matching 2011

mapping_11 <- data.table(
  cat_11 = c("White: English/Welsh/Scottish/Northern Irish/British", "White: Irish", "White: Gypsy or Irish Traveller", "White: Other White",
             "Mixed/multiple ethnic group: White and Black Caribbean", "Mixed/multiple ethnic group: White and Black African","Mixed/multiple ethnic group: White and Asian", "Mixed/multiple ethnic group: Other Mixed",
             "Asian/Asian British: Indian","Asian/Asian British: Pakistani", "Asian/Asian British: Bangladeshi", "Asian/Asian British: Chinese", "Asian/Asian British: Other Asian", 
             "Black/African/Caribbean/Black British: African", "Black/African/Caribbean/Black British: Caribbean", "Black/African/Caribbean/Black British: Other Black",
             "Other ethnic group: Arab", "Other ethnic group: Any other ethnic group"),
  cat_common = c("White: British", "White: Irish", "White: Other", "White: Other",
                 "Mixed: White and Black Caribbean", "Mixed: White and Black African","Mixed: White and Asian", "Mixed: Other",
                 "Asian: Indian","Asian: Pakistani", "Asian: Bangladeshi", "Asian: Chinese", "Asian: Other", 
                 "Black: African", "Black: Caribbean", "Black: Other",
                 "Other", "Other")
)

setkey(mapping_11, "cat_11")
setkey(dc1201_11_matched, "c_ethhuk11_name")

dc1201_11_matched2 <- dc1201_11_matched[mapping_11]

dc1201_11_matched2 <- dc1201_11_matched2[,-"c_ethhuk11_name"]

dc1201_11_matched2 <-  dc1201_11_matched2[,.(value = sum(value)),
                                        by = list(geography_code, hhcomp_common, cat_common)]

colnames(dc1201_11_matched2)[3] <- "eth_common"

unique(ftb_hhcomp_ethgroup_21_matched$ethnic_group_20)

  ### 4.3. matching 2021
mapping_21 <- data.table(
  cat_21 = c("Asian, Asian British or Asian Welsh: Bangladeshi", "Asian, Asian British or Asian Welsh: Chinese", "Asian, Asian British or Asian Welsh: Indian", "Asian, Asian British or Asian Welsh: Pakistani", "Asian, Asian British or Asian Welsh: Other Asian",
              "Black, Black British, Black Welsh, Caribbean or African: African", "Black, Black British, Black Welsh, Caribbean or African: Caribbean", "Black, Black British, Black Welsh, Caribbean or African: Other Black",
              "Mixed or Multiple ethnic groups: White and Asian", "Mixed or Multiple ethnic groups: White and Black African", "Mixed or Multiple ethnic groups: White and Black Caribbean", "Mixed or Multiple ethnic groups: Other Mixed or Multiple ethnic groups",
              "White: English, Welsh, Scottish, Northern Irish or British", "White: Irish", "White: Gypsy or Irish Traveller","White: Roma", "White: Other White",
              "Other ethnic group: Arab", "Other ethnic group: Any other ethnic group"),
  
  cat_common = c("Asian: Bangladeshi", "Asian: Chinese", "Asian: Indian", "Asian: Pakistani", "Asian: Other",
                 "Black: African", "Black: Caribbean", "Black: Other",
                 "Mixed: White and Asian", "Mixed: White and Black African", "Mixed: White and Black Caribbean", "Mixed: Other",
                 "White: British", "White: Irish", "White: Other", "White: Other", "White: Other",
                 "Other", "Other") 
)

setkey(mapping_21, "cat_21")
setkey(ftb_hhcomp_ethgroup_21_matched, "ethnic_group_20")

ftb_hhcomp_ethgroup_21_matched2 <- ftb_hhcomp_ethgroup_21_matched[mapping_21]

ftb_hhcomp_ethgroup_21_matched2 <- ftb_hhcomp_ethgroup_21_matched2[,-"ethnic_group_20"]

ftb_hhcomp_ethgroup_21_matched2 <- ftb_hhcomp_ethgroup_21_matched2[,.(value = sum(value)),
                                          by = list(lower_tier_local_authorities_code, hhcomp_common, cat_common)]

colnames(ftb_hhcomp_ethgroup_21_matched2)[3] <- "eth_common"


## 5. joining all of the datasets
colnames(st106_01_matched2)[4] <- "value_01"

colnames(dc1201_11_matched2)[4] <- "value_11"

colnames(ftb_hhcomp_ethgroup_21_matched2)[4] <- "value_21"


setkey(st106_01_matched2, "geography_code", "hhcomp_common", "eth_common")

setkey(dc1201_11_matched2, "geography_code", "hhcomp_common", "eth_common")

setkey(ftb_hhcomp_ethgroup_21_matched2, "lower_tier_local_authorities_code", "hhcomp_common", "eth_common")


ftb_hhcomp_ethgroup_21_dc1201_11 <- ftb_hhcomp_ethgroup_21_matched2[dc1201_11_matched2]
setkey(ftb_hhcomp_ethgroup_21_dc1201_11, "lower_tier_local_authorities_code", "hhcomp_common", "eth_common")


ftb_hhcomp_ethgroup_21_dc1201_11_st106_01 <- ftb_hhcomp_ethgroup_21_dc1201_11[st106_01_matched2]


## 6. writing the final dataset

fwrite(x = ftb_hhcomp_ethgroup_21_dc1201_11_st106_01,
       file = "data/category_matched_at_la/ftb_hhcomp_ethgroup_21_dc1201_11_st106_01.csv")








## matching household composition by ns-sec of hrp

setwd("//profileshare1/rdrfoldersA-D/DRing/Desktop/families_analysis_initial") # again, get rid of this once it's a project


## 1. libraries and functions

library(data.table)


## 2. Reading in the datasets

st044_01 <- fread("data/2001/ ST044.csv")

dc6115_11 <- fread("data/2011/ DC6115EW.csv")

ftb_hhcomp_nssec_21 <- fread("data/2021/ ftb_household_composition_by_nssec_hrp.csv")



## 3. Matching dependent children across each of the datasets. The final category will be "Household has dependent children"

## In 2011, the household composition variable doesn't distinguish between lone parent families with dependent children and lone parent families without dependent children. 
## This means that to make the datasets comparable over the three census periods, we have to refine the analysis only to couple family households. 
## It's really a shame to lose lone parent families, but a look around the available datasets for 2021 suggests that there is nothing else to be done. 
## same with other household types. So they need to be left out too. 

unique(st044_01$c_hhchuk11_name)

unique(dc6115_11$c_hhchuk11_name)

unique(ftb_hhcomp_nssec_21$household_composition_15)


  ### 3.1. matching 2001

mapping_01 <- data.table(
  cat_01 = c("One family and no others - Couple households - With one dependent child", "One family and no others - Couple households - With two or more dependent child"), 
  cat_common = c("Household has dependent children", "Household has dependent children")
)

setkey(mapping_01, "cat_01")
setkey(st044_01, "c_hhchuk11_name")


st044_01_matched <- st044_01[mapping_01]

st044_01_matched <- st044_01_matched[,-"c_hhchuk11_name"]

st044_01_matched <- st044_01_matched[, .(value = sum(obs_value)),
                                     by = list(geography_code, c_nssec_name, cat_common)]

colnames(st044_01_matched)[3] <- "hhcomp_common"


  ### 3.2. matching 2011

mapping_11 <- data.table(
  cat_11 = c("One family only: Couple household: Dependent children"),
  cat_common = c("Household has dependent children")
)

setkey(mapping_11, "cat_11")
setkey(dc6115_11, "c_hhchuk11_name")

dc6115_11_matched <- dc6115_11[mapping_11]

dc6115_11_matched <- dc6115_11_matched[,-"c_hhchuk11_name"]

dc6115_11_matched <- dc6115_11_matched[, .(value = sum(obs_value)),
                                       by = list(geography_code, c_nssec_name, cat_common)]

colnames(dc6115_11_matched)[3] <- "hhcomp_common"


  ### 3.3. matching 2021

mapping_21 <- data.table(
  cat_21 = c("Single family household: Married or civil partnership couple: Dependent children", "Single family household: Cohabiting couple family: With dependent children"),
  cat_common = c("Household has dependent children", "Household has dependent children")
)

setkey(mapping_21, "cat_21")
setkey(ftb_hhcomp_nssec_21, "household_composition_15")

ftb_hhcomp_nssec_21_matched <- ftb_hhcomp_nssec_21[mapping_21]

ftb_hhcomp_nssec_21_matched <- ftb_hhcomp_nssec_21_matched[,-"household_composition_15"]

ftb_hhcomp_nssec_21_matched <- ftb_hhcomp_nssec_21_matched[, .(value = sum(observation)),
                                                           by = list(lower_tier_local_authorities_code, `national_statistics_socio-economic_classification_ns-sec_10`, cat_common)]

colnames(ftb_hhcomp_nssec_21_matched)[3] <- "hhcomp_common"



## 4. matching ns_sec of HRP. The final categories will be "1. Higher managerial, administrative and professional occupations", "2. Lower managerial, administrative and professional occupations",
# "3. Intermediate occupations", "4. Small employers and own account workers", "5. Lower supervisory and technical occupations", 
# "6. Semi-routine occupations", "7. Routine occupations", 
# "8. Never worked and long-term unemployed", "L15 Full-time students"


unique(st044_01_matched$c_nssec_name)

unique(dc6115_11_matched$c_nssec_name)

unique(ftb_hhcomp_nssec_21_matched$`national_statistics_socio-economic_classification_ns-sec_10`)

  ### 4.1. matching 2001

mapping_01 <- data.table(
  cat_01 = c("1. Higher managerial, administrative and professional occupations", "2. Lower managerial, administrative and professional occupations", 
             "3. Intermediate occupations", "4. Small employers and own account workers",
             "5. Lower supervisory and technical occupations", "6. Semi-routine occupations", 
             "7. Routine occupations", "8. Never worked and long-term unemployed",                                             
             "L15 Full-time students"),
  cat_common = c("1. Higher managerial, administrative and professional occupations", "2. Lower managerial, administrative and professional occupations",
                 "3. Intermediate occupations","4. Small employers and own account workers", 
                 "5. Lower supervisory and technical occupations", 
                 "6. Semi-routine occupations", "7. Routine occupations", 
                 "8. Never worked and long-term unemployed", "L15 Full-time students")
)

setkey(mapping_01, "cat_01")

setkey(st044_01_matched, "c_nssec_name")


st044_01_matched2 <- st044_01_matched[mapping_01]

st044_01_matched2 <- st044_01_matched2[,-"c_nssec_name"]

st044_01_matched2 <- st044_01_matched2[, .(value = sum(value)),
                                       by = list(geography_code, hhcomp_common, cat_common)]


colnames(st044_01_matched2)[3] <- "nssec_common"


  ### 4.2. matching 2011

mapping_11 <- data.table(
  cat_11 = c("1. Higher managerial, administrative and professional occupations", "2. Lower managerial, administrative and professional occupations", 
             "3. Intermediate occupations", "4. Small employers and own account workers", 
             "5. Lower supervisory and technical occupations", "6. Semi-routine occupations",
             "7. Routine occupations", "8. Never worked and long-term unemployed",
             "L15 Full-time students"),
  cat_common = c("1. Higher managerial, administrative and professional occupations", "2. Lower managerial, administrative and professional occupations", 
                 "3. Intermediate occupations", "4. Small employers and own account workers", 
                 "5. Lower supervisory and technical occupations", "6. Semi-routine occupations",
                 "7. Routine occupations", "8. Never worked and long-term unemployed",
                 "L15 Full-time students")
)

setkey(mapping_11, "cat_11")

setkey(dc6115_11_matched, "c_nssec_name")

dc6115_11_matched2 <- dc6115_11_matched[mapping_11]

dc6115_11_matched2 <- dc6115_11_matched2[, -"c_nssec_name"]

dc6115_11_matched2 <- dc6115_11_matched2[, .(value = sum(value)),
                                         by = list(geography_code, hhcomp_common, cat_common)]


colnames(dc6115_11_matched2)[3] <- "nssec_common"



  ### 4.3. matching 2021

mapping_21 <- data.table(
  cat_21 = c("L1, L2 and L3: Higher managerial, administrative and professional occupations", "L4, L5 and L6: Lower managerial, administrative and professional occupations", 
             "L7: Intermediate occupations", "L8 and L9: Small employers and own account workers",
             "L10 and L11: Lower supervisory and technical occupations", "L12: Semi-routine occupations", 
             "L13: Routine occupations", "L14.1 and L14.2: Never worked and long-term unemployed",
             "L15: Full-time students"),
  cat_common = c("1. Higher managerial, administrative and professional occupations", "2. Lower managerial, administrative and professional occupations", 
                 "3. Intermediate occupations", "4. Small employers and own account workers", 
                 "5. Lower supervisory and technical occupations", "6. Semi-routine occupations",
                 "7. Routine occupations", "8. Never worked and long-term unemployed",
                 "L15 Full-time students")
)

setkey(mapping_21, "cat_21")

setkey(ftb_hhcomp_nssec_21_matched, "national_statistics_socio-economic_classification_ns-sec_10")

ftb_hhcomp_nssec_21_matched2 <- ftb_hhcomp_nssec_21_matched[mapping_21]

ftb_hhcomp_nssec_21_matched2 <- ftb_hhcomp_nssec_21_matched2[, -"national_statistics_socio-economic_classification_ns-sec_10"]

ftb_hhcomp_nssec_21_matched2 <- ftb_hhcomp_nssec_21_matched2[, .(value = sum(value)),
                                                             by = list(lower_tier_local_authorities_code, hhcomp_common, cat_common)]


colnames(ftb_hhcomp_nssec_21_matched2)[3] <- "nssec_common"


## 5. matching the datasets

colnames(st044_01_matched2)[4] <- "value_01"

colnames(dc6115_11_matched2)[4] <- "value_11"

colnames(ftb_hhcomp_nssec_21_matched2)[4] <- "value_21"


setkey(st044_01_matched2, "geography_code", "hhcomp_common", "nssec_common")

setkey(dc6115_11_matched2, "geography_code", "hhcomp_common", "nssec_common")

setkey(ftb_hhcomp_nssec_21_matched2, "lower_tier_local_authorities_code", "hhcomp_common", "nssec_common")

ftb_hhcomp_nssec_21_dc6115_11 <- ftb_hhcomp_nssec_21_matched2[dc6115_11_matched2]

setkey(ftb_hhcomp_nssec_21_dc6115_11, "lower_tier_local_authorities_code", "hhcomp_common", "nssec_common")

ftb_hhcomp_nssec_21_dc6115_11_st044_01 <- ftb_hhcomp_nssec_21_dc6115_11[st044_01_matched2]


## 6. writing the final dataset


fwrite(
  x = ftb_hhcomp_nssec_21_dc6115_11_st044_01,
  file = "data/category_matched_at_la/ftb_hhcomp_nssec_21_dc6115_11_st044_01.csv"
)




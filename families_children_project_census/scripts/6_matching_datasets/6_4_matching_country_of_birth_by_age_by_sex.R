

## matching household composition by ns-sec of hrp

setwd("//profileshare1/rdrfoldersA-D/DRing/Desktop/families_analysis_initial") # again, get rid of this once it's a project


## 1. libraries and functions

library(data.table)


## 2. Reading in the datasets

st015_01 <- fread("data/2001/ ST015.csv") # can't use this. The age range are too broad, and it's not possible to narrow down to child-bearing age. Meaning now we can only match from 2011 to 2021.

dc2103_11 <- fread("data/2011/ DC2103EW.csv")

ftb_cob_by_age_by_sex <- fread("data/2021/ ftb_cob_by_age_by_sex.csv")


## 3. matching age. The final category that we are keeping is "Aged 16 to 49". This is a bit broader than we'd really like...but the only other cutoff points that are possible are 25 at the low end or 35 at the high end. Which would be too narrow. 

unique(dc2103_11$c_age_name)

unique(ftb_cob_by_age_by_sex$age_10)


  ### matching 2001. As discussed above, we're not matching 2001 because the age ranges are too broad. 



  ### matching 2011

mapping_11 <- data.table(
  cat_11 = c("Age 16 to 24", "Age 25 to 34", "Age 35 to 49"),
  cat_common = c("Aged 16 to 49", "Aged 16 to 49", "Aged 16 to 49")
)

setkey(mapping_11, "cat_11")
setkey(dc2103_11, "c_age_name")

dc2103_11_matched <- dc2103_11[mapping_11]

dc2103_11_matched <- dc2103_11_matched[,-"c_age_name"]

dc2103_11_matched <- dc2103_11_matched[, .(value = sum(obs_value)),
                                       by = list(geography_code, c_sex_name, c_cob_name, cat_common)]


colnames(dc2103_11_matched)[4] <- "age_common"

  ### matching 2021

mapping_21 <- data.table(
  cat_21 = c("Aged 16 to 19 years", "Aged 20 to 21 years", "Aged 22 to 24 years", "Aged 25 to 29 years", 
             "Aged 30 to 34 years", "Aged 35 to 39 years", "Aged 40 to 44 years", "Aged 45 to 49 years"),
  cat_common = c("Aged 16 to 49", "Aged 16 to 49", "Aged 16 to 49", "Aged 16 to 49", 
                 "Aged 16 to 49", "Aged 16 to 49", "Aged 16 to 49", "Aged 16 to 49")
)


setkey(mapping_21, "cat_21")
setkey(ftb_cob_by_age_by_sex, "age_10")

ftb_cob_by_age_by_sex_matched <- ftb_cob_by_age_by_sex[mapping_21]

ftb_cob_by_age_by_sex_matched <- ftb_cob_by_age_by_sex_matched[, -"age_10"]

ftb_cob_by_age_by_sex_matched <- ftb_cob_by_age_by_sex_matched[, .(value = sum(observation)),
                                                               by = list(lower_tier_local_authorities_code, country_of_birth_8, sex_2, cat_common)]


colnames(ftb_cob_by_age_by_sex_matched)[4] <- "age_common"


## 4. matching country of birth. The final categories will be "United Kingdom", "Ireland", 
## "Rest of Europe", "Africa", "Middle East and Asia", "The Americas and the Caribbean", "Antarctica, Oceania, and Other"

unique(dc2103_11$c_cob_name)

unique(ftb_cob_by_age_by_sex$country_of_birth_8)


  ### matching 2011

mapping_11 <- data.table(
  cat_11 = c("Europe: United Kingdom: England", "Europe: United Kingdom: Northern Ireland", "Europe: United Kingdom: Scotland", "Europe: United Kingdom: Wales",
             "Europe: United Kingdom: Great Britain not otherwise specified", "Europe: United Kingdom: United Kingdom not otherwise specified", "Europe: Ireland", "Europe: Other Europe: EU countries: Member countries in March 2001",
             "Europe: Other Europe: EU countries: Accession countries April 2001 to March 2011", "Europe: Other Europe: Rest of Europe", "Africa: North Africa", "Africa: Central and Western Africa",
             "Africa: South and Eastern Africa", "Africa: Africa not otherwise specified", "Middle East and Asia: Middle East", "Middle East and Asia: Eastern Asia",
             "Middle East and Asia: Southern Asia", "Middle East and Asia: South-East Asia", "Middle East and Asia: Central Asia",
             "The Americas and the Caribbean: North America and the Caribbean", "The Americas and the Caribbean: Central and South America", 
             "Antarctica and Oceania (including Australasia)", "Other"),
  cat_common = c("United Kingdom", "United Kingdom", "United Kingdom", "United Kingdom", "United Kingdom", "United Kingdom", "Ireland", 
                 "Rest of Europe", "Rest of Europe", "Rest of Europe", "Africa", "Africa", "Africa", "Africa",
                 "Middle East and Asia", "Middle East and Asia", "Middle East and Asia", "Middle East and Asia", "Middle East and Asia", 
                 "The Americas and the Caribbean", "The Americas and the Caribbean", "Antarctica, Oceania, and Other", "Antarctica, Oceania, and Other")
)

setkey(mapping_11, "cat_11")
setkey(dc2103_11_matched, "c_cob_name")

dc2103_11_matched2 <- dc2103_11_matched[mapping_11]

dc2103_11_matched2 <- dc2103_11_matched2[,-"c_cob_name"]

dc2103_11_matched2 <- dc2103_11_matched2[, .(value = sum(value)),
                                         by = list(geography_code, c_sex_name, age_common, cat_common)]

colnames(dc2103_11_matched2)[4] <- "cob_common"


  ### matching 2021

mapping_21 <- data.table(
  cat_21 = c("Europe: United Kingdom", "Europe: Ireland", "Europe: Other Europe", "Africa",
             "Middle East and Asia", "The Americas and the Caribbean", "Antarctica and Oceania (including Australasia) and Other"),
  cat_common = c("United Kingdom", "Ireland", "Rest of Europe", "Africa", "Middle East and Asia", "The Americas and the Caribbean", "Antarctica, Oceania, and Other")
)

setkey(mapping_21, "cat_21")
setkey(ftb_cob_by_age_by_sex_matched, "country_of_birth_8")

ftb_cob_by_age_by_sex_matched2 <- ftb_cob_by_age_by_sex_matched[mapping_21]

ftb_cob_by_age_by_sex_matched2 <- ftb_cob_by_age_by_sex_matched2[,-"country_of_birth_8"]

ftb_cob_by_age_by_sex_matched2 <- ftb_cob_by_age_by_sex_matched2[, .(value = sum(value)),
                                                                 by = list(lower_tier_local_authorities_code, sex_2, age_common, cat_common)]

colnames(ftb_cob_by_age_by_sex_matched2)[4] <- "cob_common"


## 5. joining all datasets

  ### sub-task - changing the sex names in 2011

dc2103_11_matched2$c_sex_name <- gsub("Females", "Female", dc2103_11_matched2$c_sex_name)

dc2103_11_matched2$c_sex_name <- gsub("Males", "Male", dc2103_11_matched2$c_sex_name)


  ### setting the joining keys

setkey(dc2103_11_matched2, "geography_code", "c_sex_name", "age_common", "cob_common")

setkey(ftb_cob_by_age_by_sex_matched2, "lower_tier_local_authorities_code", "sex_2", "age_common", "cob_common")


  ### renaming the values

colnames(dc2103_11_matched2)[5] <- "value_11"

colnames(ftb_cob_by_age_by_sex_matched2)[5] <- "value_21"


  ### joining

ftb_cob_age_sex_21_dc2103_11 <- ftb_cob_by_age_by_sex_matched2[dc2103_11_matched2]


## 6. writing the final dataset

fwrite(
  x = ftb_cob_age_sex_21_dc2103_11,
  file = "data/category_matched_at_la/ftb_cob_age_sex_21_dc2103_11.csv"
)






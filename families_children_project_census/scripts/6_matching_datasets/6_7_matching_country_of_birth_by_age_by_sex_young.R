

## matching country of birth by age by sex, and only keeping those aged 15 and under.

setwd("//profileshare1/rdrfoldersA-D/DRing/Desktop/families_analysis_initial") # again, get rid of this once it's a project


## 1. libraries and functions

library(data.table)


## 2. Reading in the datasets

st015_01 <- fread("data/2001/ ST015.csv")

dc2103_11 <- fread("data/2011/ DC2103EW.csv")

ftb_cob_by_age_by_sex <- fread("data/2021/ ftb_cob_by_age_by_sex.csv")


## 3. matching age. The final category that we are keeping is "Aged 0 to 15". It doesn't go further than this, so we are unable to distinguish between primary and secondary school children. 

unique(dc2103_11$c_age_name)

unique(ftb_cob_by_age_by_sex$age_10)

unique(st015_01$c_age_name)


### matching 2001

st015_01_matched <- st015_01[c_age_name == "Aged 0 to 15",]

st015_01_matched <- st015_01_matched[, .(value = sum(obs_value)),
                      by = list(geography_code, c_cob_name, c_age_name)]

colnames(st015_01_matched)[3] <- "age_common"


### matching 2011

dc2103_11_matched <- dc2103_11[c_age_name == "Age 0 to 15", ]

dc2103_11_matched$c_age_name <- "Aged 0 to 15"

dc2103_11_matched <- dc2103_11_matched[, .(value = sum(obs_value)),
                                 by = list(geography_code, c_cob_name, c_age_name)]


colnames(dc2103_11_matched)[3] <- "age_common"


### matching 2021

ftb_cob_by_age_by_sex_matched <- ftb_cob_by_age_by_sex[age_10 == "Aged 15 years and under", ]

ftb_cob_by_age_by_sex_matched$age_10 <- "Aged 0 to 15"

ftb_cob_by_age_by_sex_matched <- ftb_cob_by_age_by_sex_matched[, .(value = sum(observation)),
                                              by = list(lower_tier_local_authorities_code, country_of_birth_8, age_10)]


colnames(ftb_cob_by_age_by_sex_matched)[3] <- "age_common"


## 4. matching country of birth. The final categories will be "United Kingdom", "Ireland", "Rest of Europe", "Africa", "Middle East and Asia", "The Americas and the Caribbean", "Antarctica, Oceania, and Other"

unique(dc2103_11$c_cob_name)

unique(ftb_cob_by_age_by_sex$country_of_birth_8)

unique(st015_01$c_cob_name)



### matching 2001
### cyprus is counted here as middle east, fairly sure it's europe (or at least partly in europe) in later censuses. Not sure if we can do anything about that. 
### Also, all of middle east is counted under asia. 

mapping_01 <- data.table(
 cat_01 = c("United Kingdom", "Republic of Ireland", "Ireland not otherwise specified", 
            "Channel Islands and Isle of Man", "Other Western Europe", "Eastern Europe", 
            "Africa", 
            "Asia", 
            "North America", "South America", 
            "Oceania", "Other"),
 cob_common = c("United Kingdom", "Ireland", "Ireland",
                "Rest of Europe", "Rest of Europe", "Rest of Europe",
                "Africa",
                "Middle East and Asia",
                "The Americas and the Caribbean", "The Americas and the Caribbean",
                "Antarctica, Oceania, and Other", "Antarctica, Oceania, and Other")
)


setkey(mapping_01, "cat_01")
setkey(st015_01_matched, "c_cob_name")

st015_01_matched2 <- st015_01_matched[mapping_01]

st015_01_matched2 <- st015_01_matched2[, .(value = sum(value)),
                                    by = list(geography_code, age_common, cob_common)]



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
                                         by = list(geography_code, age_common, cat_common)]

colnames(dc2103_11_matched2)[3] <- "cob_common"



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
                                                                 by = list(lower_tier_local_authorities_code, age_common, cat_common)]

colnames(ftb_cob_by_age_by_sex_matched2)[3] <- "cob_common"


## 5. joining all datasets

### setting the joining keys

setkey(st015_01_matched2, "geography_code", "age_common", "cob_common")

setkey(dc2103_11_matched2, "geography_code", "age_common", "cob_common")

setkey(ftb_cob_by_age_by_sex_matched2, "lower_tier_local_authorities_code", "age_common", "cob_common")


### renaming the values

colnames(st015_01_matched2)[4] <- "value_01"

colnames(dc2103_11_matched2)[4] <- "value_11"

colnames(ftb_cob_by_age_by_sex_matched2)[4] <- "value_21"


### joining

ftb_cob_age_sex_21_dc2103_11 <- ftb_cob_by_age_by_sex_matched2[dc2103_11_matched2]

setkey(ftb_cob_age_sex_21_dc2103_11, "lower_tier_local_authorities_code", "age_common", "cob_common")

ftb_cob_age_sex_21_dc2103_11_st015_01 <- ftb_cob_age_sex_21_dc2103_11[st015_01_matched2]


## 6. writing the final dataset

fwrite(
  x = ftb_cob_age_sex_21_dc2103_11_st015_01,
  file = "data/category_matched_at_la/ftb_cob_age_sex_21_dc2103_11_st015_01.csv"
)





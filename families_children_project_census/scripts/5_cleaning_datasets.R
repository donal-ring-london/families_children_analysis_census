

## libraries and functions 
library(data.table) 
source("functions/fn_remove_totals.R") 
source("functions/fn_remove_subtotals.R") 
 

## 1. reading in the datasets 
 
  ### 2021 RM tables 
data_toread_2021_rm <- c("RM009.csv", "RM012.csv", "RM034.csv", 
                      "RM095.csv", "RM132.csv", "TS003.csv", 
                      "RM006.csv") 

data_2021_rm <- lapply(X = paste0("data/2021/", data_toread_2021_rm), 
                       FUN = fread) 

names(data_2021_rm) <- gsub(".csv", "", data_toread_2021_rm) 


  ### 2021 FTB tables
data_toread_2021_ftb <- c("ftb_cob_by_age_by_sex.csv", "ftb_household_composition_by_ethnic_group_hrp.csv",
                       "ftb_household_composition_by_nssec_hrp.csv", "ftb_household_composition_by_religion_hrp.csv",
                       "ftb_household_composition_37.csv")

data_2021_ftb <- lapply(X = paste0("data/2021/", data_toread_2021_ftb),
                       FUN = fread)

names(data_2021_ftb) <- gsub(".csv", "", data_toread_2021_ftb)


  ### 2011 tables
data_toread_2011 <- c("DC1114EW.csv", "DC1201EW.csv", "DC1202EW.csv", "DC1203EW.csv", "DC1601EWla.csv", 
                   "DC2103EW.csv", "DC5104EWla.csv", "DC6115EW.csv", "LC1118EW.csv", "LC1601EW.csv", 
                   "LC4102EW.csv", "QS118EW.csv",
                   "DC1113EW.csv")

data_2011 <- lapply(X = paste0("data/2011/", data_toread_2011),
                       FUN = fread)

names(data_2011) <- gsub(".csv", "", data_toread_2011)


  ### 2001 tables
data_toread_2001 <- c("ST007.csv", "ST015.csv", "ST031.csv", 
                   "ST044.csv", "ST106.csv", "ST151.csv", "UV006.csv", 
                   "ST014.csv",
                    "ST053.csv")

data_2001 <- lapply(X = paste0("data/2001/", data_toread_2001),
                    FUN = fread)

names(data_2001) <- gsub(".csv", "", data_toread_2001)



## 2. removing totals, and also Does Not Apply for the 2021 FTB datasets
totals <- c("Total", "All categories", "Does not apply")


data_2021_rm <- lapply(X = data_2021_rm,
                FUN = remove_totals,
                total_names = totals)


data_2021_ftb <- lapply(X = data_2021_ftb,
                       FUN = remove_totals,
                       total_names = totals)


data_2011 <- lapply(X = data_2011,
                       FUN = remove_totals,
                       total_names = totals)


data_2001 <- lapply(X = data_2001,
                       FUN = remove_totals,
                       total_names = totals)


## 3. removing subtotals
## no...have decided not to remove subtotals in this file. (1) we might to just take the subtotals, if they are good simplified categories that work across the three datasets, and (2) it's really very fiddly to make this work
## instead, I'm going to rely on the lookups to get rid of categories. These will inner join with the datasets. And anything that is not in the lookup won't get picked up - this includes categories I don't want, whether they are subtotals or not. 


## 4. removing code columns (these are codes for each category in each census characteristic. Read in through the function are they might be useful for some purposes...but not here)

  ### defining function to remove this column from the datasets
remove_code_cols <- function(data){
  
  code_cols <- grep("code", colnames(data), ignore.case = TRUE)
  
  code_cols <- code_cols[-1] # removing the first entry. This will always be the geography code, which we will definitely want to keep
  
  data <- data[,-..code_cols]
  
  return(data)
  
}

  ### applying the function to all datasets
data_2021_rm <- lapply(X = data_2021_rm,
                FUN = remove_code_cols)

data_2021_ftb <- lapply(X = data_2021_ftb,
                        FUN = remove_code_cols)

data_2011 <- lapply(X = data_2011,
                    FUN = remove_code_cols)

data_2001 <- lapply(X = data_2001,
                    FUN = remove_code_cols)




## 5. for the flexible table builder datasets, aligning them with the style of the others read in from nomis. Adding underscores to the names and making them lowercase, taking away brackets and taking away the "_categories" suffix.

  ### defining function to clean up the column names
fix_colnames <- function(data){
  
  colnames(data) <- tolower(colnames(data))
  
  colnames(data) <- gsub(" ", "_", colnames(data), fixed = TRUE)
  colnames(data) <- gsub("(", "", colnames(data), fixed = TRUE)
  colnames(data) <- gsub(")", "", colnames(data), fixed = TRUE)
  colnames(data) <- gsub("_categories", "", colnames(data), fixed = TRUE)
  
  return(data)
  
}

  ### applying the function to all of the flexible table builder datasets
data_2021_ftb <- lapply(X = data_2021_ftb,
          FUN = fix_colnames)


## 6. saving the cleaned datasets
 ### I made an annoying mistake in writing these datasets. I used paste instead of paste0. 
# Meaning I didn't overwrite the old files with the fixed new files as I wanted. 
# Instead I created new files with a space at the start of the name. Too late/too fiddly to change now. But it should be changed later. 

for(i in 1:length(data_toread_2021_ftb)){
  
  fwrite(x = data_2021_ftb[[i]],
         file = paste("data/2021/", data_toread_2021_ftb[i]),
         append = FALSE)
  
}


for(i in 1:length(data_toread_2021_rm)){
  
  fwrite(x = data_2021_rm[[i]],
         file = paste("data/2021/", data_toread_2021_rm[i]),
         append = FALSE)
  
}


for(i in 1:length(data_toread_2011)){
  
  fwrite(x = data_2011[[i]],
         file = paste("data/2011/", data_toread_2011[i]),
         append = FALSE)
  
}


for(i in 1:length(data_toread_2001)){
  
  fwrite(x = data_2001[[i]],
         file = paste("data/2001/", data_toread_2001[i]),
         append = FALSE)
  
}



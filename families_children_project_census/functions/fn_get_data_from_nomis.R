## the nomisr function - put in a table name, get out the table

get_data_from_nomis <- function(table_code, # the code of of the table. 
                                geog_vec, # this is a vector of the geography codes, for the geographies we want data for. NomisR doesn't use standard gss codes. They use their own special geography codes. So these vectors need to be created elsewhere and read in.  
                                id_name_lookup, # in nomisR, you actually need to use the special nomisR table code rather than the standard ONS table code (the one that is the first argument in this function). This argument is a lookup from standard table codes to special nomisR table codes.
                                year, # whether the census year is 2001, 2011, or 2021. We need this because whether Rural/Urban is included varies by year. 
                                multi # whether the dataset is multivariate or univariate. Again needed because of the Rural/Urban variable. Included in 2011 univariate tables but not in 2011 multivariate tables.
                                ){
  
  ## 1. loading libraries
  require(nomisr)
  require(data.table)
  require(parallel)
  
  ## 2. extracting the nomis table id from the census table code 
  
  table_id <- id_name_lookup$id[grep(table_code, id_name_lookup$name.value)]
  
  ## 3. Extracting the variable names for the categories
  
    ### 3a. getting the full table of metadata
  
  metadata <- nomis_get_metadata(table_id)
  
    ### 3b. getting rid of RURAL_URBAN, for any metadata tables that have that in it
  
  keep <- metadata$conceptref != "RURAL_URBAN"
  
  metadata <- metadata[keep,]
  
    ### 3c. getting the variable names from the metadata. Now that we've removed rural_urban, they are always going to be in between "geography" and "measures" in the metadata table
  
  geog_ind <- which(metadata$conceptref == "GEOGRAPHY")
  
  meas_ind <- which(metadata$conceptref == "MEASURES")
  
  meta_inds <- 1:nrow(metadata)
  
  var_inds <- which(between(x = meta_inds, 
                            lower = geog_ind, upper = meas_ind, 
                            incbounds = FALSE))
  
  cat_vars <- unlist(metadata[var_inds,2])
  
    ### 3d. adding "name" and "code" to each of the variables,to query them from nomis (all nomis variables have _name or _code, at the end of the cat_vars extracted in the previous section)
  
  cat_var_names <- tolower(paste0(cat_vars,"_name"))
  cat_var_codes <- tolower(paste0(cat_vars,"_code"))
  
  vars_get <- c(cat_var_names, cat_var_codes)
  
  ## 4. extracting the data with parallel computing, oa by oa (function was originally written for oas, but works for any geographies. Not changing from oas in case something goes wrong)
  
  oa_id <- geog_vec[1]
  
  
    ### 4a. creating the function to extract data for a single oa. It's just using the nomis_get_data function in nomisR to extract a clean dataset for one geography. 
  
  
  if(year == "2011" & multi == "no"){ ## because if 2011 & univariate, we have to deal with that Rural/Urban variable that comes out
    
    get_oa_data <- function(oa_id){
      
      table_id <- table_id
      
      extracted_oa_data <- nomis_get_data(id = table_id, # telling it which table to extract data for
                                          geography = oa_id, # giving it the geography to get data for
                                          rural_urban = 0, # telling it to just include combined rural and urban, rather disaggregating
                                          measures = 20100, # telling it to extract just count, not percentage
                                          select = c("date","geography_code", # the select argument tells it which variables to get. We just want date (which is year), the geography code, the variables in the dataset, and the count value. 
                                                     vars_get,
                                                     "obs_value"))
      
      extracted_oa_data <- data.table(extracted_oa_data)
      
      return(extracted_oa_data)
      
    }
    
  }else if(year == "2021" | year == "2001" | multi == "yes"){ ## for all datasets that aren't 2011 univariate, don't need to include the rural/urban indicator
    
    get_oa_data <- function(oa_id){
      
      table_id <- table_id
      
      extracted_oa_data <- nomis_get_data(id = table_id,
                                          geography = oa_id,
                                          measures = 20100,
                                          select = c("date","geography_code",
                                                     vars_get,
                                                     "obs_value"))
      
      extracted_oa_data <- data.table(extracted_oa_data)
      
      return(extracted_oa_data)
      
    }
    
  }
  
    ### 4b. setting up the cluster, for the parellel computing oa by oa step
  
  no_cores <- round(detectCores()*0.75)
  
  cl <- makeCluster(no_cores)
  
  clusterEvalQ(cl = cl, expr = c(library(nomisr),
                                 library(data.table)))
  
  clusterExport(cl = cl, c("table_id", "vars_get"), envir = environment())
  
    ### 4c. extracting the data to a list (one item for each oa)
  
  extracted_in_oa <- parLapply(cl = cl,
                               X = geog_vec,
                               fun = get_oa_data)
  
  stopCluster(cl)
  
    ### 4d. convert from list to single datatable, and any more formatting
  output_data <- rbindlist(extracted_in_oa)
  
  colnames(output_data) <- tolower(colnames(output_data))
  
  ## 5. Returning the final data.table
    
  return(output_data)  
    
}

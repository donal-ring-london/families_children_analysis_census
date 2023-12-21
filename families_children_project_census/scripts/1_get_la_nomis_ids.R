
### script that gets vectors of all of the geographies we need to extract data for in nomisR. You can't use GSS codes in nomisR. They have their own special geography codes. So this script extracts them.
### this script should be organised into sections and subsections rather than 12 different sections. Change later if time. 

## 0. libraries

library(nomisr)

### 1. getting the meta data for all local authorities

las_11_inf <- data.table(nomis_get_metadata(id = "NM_503_1", # a 2011 census table nomis code - any will do 
                                                concept = "geography", # telling it to extract geography information
                                                type = "TYPE463")) # telling it to extract local authority information


### 2. extracting the ids for London local authorities only

london_las <- c("Camden", "City of London", "Hackney", "Hammersmith and Fulham", "Haringey", "Islington", "Kensington and Chelsea", "Lambeth", "Lewisham", 
                "Newham", "Southwark", "Tower Hamlets", "Wandsworth", "Westminster", "Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Croydon",
                "Ealing", "Enfield", "Greenwich", "Harrow", "Havering", "Hillingdon", "Hounslow", "Kingston upon Thames", "Merton", "Redbridge", "Richmond upon Thames", 
                "Sutton", "Waltham Forest")

lond_la_ids <- las_11_inf$id[las_11_inf$label.en %in% london_las]

lond_la_ids <- data.table(lond_la_ids)

### 3. writing the vector

fwrite(x = lond_la_ids,
       file = "data_resources/london_la_ids_2011.csv")



### 4. getting the meta data for all merged local authorities

las_11_inf <- data.table(nomis_get_metadata(id = "NM_761_1", # a 2011 census table nomis code - needs to be one that is available on merged local authorities
                                            concept = "geography", # telling it to extract geography information
                                            type = "TYPE270")) # telling it to extract merged local authority information



### 5. extracting the ids for London local authorities only

london_las <- c("Camden", "Westminster,City of London", "Hackney", "Hammersmith and Fulham", "Haringey", "Islington", "Kensington and Chelsea", "Lambeth", "Lewisham", 
                "Newham", "Southwark", "Tower Hamlets", "Wandsworth", "Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Croydon",
                "Ealing", "Enfield", "Greenwich", "Harrow", "Havering", "Hillingdon", "Hounslow", "Kingston upon Thames", "Merton", "Redbridge", "Richmond upon Thames", 
                "Sutton", "Waltham Forest")

lond_la_ids <- las_11_inf$id[las_11_inf$label.en %in% london_las]

lond_la_ids <- data.table(lond_la_ids)


### 6. writing the vector

fwrite(x = lond_la_ids,
       file = "data_resources/london_merged_la_ids_2011.csv")


nomis_id_name_lookup

### 7. getting the meta data for all 2021 local authorities

las_21_inf <- data.table(nomis_get_metadata(id = "NM_2021_1", # a 2021 census table - any will do 
                                            concept = "geography", # telling it to extract geography information
                                            type = "TYPE154")) # telling it to extract 2021 local authority information


### 8. extracting the ids for London local authorities only

london_las <- c("Camden", "City of London", "Hackney", "Hammersmith and Fulham", "Haringey", "Islington", "Kensington and Chelsea", "Lambeth", "Lewisham", 
                "Newham", "Southwark", "Tower Hamlets", "Wandsworth", "Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Croydon",
                "Ealing", "Enfield", "Greenwich", "Harrow", "Havering", "Hillingdon", "Hounslow", "Kingston upon Thames", "Merton", "Redbridge", "Richmond upon Thames", 
                "Sutton", "Waltham Forest", "Westminster")

lond_la_ids <- las_21_inf$id[las_21_inf$label.en %in% london_las]

lond_la_ids <- data.table(lond_la_ids)


### 9. writing the vector

fwrite(x = lond_la_ids,
       file = "data_resources/london_la_ids_2021.csv")




### 10. getting the meta data for all 2001 local authorities # "NM_1245_1"

las_01_inf <- data.table(nomis_get_metadata(id = "NM_1801_1", # a 2001 census table - any will do 
                                            concept = "geography", # telling it to extract geography information
                                            type = "TYPE464")) # telling it to extract 2001 local authority district information


### 11. extracting the ids for London local authorities only

london_las <- c("Camden", "City of London", "Hackney", "Hammersmith and Fulham", "Haringey", "Islington", "Kensington and Chelsea", "Lambeth", "Lewisham", 
                "Newham", "Southwark", "Tower Hamlets", "Wandsworth", "Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Croydon",
                "Ealing", "Enfield", "Greenwich", "Harrow", "Havering", "Hillingdon", "Hounslow", "Kingston upon Thames", "Merton", "Redbridge", "Richmond upon Thames", 
                "Sutton", "Waltham Forest", "Westminster")

lond_la_ids <- las_01_inf$id[las_01_inf$label.en %in% london_las]

lond_la_ids <- data.table(lond_la_ids)


### 12. writing the vector

fwrite(x = lond_la_ids,
       file = "data_resources/london_la_ids_2001.csv")





## 0. setting up
library(data.table)

source("functions/fn_keep_london.R")

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default"))

## 1. reading in data
reception <- fread("data/other_data/all_reception_state_primary_pupils_201516 to 202223_ed.csv")

myes <- fread("data/other_data/mye_tables_EandW_LAs_20112021.csv")

births <- fread("data/other_data/births_by_mothers_country_of_birth_2001_to_2022.csv")


## 2. cleaning the data

  ### 2.1. cleaning reception first

    #### keeping only London Local Authorities, aggregating to all London
reception <- lond_keep(reception, "new_la_code")

reception <- reception[, .(reception_headcount = sum(headcount)),
                    by = list(time_period)]


    #### creating a new intake year variable
reception[, intake_year := as.numeric(substr(time_period, 1, 4))]

  ### 2.2. cleaning mid year estimates

    #### filtering to age 0
myes <- myes[age == 0, ]

    #### pivoting to long format
id_vars <- c("ladcode21", "laname21", "country", "sex", "age")

myes <- melt(data = myes, id.vars = c("ladcode21", "laname21", "country", "sex", "age"))

myes[, variable := gsub("population_", "", variable, fixed = TRUE)]

myes[, variable := as.numeric(variable)]

    #### keeping only London Local Authorities, then aggregating to all London
myes <- lond_keep(myes, "ladcode21")

myes <- myes[, .(value = sum(value)), 
             by = list(variable)]

colnames(myes) <- c("mye_year", "pop_0")


    #### creating new variable, year plus 4, so that we can match population of year 0. Then getting rid of last 4 rows so that we have the same years of interest for both datasets
myes[, mye_year_plus4 := (mye_year + 4)]

myes <- myes[mye_year_plus4 <= 2022, ]


  ### 2.3. cleaning births

    #### narrowing to just London
births <- births[usual_residence_of_mother == "LONDON",]

    #### keeping only desired columns, renaming as suitable
to_keep <- c("year", "total_births_all")

births <- births[, ..to_keep]

colnames(births) <- c("births_year", "births_count")


    #### creating the new year variable (4 years along), and only keeping the years we want
births[, births_year_plus4 := (births_year + 4)]

births <- births[births_year_plus4 >= 2015 & births_year_plus4 <= 2022]


  ### 2.4. joining the datasets (easier to have them in one)

setkey(reception, "intake_year") ## setting joining key

setkey(myes, "mye_year_plus4") ## setting joining key

setkey(births, "births_year_plus4") ## setting joining key


reception_myes_births <- reception[myes][births] ## joining the three tables


  ### 2.5. creating indexed versions of the variables


reception_15 <- as.numeric(reception_myes_births[1,2])
pop_0_15 <- as.numeric(reception_myes_births[1,5])
births_15 <- as.numeric(reception_myes_births[1,7])


reception_myes_births[, reception_headcount_ind := round(100*(reception_headcount/reception_15),1)] # calculating index for reception headcount
reception_myes_births[, pop_0_ind := round(100*(pop_0/pop_0_15), 1)] # calculating index for population aged 0
reception_myes_births[, births_ind := round(100*(births_count/births_15), 1)]


## 3. making the plots

  ### absolute numbers
jpeg(filename = "output_plots/9_reception_births_population.jpg", 
     width = 10, height = 6, units = "in", res = 850) 

ggplot(data = reception_myes_births) +
  geom_line(aes(x = intake_year, y = pop_0, colour = "Age 0 population 4 years previously"), size = 1) + 
  geom_label_repel() + 
  geom_line(aes(x = intake_year, y = reception_headcount, colour = "Reception headcount"), size = 1) + 
  geom_label_repel() + 
  geom_line(aes(x = intake_year, y = births_count, colour = "Births 4 years previously"), size = 1) + 
  geom_label_repel() + 
  scale_y_continuous(limits = c(90000, 150000)) + 
  labs(title = "Reception headcount compared to births and population 4 years previously",
       caption = "Source: ONS, Chart: GLA Demography")

dev.off()


  ### indexed

    #### getting the positions at the end of the lines, for the labels
label_pos_x1 <- reception_myes_births[length(intake_year), intake_year]
label_pos_y1 <- reception_myes_births[length(pop_0_ind), pop_0_ind]

label_pos_x2 <- reception_myes_births[length(intake_year), intake_year]
label_pos_y2 <- reception_myes_births[length(reception_headcount_ind), reception_headcount_ind]

label_pos_x3 <- reception_myes_births[length(intake_year), intake_year]
label_pos_y3 <- reception_myes_births[length(births_ind), births_ind]

jpeg(filename = "output_plots/10_reception_births_population_indexed.jpg", 
     width = 10, height = 6, units = "in", res = 850) 

ggplot() + 
  geom_line(data = reception_myes_births, aes(x = intake_year, y = pop_0_ind, group = 1), 
            size = 1, colour = pal[7]) + 
  geom_label_repel(aes(x = label_pos_x1, y = label_pos_y1, label = "Lagged population aged 0"), 
                   nudge_x = 1, nudge_y = 0.5, colour = pal[7]) + 
  geom_line(data = reception_myes_births, aes(x = intake_year, y = reception_headcount_ind, group = 1), 
            size = 1, colour = pal[6]) + 
  geom_label_repel(aes(x = label_pos_x2, y = label_pos_y2, label = "Reception headcount"), 
                   nudge_x = 1, nudge_y = 0.5, colour = pal[6]) + 
  geom_line(data = reception_myes_births, aes(x = intake_year, y = births_ind, group = 1), 
            size = 1, colour = pal[9]) + 
  geom_label_repel(aes(x = label_pos_x3, y = label_pos_y3, label = "Lagged birth count"), 
                   nudge_x = 1, nudge_y = 0.5, colour = pal[9]) + 
  scale_y_continuous(limits = c(85, 106)) 

dev.off()





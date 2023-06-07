
##-----------------------------------------------------------------------------------------------------------------------
## load package, function, and data files necessary for analyses 
##------------------------------------------------------------------------------------------------------------------------
########first, load package
library(pacman)
p_load("here", "dplyr", "tidyr", "usdata", "psych", "readr","forecast","lubridate","epitools","readr","reshape","vroom", "forecast")


####load function
source(here("functions/Convert_date_year.R"))
source(here("functions/agg_ten_year.R"))
source(here("functions/Excess_year_stand.R"))
source(here("functions/Excess_year_stand_age.R"))


##load age weight file 
age_weights<- readRDS("population/age_weights.rds")


##-----------------------------------------------------------------------------------------------------------------------
## estimate yearly excess deaths by age group:: results in table 1
##------------------------------------------------------------------------------------------------------------------------
###Estimate excess deaths for each age group
year_df_age <- NULL 
excess_year_stand_age ("65-74")
excess_year_stand_age ("75-84")
excess_year_stand_age ("85+")


###save yearly rate 
saveRDS(year_df_age, here("results/01_data/year_df_age.rds") )


##-----------------------------------------------------------------------------------------------------------------------
## estimate yearly standardized excess deaths for all, by gender, and by race :: results in table 1
##------------------------------------------------------------------------------------------------------------------------
year_df <- NULL
excess_year_stand ("2018-2022_all","1999_2020_all", "ten_year_estimate","All")
excess_year_stand ("2018-2022_gender","1999_2020_gender", "ten_year_estimate","Female")
excess_year_stand ("2018-2022_gender","1999_2020_gender", "ten_year_estimate","Male")
excess_year_stand ("2018-2022_race","1999_2020_race", "ten_year_estimate","White")
excess_year_stand ("2018-2022_race","1999_2020_race", "ten_year_estimate","Black")
excess_year_stand ("2018-2022_race","1999_2020_race", "ten_year_estimate","Hispanic")
excess_year_stand ("2018-2022_race","1999_2020_race", "ten_year_estimate","Asian")

###save yearly rate 
saveRDS(year_df, here("results/01_data/year_df_all_gende_race.rds") )

















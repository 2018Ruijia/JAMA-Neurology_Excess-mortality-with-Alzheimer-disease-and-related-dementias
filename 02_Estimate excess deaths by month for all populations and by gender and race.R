
##-----------------------------------------------------------------------------------------------------------------------
## load package, function, and data files necessary for analyses 
##------------------------------------------------------------------------------------------------------------------------


########first, load package
library(pacman)
p_load("here", "dplyr", "tidyr", "usdata", "psych", "readr","forecast","lubridate","epitools","readr","reshape","vroom")


####load function
source(here("functions/Convert_date_year.R"))
source(here("functions/agg_ten_year.R"))
source(here("functions/Excess_Month.R"))
source(here("functions/Month_stand_data.R"))
source(here("functions/Month_stand_data_not_per_capita.R"))

##load age weight file 
age_weights<- readRDS("population/age_weights.rds")

##-----------------------------------------------------------------------------------------------------------------------
## estimate monthly age-standardized deaths for all, by sex, and by race 
##------------------------------------------------------------------------------------------------------------------------

month_stand_df<-NULL
month_stand_df_not_per_capita<-NULL

#####all_gender
month_stand_data ("2018-2022_all","1999_2020_all", "ten_year_estimate","All")
#month_stand_df_not_per_capita("2018-2022_all","1999_2020_all", "ten_year_estimate","All")

month_stand_data ("2018-2022_gender","1999_2020_gender", "ten_year_estimate","Female")
month_stand_data ("2018-2022_gender","1999_2020_gender", "ten_year_estimate","Male")
#######race
month_stand_data("2018-2022_race","1999_2020_race", "ten_year_estimate","White")
month_stand_data("2018-2022_race","1999_2020_race", "ten_year_estimate","Black")
month_stand_data("2018-2022_race","1999_2020_race", "ten_year_estimate","Hispanic")
month_stand_data("2018-2022_race","1999_2020_race", "ten_year_estimate","Asian")



###save data frame
saveRDS(month_stand_df, here("results/01_data/month_stand_df_all_gender_race.rds") )



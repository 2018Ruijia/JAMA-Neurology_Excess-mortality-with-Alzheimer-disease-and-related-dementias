########first, load package
library(pacman)
p_load("here", "dplyr", "tidyr", "usdata", "psych", "readr","forecast","lubridate","epitools","readr","reshape","vroom", "forecast")


####load function
source(here("functions/Convert_date_year.R"))
source(here("functions/agg_ten_year.R"))
source(here("functions/Excess_year_stand_race_sensitivity analysis.R"))



##estimate excess for race
year_df_race_sen <- NULL
excess_year_stand_race_sen ("2018-2022_race","1999_2020_race", "ten_year_estimate","White")
excess_year_stand_race_sen("2018-2022_race","1999_2020_race", "ten_year_estimate","Black")
excess_year_stand_race_sen("2018-2022_race","1999_2020_race", "ten_year_estimate","Hispanic")
excess_year_stand_race_sen ("2018-2022_race","1999_2020_race", "ten_year_estimate","Asian")
###save yearly rate 
saveRDS(year_df_race_sen, here("results/01_data/year_df_race_sen.rds") )

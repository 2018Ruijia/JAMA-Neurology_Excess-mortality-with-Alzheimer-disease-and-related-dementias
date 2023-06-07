##-----------------------------------------------------------------------------------------------------------------------
## read package 
##------------------------------------------------------------------------------------------------------------------------

##############load the data 
library(dplyr)
library(readr)
library(here)
library(tidyr)
library(here)
library(ggplot2)
library(ggsci)
options(scipen=999)

##-----------------------------------------------------------------------------------------------------------------------
## create function to read data 
##------------------------------------------------------------------------------------------------------------------------

###read single race data function 
read_raw<- function (race) {read_delim ((paste("raw_data/single_race_2018_2020/numerators/",race,".txt", sep="")), show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        filter(is.na(notes))%>% select(-notes) %>% select(month_code,deaths)%>%
        extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE) %>%
        mutate (race=race,
                type="Single Race")}
    

all_single<-rbind(
    read_raw("Latino"),
    read_raw("NH_AIAN"),
    read_raw("NH_Asian"),
    read_raw("NH_Black"),
    read_raw("NH_NHPI"),
    read_raw("NH_White"))

all_single<- all_single %>% mutate (race=ifelse(race=="NH_NHPI"|race=="NH_Asian","NH_Asian",race))

###read bridged race data function 
read_bridge<- function (race) {read_delim ((paste("raw_data/bridge_race_2018_2020/numerators/",race,".txt", sep="")), show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        filter(is.na(notes))%>% select(-notes) %>% select(month_code,deaths)%>%
        extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE) %>%
        mutate (race=race,
                type="Bridged Race")}

all_bridge<-rbind(
    read_bridge("Latino"),
    read_bridge("NH_AIAN"),
    read_bridge("NH_Asian"),
    read_bridge("NH_Black"),
    read_bridge("NH_White"))


##-----------------------------------------------------------------------------------------------------------------------
## create data frame to compare numerators  
##------------------------------------------------------------------------------------------------------------------------
###combine single and bridge data
num_df<- rbind(all_single, all_bridge)

###sum to get yearly estimate for each race
num_df_sum <- num_df %>% group_by(year, type, race) %>% 
       summarize (deaths_year=sum(deaths)) %>% 
       ungroup()

##convert long to wide
num_df_sum2 <- num_df_sum %>% pivot_wider(names_from =type, values_from = deaths_year) %>% 
              dplyr::rename(bridge="Bridged Race",
                     single="Single Race") %>% 
               mutate (percent_diff=(bridge-single)/bridge*100)


###sum the pop across years 
num_df_new <- num_df_sum %>% group_by(type, race) %>% 
             summarize (deaths_year_new=sum(deaths_year)) %>% 
             ungroup()

num_df_new<- num_df_new %>% pivot_wider(names_from =type, values_from = deaths_year_new) %>% 
    dplyr::rename(bridge="Bridged Race",
                  single="Single Race") %>% 
    mutate (percent_diff=(bridge-single)/bridge*100)




num_df$race <- factor(num_df$race, levels=c("Latino","NH_AIAN","NH_Asian","NH_Black","NH_White"),
                   labels=c("Hispanic","Non-Hispanic AIAN","Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic White"))




saveRDS(num_df, 'results/01_data/num_df.RDS')




##-----------------------------------------------------------------------------------------------------------------------
## create data frame to compare denominators
##------------------------------------------------------------------------------------------------------------------------



single_pop<- read_delim ((paste("raw_data/single_race_2018_2020/Single_Race_Population.txt")), show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        filter(is.na(notes))%>% select(-notes) %>% 
        filter(! race=="More than one race") %>% 
        mutate (race_cat= ifelse(ethnicity=="Hispanic or Latino","Hispanic",
                          ifelse(race=="Asian"|race=="Native Hawaiian or Other Pacific Islander","Non-Hispanic Asian",
                          ifelse(race=="Black or African American","Non-Hispanic Black",
                          ifelse(race=="American Indian or Alaska Native","Non-Hispanic AIAN",  
                          ifelse(race=="White","Non-Hispanic White", NA))))),
                type="Single")  %>% 
    select(race_cat,yearly_july_1st_estimates, population,type)     





bridge_pop<- read_delim ((paste("raw_data/bridge_race_2018_2020/Bridged_Race_Population.txt")), show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    filter(is.na(notes))%>% select(-notes) %>% 
    filter(! race=="More than one race") %>% 
    mutate (race_cat= ifelse(ethnicity=="Hispanic or Latino","Hispanic",
                      ifelse(race=="Asian or Pacific Islander","Non-Hispanic Asian",
                      ifelse(race=="Black or African American","Non-Hispanic Black",
                      ifelse(race=="American Indian or Alaska Native","Non-Hispanic AIAN",  
                      ifelse(race=="White","Non-Hispanic White", NA))))),
            type="Bridged")  %>% 
    select(race_cat,yearly_july_1st_estimates, population,type)

deno <- rbind(single_pop, bridge_pop)

deno_df <- deno %>% group_by(race_cat, yearly_july_1st_estimates, type) %>% 
              summarise(pop=sum(population)) %>% 
              dplyr::rename(year=yearly_july_1st_estimates)


deno_df_wide<- deno_df %>% pivot_wider(names_from =type, values_from =pop) %>% 
                      mutate (percent_diff=(Bridged-Single)/Bridged*100)


###sum all year pops together
deno_df2 <- deno_df %>% group_by(race_cat,  type) %>% 
    summarise(pop_new=sum(pop)) %>% 
    ungroup

deno_df2_wide<- deno_df2 %>% pivot_wider(names_from =type, values_from =pop_new) %>% 
                             mutate (percent_diff=(Bridged-Single)/Bridged*100)



deno_df_wide$race_cat<-as.factor(deno_df_wide$race_cat)
deno_df_wide$year<-as.factor(deno_df_wide$year)



saveRDS(deno_df_wide, 'results/01_data/deno_df_wide.RDS')







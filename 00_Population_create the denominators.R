###load packages 
library(here)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

##-----------------------------------------------------------------------------------------------------------------------
## cauclate age weights
##------------------------------------------------------------------------------------------------------------------------
##create age weights- make it as a data frame 
#https://www.cdc.gov/nchs/data/statnt/statnt20.pdf
age_weights <- c(0.066037, 0.044842, 0.015508)
print(paste0('Sum of age weights before re-normalization: ', sum(age_weights)))
# Re-normalization so that the final weights sum up to 1
age_weights <- c(0.066037, 0.044842, 0.015508)/sum(age_weights)
age_weights<- as.data.frame(age_weights)
age_weights$age_gp<- c('65-74', '75-84','85+')

saveRDS(age_weights, here("population/age_weights.rds"))

##-----------------------------------------------------------------------------------------------------------------------
## 2021 population for all and by age group, sex, and race -
##------------------------------------------------------------------------------------------------------------------------
##2021 data downloaded from https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/state/totals/
##data dictionary:https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2021/sc-est2021-alldata6.pdf
##chose mid-year estimate filter to month=7 and age>=65, 999 is to indicate total population
pop_2020_2021<- read.csv(here("population/sc-est2021-alldata6.csv")) %>% filter(AGE>=65 & !AGE==999)

##change the names to lower case
names(pop_2020_2021) <- tolower(names(pop_2020_2021)) 

##categorize age groups
pop_2020_2021 <- pop_2020_2021 %>%  mutate (age_group=ifelse(age>=65 & age <=74, "65-74",
                                                      ifelse(age>=75 & age <=84, "75-84",  
                                                      ifelse(age>=85, "85+", NA))),
                                          sex=ifelse (sex==0, "All",
                                              ifelse (sex==1, "Male",
                                              ifelse (sex==2, "Female", sex))),
                                          origin=ifelse (origin==0, "All",
                                                 ifelse (origin==1, "Not Hispanic",
                                                 ifelse (origin==2, "Hispanic", origin)))) 
    
###recode race    
pop_2020_2021 <- pop_2020_2021 %>% mutate (race=ifelse (origin=="Hispanic", "Hispanic",
                                                ifelse (origin=="Not Hispanic" & race==1, "White",
                                                ifelse (origin=="Not Hispanic" & race==2, "Black",
                                                ifelse (origin=="Not Hispanic" & race==3, "AIAN",               
                                                ifelse (origin=="Not Hispanic" & (race==4|race==5), "Asian", race))))))     

#############calculate population by sex in 2021
  pop_2021_sex<- pop_2020_2021 %>% filter(origin=="All") %>%  group_by(age_group, sex)  %>% 
                                   mutate (tot_pop_age=sum(popestimate2021))%>% 
                                   filter(name=="Alabama")

  pop_2021_sex <-pop_2021_sex[!duplicated(pop_2021_sex$tot_pop_age), ]
  pop_2021_sex <- pop_2021_sex %>% select(sex, age_group, tot_pop_age) %>% mutate (year="2021")

  ####transpose the data: convert from long to wide format 
  pop_2021_sex<-  pivot_wider (pop_2021_sex,
            id_cols=c(age_group, year),
            names_from =sex,
            values_from= tot_pop_age)
  
  
  
#############calculate population by race 
  pop_2021_race<- pop_2020_2021  %>%  filter(!origin=="All" & sex=="All" & !race==6)%>% group_by(age_group, race) %>% 
                                      mutate (tot_pop_age=sum(popestimate2021))%>% 
                                      filter(name=="Alabama")
  
  pop_2021_race <-pop_2021_race[!duplicated(pop_2021_race$tot_pop_age), ]
  pop_2021_race <- pop_2021_race %>% filter(!origin=="All") %>%  select(race, age_group, tot_pop_age) %>% mutate (year="2021")
  
  ####transpose the data
  pop_2021_race<-  pivot_wider (pop_2021_race,
                               id_cols=c(age_group, year),
                               names_from =race,
                               values_from= tot_pop_age)
  
  pop_2021 <- pop_2021_sex %>% left_join (pop_2021_race, by=c("year","age_group"))
  
###save the data 
saveRDS(pop_2021, here("population/pop_2021.rds"))

##-----------------------------------------------------------------------------------------------------------------------
## 2020 population for all and by age group, sex, and race -
##-----------------------------------------------------------------------------------------------------------------------
###calculate 2020 population 
pop_2020_sex<- pop_2020_2021 %>% filter(origin=="All") %>% group_by(age_group, sex) %>% 
    mutate (tot_pop_age=sum(popestimate2020))%>% 
    filter(name=="Alabama")

pop_2020_sex <-pop_2020_sex[!duplicated(pop_2020_sex$tot_pop_age), ]
pop_2020_sex <- pop_2020_sex %>% select(sex, age_group, tot_pop_age) %>% mutate (year="2020")

####transpose the data
pop_2020_sex<-  pivot_wider (pop_2020_sex,
                             id_cols=c(age_group, year),
                             names_from =sex,
                             values_from= tot_pop_age)

#############calculate population by race 
pop_2020_race<- pop_2020_2021  %>% filter(!origin=="All" & sex=="All" & !race==6)%>% group_by(age_group, race) %>% 
    mutate (tot_pop_age=sum(popestimate2020))%>% 
    filter(name=="Alabama")

pop_2020_race <-pop_2020_race[!duplicated(pop_2020_race$tot_pop_age), ]
pop_2020_race <- pop_2020_race %>% filter(!origin=="All") %>%  select(race, age_group, tot_pop_age) %>% mutate (year="2020")

####transpose the data
pop_2020_race<-  pivot_wider (pop_2020_race,
                              id_cols=c(age_group, year),
                              names_from =race,
                              values_from= tot_pop_age)

pop_2020 <- pop_2020_sex %>% left_join (pop_2020_race, by=c("year","age_group"))

###save the data 
saveRDS(pop_2020, here("population/pop_2020.rds"))


##-----------------------------------------------------------------------------------------------------------------------
## 2022 population based on 2021 data 
##-----------------------------------------------------------------------------------------------------------------------
###save 2022 data 
pop_2022 <- pop_2021 %>% mutate (year="2022")

##bind two datasets 
pop_2020_2021_2022<-rbind(pop_2022, pop_2021, pop_2020)

###save all pop
saveRDS(pop_2020_2021_2022, here("population/pop_2020_2021_2022.rds"))

##-----------------------------------------------------------------------------------------------------------------------
## read population by state
##-----------------------------------------------------------------------------------------------------------------------
###2021 state population was downloaded from https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/state/totals/

state_pop_2021<- read.csv(here("population/state_pop_2021.csv"))

names(state_pop_2021)<-tolower(names(state_pop_2021))

state_pop <- state_pop_2021 %>% select(name, popestimate2020,popestimate2021)

saveRDS(state_pop, here("population/state_pop.rds"))


















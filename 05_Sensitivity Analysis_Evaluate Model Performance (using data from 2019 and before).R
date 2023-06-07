
##-----------------------------------------------------------------------------------------------------------------------
## load package, function, and data files necessary for analyses 
##------------------------------------------------------------------------------------------------------------------------
library(pacman)
p_load("here", "dplyr", "tidyr", "usdata", "psych", "readr","forecast","lubridate","epitools","readr","reshape","vroom")

##load age weight file 
age_weights<- readRDS("population/age_weights.rds")

source("functions/Excess_year_stand_age_previous_years.R")



##-----------------------------------------------------------------------------------------------------------------------
## function for estimate excess death for 2019 and before 
##------------------------------------------------------------------------------------------------------------------------


excess_year_stand_2019<-function(y, year, group,forecast.start=as.Date('2019-01-01'),
                            forecast.window=12){ 
    
    ###edit population: use 2020 pop for year 1 and 2021 pop for year 2
    pop_2019 <- read_delim(paste("population/2014-2020_Bridged_Pop/", group, ".txt", sep=""), show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    filter(is.na(notes))%>% select(-notes) %>% 
    select(age_group, yearly_july_1st_estimates, population) %>% filter(yearly_july_1st_estimates=="2019") %>% 
    mutate (age_group=ifelse(age_group=="65-69 years"|age_group=="70-74 years", "65-74",
                              ifelse(age_group=="75-79 years"|age_group=="80-84 years", "75-84", "85+"))) %>% 
    group_by(age_group) %>% 
    mutate(pop=sum(population)) %>% 
    select(-population) %>% 
    dplyr::rename(age_gp=age_group) %>% 
    ungroup
    pop_2019 <-pop_2019[!duplicated(pop_2019$pop),] 
    
    
    
    #read 2014-2020 data - select 2014 to 2020 data 
    df_2014_2019<-read_delim(here(paste("raw_data/",y,"/",year,"/",group,".txt", sep="")), show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths, ten_year_age_groups)%>%
        extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE)%>% filter(year>=2014 & year <= 2019)
    
    
    ####convert date 
    df_2014_2019<- convert_date(df_2014_2019)
    
    ###merge pre and post pandemic data together 
    month_df<- df_2014_2019 %>% mutate(date=as.Date(date)) %>% arrange(date)
    
    ##yearly deaths
    month_df$ten_year_age_groups<-gsub(" years","",as.character(month_df$ten_year_age_groups))
    month_df$deaths<-as.numeric(month_df$deaths)
    
    month_df<- month_df %>% arrange(date)
    
    
    est_df <- NULL
    sim_ci <-NULL
    
    
    # fit model
    sim_age_df<- function (age_group) { 
        
        data <- month_df %>% filter(ten_year_age_groups==age_group) %>% 
                            arrange(date)
        
        
        age_weights_age<- age_weights %>% filter(age_gp==age_group)
        

        
        
        #######estimate expected deaths 

        
        tt<-ts(data[data$date<(forecast.start),"deaths"],deltat=1/12,
               start=c(2014))
        
        
        # # obtain forecasts
        # ff<-forecast(mm,xreg=fourier(tt,K=k.best,h=forecast.window))
        mm<-auto.arima(tt)
        # # # # obtain forecasts
        ff<-forecast(mm,h=forecast.window)
        
        
        # extract observed values
        
        # extract observed values
        rr<-data$date[(data$date>=forecast.start)]
        rr<-rr[1:forecast.window]
        oo<-data[is.element(data$date,rr),c('date','deaths')]
        names(oo)[2]<-'observed'
        # extract expected values
        ee<-data.frame(
            date=rr,
            expected=as.numeric(ff$mean),
            expected.lower=as.numeric(ff$lower[,'95%']),
            expected.upper=as.numeric(ff$upper[,'95%'])
        )
        # define and output combined data, with observed, expected, and excess
        oo$date<-as.character(oo$date)
        ee$date<-as.character(ee$date)
        aa<-merge(oo,ee,by='date',all.x=FALSE,all.y=FALSE)
        # View(bb)
        # saveRDS(bb,ff)
        oo$date<-as.Date(oo$date,'%Y-%m-%d')
        ee$date<-as.Date(ee$date,'%Y-%m-%d')
        aa$age_gp<-age_group
        pop_year_age<- pop_2019 %>% filter(age_gp==age_group) 
        bb<-aa %>% left_join (pop_year_age, by=c("age_gp")) %>% 
            left_join (age_weights_age) %>% 
            mutate (observed_adjusted=observed/pop*100000*age_weights,
                    expected_adjusted=expected/pop*100000*age_weights)
        
        est_df<<-rbind(bb,est_df)
        # obtain prediction intervals for totals
        set.seed(94118)
        NN<-10000
        SS<-NULL
        for(ii in 1:NN){
            sim.i<-simulate(mm,future=TRUE,nsim=forecast.window)
            SS.i<-data.frame(pt=sum(sim.i))
            SS<-rbind(SS,SS.i)
            
        }
        SS$age_gp<- age_group
        
        SS<- SS %>% left_join (age_weights_age) %>% 
                   left_join (pop_year_age) %>% 
            mutate (
                pt_adjusted=pt/pop*100000*age_weights,
                sim_num=seq(1:10000))
        sim_ci <<- rbind(sim_ci, SS)
    }
    sim_age_df ("65-74")
    sim_age_df ("75-84")
    sim_age_df ("85+")
    
    sim_ci_df <- sim_ci %>% group_by (sim_num)  %>% 
                mutate(pt_sum_adjusted=sum(pt_adjusted)) %>% 
                select(pt_sum_adjusted, sim_num)
    
    sim_ci_df <- unique(sim_ci_df)
    
    # store results
    delta<-data.frame(
        # define results for 1st period
        pt.observed=sum(est_df$observed_adjusted),
        pt.expected=sum(est_df$expected_adjusted),
        pt.expected.lower=as.numeric(quantile(sim_ci_df$pt_sum_adjusted,0.025)),
        pt.expected.upper=as.numeric(quantile(sim_ci_df$pt_sum_adjusted,0.975))
        
    )
    
    delta$group<- group
    
    year_df_2019 <<-rbind(year_df_2019,delta)
    
}


##-----------------------------------------------------------------------------------------------------------------------
## estimate excess deaths
##------------------------------------------------------------------------------------------------------------------------
year_df_2019 <- NULL
excess_year_stand_2019 ("1999_2020_all", "ten_year_estimate","All")
excess_year_stand_2019("1999_2020_gender", "ten_year_estimate","Female")
excess_year_stand_2019 ("1999_2020_gender", "ten_year_estimate","Male")
excess_year_stand_2019 ("1999_2020_race", "ten_year_estimate","White")
excess_year_stand_2019("1999_2020_race", "ten_year_estimate","Black")
excess_year_stand_2019("1999_2020_race", "ten_year_estimate","Hispanic")
excess_year_stand_2019("1999_2020_race", "ten_year_estimate","Asian")

year_df_age_previous <- NULL
excess_year_stand_age_previous ("65-74")
excess_year_stand_age_previous ("75-84")
excess_year_stand_age_previous ("85+")

saveRDS(year_df_2019, here("results/01_data/year_df_rate_all_gender_race_2019.rds") )










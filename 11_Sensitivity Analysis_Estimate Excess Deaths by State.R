#########load packages 
library(pacman)
p_load("here", "dplyr", "tidyr", "usdata", "psych", "readr","forecast","lubridate","epitools","tidyr",
       "reshape","vroom","usdata")

source(here("functions/agg_ten_year.r"))
source(here("functions/Excess_Month.R"))
source(here("functions/Month_stand_data.R"))





####convert date format function 
year_month_day <- function(x){
    y <- strsplit(x,"/")[[1]][2]
    return(y)
}

year_str <- function(x){
    return(str(x))
}

convert_date<-  function (dat) {
    dat$month_alone <- sapply(dat$month_code,year_month_day)
    dat$day <- rep("01",nrow(dat))
    dat$date <- paste(dat$year,dat$month_alone, dat$day,sep="-")
    return(dat)}

#read state population
state_pop<-readRDS("population/pop_state_total.rds") %>% 
    dplyr::rename("2020"="state_pop_2020_total",
                  "2021"="state_pop_2021_total",
                  "2022"="state_pop_2022_total")

###convert data wide to long
state_pop_long <- pivot_longer(state_pop,
                               "2020":"2022",
                               names_to = "year",
                               values_to = "population")

###read data 
df_2021<- read_delim ("raw_data/2018_2022_state/2018 through Last Month_not_by_five_year.txt", show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,residence_state,deaths)%>%
    extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE)

df_2020<- read_delim ("raw_data/1999_2020_state/1999-2020_state_not_by_five_year.txt", show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,state,deaths)%>%
        extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE)

df_2021<- df_2021 %>% 
    mutate (date=paste(month_code, "/01", sep="")) %>% 
    filter(date>=as.Date("2021/01/01") & date<=as.Date("2022/02/01"))%>% dplyr::rename(state=residence_state) 

df_2014_2020<- df_2020%>% filter(year>=2014 & year<=2020)

df_2021<- convert_date(df_2021)
df_2014_2020<- convert_date(df_2014_2020)

all<-rbind(df_2014_2020, df_2021) %>% 
    mutate(date=as.Date(date))

RR<-NULL
month_oe<-NULL

######################################################################calculate state monthly deaths
state_month_excess<-  function(x) {

    
    month_df<-all %>% filter(state==x) %>% arrange(date)
    yy="deaths"
    forecast.start=as.Date('2020-03-01')
    forecast.window=24
    # store forecast window
    fw<<-forecast.window
    # define data
    mm<-auto.arima(tt)
    # # # # obtain forecasts
    ff<-forecast(mm,h=forecast.window)
    # extract observed values
    rr<-month_df$date[(month_df$date>=forecast.start)]
    rr<-rr[1:forecast.window]
    oo<-month_df[is.element(month_df$date,rr),c('date',yy)]
    names(oo)[2]<-'observed'
    ee<-data.frame(
        date=seq(forecast.start, by = "month", length.out =24),
        expected=as.numeric(ff$mean),
        expected.lower=as.numeric(ff$lower[,'95%']),
        expected.upper=as.numeric(ff$upper[,'95%'])
        
    )
    # define and output combined data, with observed, expected, and excess
    oo$date<-as.character(oo$date)
    ee$date<-as.character(ee$date)
    
    oo$state<-x
    oe<-oo %>% left_join(ee,by="date")
    
    
    month_oe<<-rbind(month_oe,oe)
}

state_names<- unique(all[['state']])

lapply(state_names,state_month_excess)



##combine the two population df
##extract year 
month_oe$year <- year(month_oe$date)
month_oe$year <- as.character(month_oe$year)


##combine state population and death data

state_all<- month_oe %>% left_join(state_pop_long, by=c("state", "year"))%>%
            mutate (observed_pop=observed/population*100000,
                    expected_pop=expected/population*100000,
                    expected.lower.pop=expected.lower/population*100000,
                    expected.upper.pop=expected.upper/population*100000,
                    excess=observed-expected,
                    excess_lower=observed-expected.upper,
                    excess_upper=observed-expected.lower,
                    excess_pop=excess/population*100000,
                    excess_lower_pop=excess_lower/population*100000,
                    excess_upper_pop=excess_upper/population*100000)

saveRDS(state_all, 'results/01_data/state_non_stand_month_df.RDS')


###########################################calculate state yearly deaths###############################################################################
#######note: this is the main analysis and is not age adjusted 


state_year_df <- NULL

state_year_excess<-function(x, forecast.start=as.Date('2020-03-01'),
                         forecast.window=24){ 
  yy="deaths"

    data <- all %>% filter(state==x) %>% 
                    arrange(date)
    
    
    # fit model
    tt<-ts(data[data$date<(forecast.start),"deaths"],deltat=1/12,
           start=c(2014))
    
    # define periods
    unique.dates<-as.Date(sort(unique(data$date)),'%Y-%m-%d')
    unique.dates<-subset(unique.dates,unique.dates>=as.Date('2020-03-01'))
    p1<-1:which(unique.dates==as.Date('2021-02-01'))
    p2<-which(unique.dates==as.Date('2021-03-01')):
        which(unique.dates==as.Date('2022-02-01'))
    
    
    # # obtain forecasts
    # ff<-forecast(mm,xreg=fourier(tt,K=k.best,h=forecast.window))
    mm<-auto.arima(tt)
    # # # # obtain forecasts
    ff<-forecast(mm,h=forecast.window)
    # extract observed values
    
    # extract observed values
    rr<-data$date[(data$date>=forecast.start)]
    rr<-rr[1:forecast.window]
    oo<-data[is.element(data$date,rr),c('date',yy)]
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
    bb<-merge(oo,ee,by='date',all.x=FALSE,all.y=FALSE)
    # saveRDS(bb,ff)
    oo$date<-as.Date(oo$date,'%Y-%m-%d')
    ee$date<-as.Date(ee$date,'%Y-%m-%d')
    # obtain prediction intervals for totals
    set.seed(94118)
    NN<-10000
    SS<-NULL
    for(ii in 1:NN){
        sim.i<-simulate(mm,future=TRUE,nsim=forecast.window)
        SS.i<-data.frame(pt=sum(sim.i),p1=sum(sim.i[p1]),
                         p2=sum(sim.i[p2]))
        SS<-rbind(SS,SS.i)
    }
    
    # store results
    delta<-data.frame(
        
        # define results for 1st period
        p1.observed=sum(bb$observed[p1]),
        p1.expected=sum(bb$expected[p1]),
        p1.expected.lower=as.numeric(quantile(SS$p1,0.025)),
        p1.expected.upper=as.numeric(quantile(SS$p1,0.975)),
        p1.excess=sum(bb$observed[p1]-bb$expected[p1]),
        p1.excess.lower=sum(bb$observed[p1])-as.numeric(quantile(SS$p1,0.975)),
        p1.excess.upper=sum(bb$observed[p1])-as.numeric(quantile(SS$p1,0.025)),
        # define results for 2nd period
        p2.observed=sum(bb$observed[p2]),
        p2.expected=sum(bb$expected[p2]),
        p2.expected.lower=as.numeric(quantile(SS$p2,0.025)),
        p2.expected.upper=as.numeric(quantile(SS$p2,0.975)),
        p2.excess=sum(bb$observed[p2]-bb$expected[p2]),
        p2.excess.lower=sum(bb$observed[p2])-as.numeric(quantile(SS$p2,0.975)),
        p2.excess.upper=sum(bb$observed[p2])-as.numeric(quantile(SS$p2,0.025))
        
    )

    delta$state<- x
    delta_long<-delta %>%
        pivot_longer(-state,
                     names_to = c("Year", ".value"),
                     names_pattern = "([[:alnum:]]+)\\.(.+)")%>%
        mutate (Year=ifelse(Year=="p1","Year 1", "Year 2"))
    ###read population 
    ###edit population: use 2020 pop for year 1 and 2021 pop for year 2
    pop_year <- state_pop_long %>%  filter(state==x & !year=="2022") %>% 
        mutate (Year=ifelse(year=="2020", "Year 1", "Year 2")) %>% 
        select(-year) 
    delta_long_pop <-   delta_long %>% left_join(pop_year, by="Year")
    
    
    
    delta_rate<-  delta_long_pop %>%  mutate (observed_capita= observed/population*100000,
                                               expected_capita=expected/population*100000,
                                               expected.lower_capita=expected.lower/population*100000,
                                               expected.upper_capita=expected.upper/population*100000,
                                               excess_capita=excess/population*100000,
                                               excess.lower_capita=excess.lower/population*100000,
                                               excess.upper_capita=excess.upper/population*100000)
    
    state_year_df <<-rbind(state_year_df,delta_rate)
    
}


state_names<- unique(all[['state']])

lapply(state_names,state_year_excess)

saveRDS(state_year_df, 'results/01_data/state_non_stand_year_df.RDS')








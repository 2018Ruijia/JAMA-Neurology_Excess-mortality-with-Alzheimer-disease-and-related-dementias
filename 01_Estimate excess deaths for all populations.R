
########first, load package
library(pacman)
p_load("here", "dplyr", "tidyr", "usdata", "psych", "readr","forecast","lubridate","epitools","readr","reshape","vroom")


##-----------------------------------------------------------------------------------------------------------------------
## calculating the total number of deaths 
##------------------------------------------------------------------------------------------------------------------------


######################calculate the total number of pre and post pandemic deaths 

df_2021_2022 <-read_delim(here(paste("raw_data/","2018-2022_all","/","ten_year_estimate","/","All",".txt", sep="")), show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths, ten_year_age_groups)%>%
  extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE) %>% 
  mutate (date=paste(month_code, "/01", sep="")) %>% 
  filter(date>=as.Date("2021/01/01") & date<=as.Date("2022/02/01"))


#read 2014-2020 data - select 2014 to 2020 data 
df_2014_2020<-read_delim(here(paste("raw_data/","1999_2020_all","/","ten_year_estimate","/","All",".txt", sep="")), show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths, ten_year_age_groups)%>%
  extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE)%>% filter(year>=2014 & year <= 2020)


####convert date 
df_2021_2022<- convert_date(df_2021_2022)
df_2014_2020<- convert_date(df_2014_2020)

###merge pre and post pandemic data together 
total<-rbind(df_2021_2022, df_2014_2020) %>% mutate(date=as.Date(date))


#####define pre and postdoc 
pre_df <- total %>%  filter (date< as.Date("2020/03/01"))%>% arrange(date)
post_df <- total %>%  filter (date>= as.Date("2020/03/01")) %>% 
  mutate (Year=ifelse (date < as.Date("2021/03/01"), "Year 1", "Year 2")) %>% 
  arrange(date)

sum(pre_df$deaths) 

aggregate(deaths~Year, data=post_df, sum)

forecast.start=as.Date('2020-03-01')
forecast.window=24

##-----------------------------------------------------------------------------------------------------------------------
## estimate the excess deaths for all--- did not consider population size 
##------------------------------------------------------------------------------------------------------------------------
     #read 2021-2022 data
    df_2021_2022 <-read_delim(here(paste("raw_data/","2018-2022_all","/","not_by_age","/","All",".txt", sep="")), show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths)%>%
        extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE) %>% 
        mutate (date=paste(month_code, "/01", sep="")) %>% 
        filter(date>=as.Date("2021/01/01") & date<=as.Date("2022/02/01"))
    
    
    #read 2014-2020 data - select 2014 to 2020 data 
    df_2014_2020<-read_delim(here(paste("raw_data/","1999_2020_all","/","not_by_age","/","All",".txt", sep="")), show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths)%>%
        extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE)%>% filter(year>=2014 & year <= 2020)
    
    
    ####convert date 
    df_2021_2022<- convert_date(df_2021_2022)
    df_2014_2020<- convert_date(df_2014_2020)
    
    ###merge pre and post pandemic data together 
    month_df<-rbind(df_2021_2022, df_2014_2020) %>% mutate(date=as.Date(date))
    
    ##yearly deaths
   # month_df$ten_year_age_groups<-gsub(" years","",as.character(month_df$ten_year_age_groups))
    month_df$deaths<-as.numeric(month_df$deaths)
    
    data<- month_df %>% arrange(date)
    
    #######estimate expected deaths 
    # define periods
    unique.dates<-as.Date(sort(unique(data$date)),'%Y-%m-%d')
    unique.dates<-subset(unique.dates,unique.dates>=as.Date('2020-03-01'))
    p1<-1:which(unique.dates==as.Date('2021-02-01'))
    p2<-which(unique.dates==as.Date('2021-03-01')):
        which(unique.dates==as.Date('2022-02-01'))
    
    tt<-ts(data[data$date<(forecast.start),"deaths"],deltat=1/12,
           start=c(2014))
    
    
    # # obtain forecasts
    # ff<-forecast(mm,xreg=fourier(tt,K=k.best,h=forecast.window))
    mm<-auto.arima(tt)
    # # # # obtain forecasts
    ff<-forecast(mm,h=forecast.window)
    
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
    # saveRDS(bb,ff)
    oo$date<-as.Date(oo$date,'%Y-%m-%d')
    ee$date<-as.Date(ee$date,'%Y-%m-%d')
   # aa$age_gp<-age_group
    aa<-aa %>% mutate(Year=ifelse (date>as.Date("2020-02-01") & date<as.Date("2021-03-01"), "Year 1", 
                                   ifelse (date>as.Date("2021-02-01") & date<as.Date("2022-03-01"), "Year 2", NA)))
    
    bb<-aa  %>% 
        mutate (observed_adjusted=observed,
                expected_adjusted=expected)

#####save the data 
    saveRDS(file="results/01_data/total_ADRD_deaths.rds",bb)


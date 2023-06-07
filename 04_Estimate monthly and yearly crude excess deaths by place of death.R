
##-----------------------------------------------------------------------------------------------------------------------
## load package, function, and data files necessary for analyses 
##------------------------------------------------------------------------------------------------------------------------
####load package
library(pacman)
p_load("here", "dplyr", "tidyr", "usdata", "psych", "readr","forecast","lubridate","epitools","readr","reshape","vroom")

####load function
source(here("functions/Convert_date_year.r"))
source(here("functions/agg_ten_year.r"))
source(here("functions/Excess_Month.r"))


##load age weight file 
age_weights<- readRDS("population/age_weights.rds")



##-----------------------------------------------------------------------------------------------------------------------
## estimate monthly excess deaths for place of death
##------------------------------------------------------------------------------------------------------------------------

###at the beginning, set df as null
pod_crude_df<-NULL


pod_crude<-  function (x) {


    #read 2018-2022 single race data
    df_2021 <-read_delim(here(paste("raw_data/2018_2022_POD/crude/",x,".txt", sep="")), show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths)%>%
        extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE)%>%
        mutate (date=paste(month_code, "/01", sep="")) %>%
        filter(date>=as.Date("2021/01/01") & date<=as.Date("2022/02/01"))


    #read 2014-2017  bridged race data
    df_2014_2020<-read_delim(here(paste("raw_data/1999_2020_pod/crude/",x,".txt", sep="")), show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths)%>%
        extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE)%>% filter(year>=2014 & year <= 2020)


    ####convert date in the data
    df_2021<- convert_date(df_2021)
    df_2014_2020<- convert_date( df_2014_2020)

    ###merge data
    month_df<-rbind(df_2021,  df_2014_2020) %>%
              mutate(date=as.Date(date)) %>% 
              arrange(date)
    month_df$deaths<- as.numeric(month_df$deaths)

    ###calculate excess deaths
    pod_month_df<- visualize_forecast_group(month_df, "deaths")
    pod_month_df$pod<- x
    pod_crude_df<<-rbind(pod_crude_df, pod_month_df)

    return(pod_crude_df)

}

###calculate excess deaths in each setting and combine them together
pod_crude("Home")
pod_crude("Nursing Home")
pod_crude("Medical Facilities")
pod_crude("Hospice")

saveRDS(pod_crude_df, here("results/01_data/pod_crude_month_df.rds") )



##-----------------------------------------------------------------------------------------------------------------------
## estimate year excess deaths for place of death
##------------------------------------------------------------------------------------------------------------------------
# ###############################calculate POD yearly deaths
pod_year_df<-NULL
pod_year_non_stand<-function(x, forecast.start=as.Date('2020-03-01'),
                                forecast.window=24){

    #read 2018-2022 single race data
    df_2021 <-read_delim(here(paste("raw_data/2018_2022_POD/crude/",x,".txt", sep="")), show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths)%>%
        extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE)%>%
        mutate (date=paste(month_code, "/01", sep="")) %>%
        filter(date>=as.Date("2021/01/01") & date<=as.Date("2022/02/01"))


    #read 2014-2017  bridged race data
    df_2014_2020<-read_delim(here(paste("raw_data/1999_2020_pod/crude/",x,".txt", sep="")), show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths)%>%
        extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE)%>% filter(year>=2014 & year <= 2020)


    ####convert date in the data
    df_2021<- convert_date(df_2021)
    df_2014_2020<- convert_date( df_2014_2020)

    ###merge data
    data<-rbind(df_2021, df_2014_2020) %>%
              mutate(date=as.Date(date)) %>%
              arrange(date)

    data$deaths<- as.numeric(data$deaths)
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
    bb<-merge(oo,ee,by='date',all.x=FALSE,all.y=FALSE)
    bb<-bb%>% mutate(Year=ifelse (date>as.Date("2020-02-01") & date<as.Date("2021-03-01"), "Year 1",
                          ifelse (date>as.Date("2021-02-01") & date<as.Date("2022-03-01"), "Year 2", NA)))

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
    # store results
    delta<-data.frame(
        # define results for 1st period
        p1.observed=sum(bb$observed[bb$Year=="Year 1"]),
        p1.expected=sum(bb$expected[bb$Year=="Year 1"]),
        p1.expected.lower=as.numeric(quantile(SS$p1,0.025)),
        p1.expected.upper=as.numeric(quantile(SS$p1,0.975)),
        p1.excess=sum(bb$observed[bb$Year=="Year 1"]-bb$expected[bb$Year=="Year 1"]),
        p1.excess.lower=sum(bb$observed[bb$Year=="Year 1"])-as.numeric(quantile(SS$p1,0.975)),
        p1.excess.upper=sum(bb$observed[bb$Year=="Year 1"])-as.numeric(quantile(SS$p1,0.025)),
        # define results for 2nd period
        p2.observed=sum(bb$observed[bb$Year=="Year 2"]),
        p2.expected=sum(bb$expected[bb$Year=="Year 2"]),
        p2.expected.lower=as.numeric(quantile(SS$p2,0.025)),
        p2.expected.upper=as.numeric(quantile(SS$p2,0.975)),
        p2.excess=sum(bb$observed[bb$Year=="Year 2"]-bb$expected[bb$Year=="Year 2"]),
        p2.excess.lower=sum(bb$observed[bb$Year=="Year 2"])-as.numeric(quantile(SS$p2,0.975)),
        p2.excess.upper=sum(bb$observed[bb$Year=="Year 2"])-as.numeric(quantile(SS$p2,0.025))

    )

    delta$group<-x
    delta_long<-delta %>% pivot_longer(-group,
                     names_to = c("Year", ".value"),
                     names_pattern = "([[:alnum:]]+)\\.(.+)")%>%
                     mutate (Year=ifelse(Year=="p1","Year 1", "Year 2"))
    pod_year_df <<- rbind(delta_long, pod_year_df)

}



pod_year_non_stand("Home")
pod_year_non_stand("Nursing Home")
pod_year_non_stand("Medical Facilities")
pod_year_non_stand("Hospice")
pod_year_non_stand("Other")


saveRDS(pod_year_df, here("results/01_data/pod_crude_year_df.rds") )


##-----------------------------------------------------------------------------------------------------------------------
## load package, function, and data files necessary for analyses 
##------------------------------------------------------------------------------------------------------------------------


options(scipen=999)
library(pacman)
library(flextable)
p_load("here", "dplyr", "tidyr", "usdata", "psych", "readr","forecast","lubridate","epitools","readr","reshape","vroom", "stringr", 
       "ggsci","officer","here","purrr","ggplot2")

####load function
source(here("functions/Convert_date_year.r"))
source(here("functions/agg_ten_year.r"))
source(here("functions/Excess_Month.r"))


##-----------------------------------------------------------------------------------------------------------------------
## read data
##------------------------------------------------------------------------------------------------------------------------


################read the number of nursing home residents from 2019-2022

read_csv_files <- function(folder_name, file_pattern) {
    # get list of all files in folder that match the pattern
    file_names <- list.files(path = folder_name, pattern = file_pattern, full.names = TRUE)
    
    # read each file and add month and year variables based on file name
    dfs <- lapply(file_names, function(file) {
        month <- str_extract(file, "(?<=_)[0-9]{2}(?=_)")
        year <- str_extract(file, "(?<=_)\\d{4}")
        df <- read.csv(file) 
    ###select columns 
        select_vars <- if ("RESTOT" %in% colnames(df)) {
            c("RESTOT", 
              if ("Federal.Provider.Number" %in% colnames(df)) {
                  "Federal.Provider.Number"
              } else {
                  "PROVNUM"
              })
        } else {
            c("Average.Number.of.Residents.per.Day", 
              if ("Federal.Provider.Number" %in% colnames(df)) {
                  "Federal.Provider.Number"
              } else {
                  "PROVNUM"
              })
        }

        df <- df[, select_vars]
        colnames(df)[1] <- "RESTOT" 
        colnames(df)[2] <- "PROVNUM" 
        
        
        df$month <- month
        df$year <- year
        
        df$date <- as.Date(paste(df$year, df$month, "01", sep = "-"))
        

        return(df)
    })
    
    # combine all data frames into a single data frame
    combined_df <- do.call(rbind, dfs)
    
    
    # if the number in a month is missing, I use that provider's average available number in other months of a year.    
    combined_df <- combined_df %>%
        group_by(PROVNUM) %>%
        mutate(RESTOT = ifelse(is.na(RESTOT), mean(RESTOT, na.rm = TRUE), RESTOT)) %>%
        mutate(RESTOT = ifelse(is.nan(RESTOT), mean(RESTOT, na.rm = TRUE), RESTOT)) %>%
        ungroup()
    
    return(combined_df)
}

nursing_2019 <- read_csv_files("raw_data/Nursing Home Number/2019", "_2019.csv")
nursing_2020 <- read_csv_files("raw_data/Nursing Home Number/2020", "_2020.csv")
nursing_2021 <- read_csv_files("raw_data/Nursing Home Number/2021", "_2021.csv")
nursing_2022 <- read_csv_files("raw_data/Nursing Home Number/2022", "_2022.csv")


###change the id formats for some of the character ids 
nursing_2019$ID_numeric <- as.numeric(gsub("[^0-9]", "", nursing_2019$PROVNUM))
nursing_2020$ID_numeric <- as.numeric(gsub("[^0-9]", "", nursing_2020$PROVNUM))
nursing_2021$ID_numeric <- as.numeric(gsub("[^0-9]", "", nursing_2021$PROVNUM))
nursing_2022$ID_numeric <- as.numeric(gsub("[^0-9]", "", nursing_2022$PROVNUM))

 # Get the unique IDs in each data frame
ids_2019 <- unique(nursing_2019$ID_numeric)
ids_2020 <- unique(nursing_2020$ID_numeric)
ids_2021 <- unique(nursing_2021$ID_numeric)
ids_2022 <- unique(nursing_2022$ID_numeric)

###identify the ids that are included in all files 
included_in_all <- reduce(list(ids_2019, ids_2020, ids_2021, ids_2022), intersect)

# Filter the data frames to include only the PROVNUM values that are in included_in_all
nursing_2019_filtered <- nursing_2019[nursing_2019$ID_numeric %in% included_in_all, ]
nursing_2020_filtered <- nursing_2020[nursing_2020$ID_numeric %in% included_in_all, ]
nursing_2021_filtered <- nursing_2021[nursing_2021$ID_numeric %in% included_in_all, ]
nursing_2022_filtered <- nursing_2022[nursing_2022$ID_numeric %in% included_in_all, ]

# Combine the filtered data frames
nursing_combined <- rbind(nursing_2019_filtered, nursing_2020_filtered, nursing_2021_filtered, nursing_2022_filtered)

###count the number of ids that occured at least once 
unique_ids <- unique(nursing_combined$PROVNUM) ###16685
total_unique_ids <- length(unique_ids)


df_summary <- nursing_combined %>%
    group_by(month,year) %>%
    summarize(total_restot = sum(RESTOT, na.rm = TRUE)) %>% 
    ungroup() %>%
    arrange(month, year) %>%
    mutate(total_restot_2019 =  total_restot[match(month, month[year == 2019])],
           restot_change = total_restot / total_restot_2019) %>% 
    mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>% 
    filter(date>="2020-03-01") %>% arrange (year)

###### plot the changes in population size 

ggplot(df_summary, aes(x = date, y = total_restot, fill = "Total")) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = "#DF8F44") +
    labs(x = "Month", y = "Total Residents") +
    theme(legend.position = "none", 
          panel.background = element_blank(), 
          panel.grid.minor = element_blank())




##-----------------------------------------------------------------------------------------------------------------------
## Estimate the adjusted monthly excess deaths, accounted for changes in population size 
##------------------------------------------------------------------------------------------------------------------------
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

pod_crude_df<- pod_crude("Nursing Home") %>% mutate (year=year(date)) 

#####merge with DF summary
pod_crude_df <- pod_crude_df %>% left_join (df_summary, by="date") %>% 
    mutate(pop=total_restot) %>% 
    mutate (observed_pop=observed/pop*100000,
            expected_pop=expected/pop*100000,
            expected.lower_pop=expected.lower/pop*100000,
            expected.upper_pop=expected.upper/pop*100000) %>% 
    mutate(excess_pop=observed_pop-expected_pop,
           excess.lower_pop=observed_pop-expected.upper_pop,
           excess.upper_pop=observed_pop-expected.lower_pop) %>% 
    mutate(observed_updated=observed/restot_change,
           excess_updated=observed_updated-expected,
           excess_updated.lower=observed_updated-expected.upper,
           excess_updated.upper=observed_updated-expected.lower) %>% 
    mutate(year_bin=ifelse(date<"2021-03-01","year1","year2"))
###calculate the cumulative excess death by year 

pod_crude_df<- pod_crude_df %>% group_by(year_bin) %>% 
                 mutate(sum_excess_updated=sum(excess_updated))


pod_crude_df$date<-as.Date(pod_crude_df$date)



pod_crude_df_adjusted <-pod_crude_df %>% mutate_if(is.list,as.numeric)


######save the data 

saveRDS(pod_crude_df_adjusted, "results/01_data/pod_crude_df_adjusted.rds") 


##-----------------------------------------------------------------------------------------------------------------------
## Estimate the adjusted years excess deaths, accounted for changes in population size 
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
    df_summary$date<-as.Date(df_summary$date,'%Y-%m-%d')
    bb$date<-as.Date(bb$date,'%Y-%m-%d')
    
    ####incorporate the adjusted observed deahs 
   bb<-bb %>% left_join (df_summary, by="date") %>% 
        mutate(observed_updated=observed/restot_change)
    
    
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
        p1.observed_updated=sum(bb$observed_updated[bb$Year=="Year 1"]),
        p1.expected=sum(bb$expected[bb$Year=="Year 1"]),
        p1.expected.lower=as.numeric(quantile(SS$p1,0.025)),
        p1.expected.upper=as.numeric(quantile(SS$p1,0.975)),
        p1.excess_updated=sum(bb$observed_updated[bb$Year=="Year 1"]-bb$expected[bb$Year=="Year 1"]),
        p1.excess_updated.lower=sum(bb$observed_updated[bb$Year=="Year 1"])-as.numeric(quantile(SS$p1,0.975)),
        p1.excess_updated.upper=sum(bb$observed_updated[bb$Year=="Year 1"])-as.numeric(quantile(SS$p1,0.025)),
        # define results for 2nd period
        p2.observed_updated=sum(bb$observed_updated[bb$Year=="Year 2"]),
        p2.expected=sum(bb$expected[bb$Year=="Year 2"]),
        p2.expected.lower=as.numeric(quantile(SS$p2,0.025)),
        p2.expected.upper=as.numeric(quantile(SS$p2,0.975)),
        p2.excess_updated=sum(bb$observed_updated[bb$Year=="Year 2"]-bb$expected[bb$Year=="Year 2"]),
        p2.excess_updated.lower=sum(bb$observed_updated[bb$Year=="Year 2"])-as.numeric(quantile(SS$p2,0.975)),
        p2.excess_updated.upper=sum(bb$observed_updated[bb$Year=="Year 2"])-as.numeric(quantile(SS$p2,0.025)),
        p1.observed=sum(bb$observed[bb$Year=="Year 1"]),
        p1.excess=sum(bb$observed[bb$Year=="Year 1"]-bb$expected[bb$Year=="Year 1"]),
        p1.excess.lower=sum(bb$observed[bb$Year=="Year 1"])-as.numeric(quantile(SS$p1,0.975)),
        p1.excess.upper=sum(bb$observed[bb$Year=="Year 1"])-as.numeric(quantile(SS$p1,0.025)),
        # define results for 2nd period
        p2.observed=sum(bb$observed[bb$Year=="Year 2"]),
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




pod_year_non_stand("Nursing Home")

pod_year_df <- flextable(pod_year_df)
path<-"results/02_output/pod_year_df_adjusted.docx"

print(pod_year_df , target=path)

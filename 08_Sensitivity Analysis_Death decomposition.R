
read_sup<- function (name1, name2) {
    
    df_2021_2022 <-    read_delim ((paste("raw_data/supplemental data/",name1,".txt", sep="")),
                                   show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths)%>%
    extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE) %>% 
    mutate (date=paste(month_code, "/01", sep="")) %>% 
    filter(date>=as.Date("2021/01/01") & date<=as.Date("2022/02/01"))


#read 2014-2020 data - select 2014 to 2020 data 
df_2014_2020<-read_delim((paste("raw_data/supplemental data/",name2,".txt", sep="")), 
                         show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths)%>%
    extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE)%>% filter(year>=2014 & year <= 2020)


####convert date 
df_2021_2022<- convert_date(df_2021_2022)
df_2014_2020<- convert_date(df_2014_2020)

###merge pre and post pandemic data together 
month_df<-rbind(df_2021_2022, df_2014_2020) %>% mutate(date=as.Date(date)) }
# 
# AUNC<-read_sup(  "ADRD Underlying, Non-ADRD Non-Covid Contributing_2018_2022",
#                  "ADRD Underlying, Non-ADRD Non-Covid Contributing_1999_2020") %>% 
#       mutate(group="ADRD Underlying, Non-ADRD Non-Covid Contributing")%>% 
#     select(month_code, deaths, year, group)

###############################################AUNC#############################################
AUNC<-read_sup(  "ADRD Underlying _2018_2022",
                 "ADRD Underlying _1999_2020") %>% 
  mutate(group="ADRD Underlying, Non-Covid Contributing")%>% 
  select(month_code, deaths, year, group)

AC<-read_delim((paste("raw_data/supplemental data/",
                      "ADRD Underlying, Covid Contributing_2018_2022",".txt", sep="")), 
               show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths)%>%
  extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE)%>% 
  mutate (date=paste(month_code, "/01", sep="")) %>% 
  mutate(group="ADRD Underlying, Covid Contributing")%>% 
  filter(date<=as.Date("2022/02/01"))%>% 
  select(month_code, deaths, year, group) 

AC_sub <- AC %>% 
  dplyr:: select(month_code,deaths) %>% 
  dplyr::rename(cov_deaths=deaths)

AUNC <- merge(AUNC, AC_sub, by="month_code",all.x = TRUE)

AUNC <- AUNC %>% 
  mutate(cov_deaths=replace_na(cov_deaths, 0),
         deaths=deaths-cov_deaths) %>%
  select(-cov_deaths)

###############################################NUAC#############################################


NUAC<- read_sup("Non-ADRD Non-Covid Underlying, ADRD Contributing_2018_2022",
                "Non-ADRD Non-Covid Underlying, ADRD Contributing_1999_2020")%>% 
    mutate(group="Non-ADRD Non-Covid Underlying, ADRD Contributing")%>% 
    select(month_code, deaths, year, group)


###############################################CA#############################################


CA<-read_delim((paste("raw_data/supplemental data/",
                      "Covid Underlying, ADRD Contributing_2018_2022",".txt", sep="")), 
               show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    filter(is.na(notes))%>% select(-notes) %>% select(month, month_code,deaths)%>%
    mutate (date=paste(month_code, "/01", sep="")) %>% 
    extract(month_code, c("year", "month"),  "([[:digit:]]+)/([[:digit:]]+)", remove=FALSE)%>% 
    mutate(group="Covid Underlying, ADRD Contributing") %>% 
    filter(date<=as.Date("2022/02/01"))%>% 
    select(month_code, deaths, year, group) 


################################rbind all the data 
all.df<- rbind(AUNC, NUAC, AC, CA) %>% dplyr::rename(Group=group)

all.df$date_code<-paste (all.df$month_code, "/01", sep="")
all.df$date_code<-as.Date(all.df$date_code)

all.df <- all.df %>%  mutate(year_group=ifelse(month_code>="2020/03" & month_code<="2021/02", "year 1",
                                        ifelse(month_code>="2021/03" & month_code<="2022/02", "year 2", 
                                                "prepandemic")))

aggregate(deaths~year_group+Group,data=all.df,FUN=sum)

Figure.s2<-ggplot(all.df, aes(x = date_code, y = deaths, fill=Group)) + 
    geom_col() +
    theme_bw() +scale_fill_jama()+ylab("Number of Deaths")+xlab("Date")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())+theme(legend.position = "right",
                                          text = element_text(family = "serif"))



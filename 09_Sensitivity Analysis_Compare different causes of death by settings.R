# Load required libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsci)
library(scales)



# Heatmap - By (year, month) and leading cause
YM_POD_MCD_113_2018_2022 <- read.csv("raw_data/Year_Month_POD_MCD_113_2018_2022.txt",header=TRUE,sep="\t")
YM_POD_MCD_ADRD_2018_2022 <- read.csv("raw_data/Year_Month_POD_MCD_ADRD_2018_2022.txt",header=TRUE,sep="\t")


# Remove notes
YM_POD_MCD_113_2018_2022 <- YM_POD_MCD_113_2018_2022[,2:ncol(YM_POD_MCD_113_2018_2022)]
YM_POD_MCD_ADRD_2018_2022 <- YM_POD_MCD_ADRD_2018_2022[,2:ncol(YM_POD_MCD_ADRD_2018_2022)]

# Convert the monthly number of deaths to a numeric value
YM_POD_MCD_113_2018_2022$Deaths <- as.numeric(YM_POD_MCD_113_2018_2022$Deaths)
YM_POD_MCD_ADRD_2018_2022$Deaths <- as.numeric(YM_POD_MCD_ADRD_2018_2022$Deaths)

# Check dimensions
dim(YM_POD_MCD_113_2018_2022) # 2160 18
dim(YM_POD_MCD_ADRD_2018_2022) # 574 17

# Selected columns
col_short_list <- c("Year","Year.Code","Month","Month.Code","Place.of.Death","Place.of.Death.Code",
                    "MCD...ICD.10.113.Cause.List","Deaths")
col_short_list_v2 <- c("Year","Year.Code","Month","Month.Code","Place.of.Death","Place.of.Death.Code","Deaths")

YM_POD_MCD_113_2018_2022_short <- YM_POD_MCD_113_2018_2022[,col_short_list]
YM_POD_MCD_ADRD_2018_2022_short <- YM_POD_MCD_ADRD_2018_2022[,col_short_list_v2]
YM_POD_MCD_ADRD_2018_2022_short$MCD...ICD.10.113.Cause.List <- 'Alzheimer\'s disease and related dementias'
YM_POD_MCD_ADRD_2018_2022_short <- YM_POD_MCD_ADRD_2018_2022_short[,col_short_list]

# Check dimensions
dim(YM_POD_MCD_ADRD_2018_2022_short) # 574 8
dim(YM_POD_MCD_113_2018_2022_short) # 2160 8

# Handy function
extract_month <- function(x){
    return(strsplit(x,"/")[[1]][2])
}

# Combine ADRD and other causes of death
YM_POD_MCD_113_ADRD_2018_2022_short <- rbind(YM_POD_MCD_ADRD_2018_2022_short,YM_POD_MCD_113_2018_2022_short)
YM_POD_MCD_113_ADRD_2020_2022_short <- YM_POD_MCD_113_ADRD_2018_2022_short[YM_POD_MCD_113_ADRD_2018_2022_short$Year != 2018,]
# Note: Before, we used to visualize the 2020-2022 timeline. Now, we include 2019.
#YM_POD_MCD_113_ADRD_2020_2022_short <- YM_POD_MCD_113_ADRD_2020_2022_short[YM_POD_MCD_113_ADRD_2020_2022_short$Year != 2019,]

rows_to_remove_113_ADRD <- which(YM_POD_MCD_113_ADRD_2020_2022_short$MCD...ICD.10.113.Cause.List == "")
rows_to_remove_113_ADRD_2018_2022 <- which(YM_POD_MCD_113_ADRD_2018_2022_short$MCD..ICD.10.113.Cause.List == "")
YM_POD_MCD_113_ADRD_2020_2022_short <- YM_POD_MCD_113_ADRD_2020_2022_short[-c(rows_to_remove_113_ADRD),]
YM_POD_MCD_113_ADRD_2018_2022_short <- YM_POD_MCD_113_ADRD_2018_2022_short[-c(rows_to_remove_113_ADRD_2018_2022),]

YM_POD_MCD_113_ADRD_2018_2022_short$Year.Code <- as.factor(YM_POD_MCD_113_ADRD_2018_2022_short$Year.Code)
YM_POD_MCD_113_ADRD_2018_2022_short$Month.Short <- sapply(YM_POD_MCD_113_ADRD_2018_2022_short$Month.Code, 
                                                          function(x) extract_month(x))
YM_POD_MCD_113_ADRD_2018_2022_short$Month.Code <- as.factor(YM_POD_MCD_113_ADRD_2018_2022_short$Month.Code)
YM_POD_MCD_113_ADRD_2018_2022_short <- YM_POD_MCD_113_ADRD_2018_2022_short[-which(is.na(YM_POD_MCD_113_ADRD_2018_2022_short$Deaths)),]

YM_POD_MCD_113_ADRD_2020_2022_short$Year.Code <- as.factor(YM_POD_MCD_113_ADRD_2020_2022_short$Year.Code)
YM_POD_MCD_113_ADRD_2020_2022_short$Month.Short <- sapply(YM_POD_MCD_113_ADRD_2020_2022_short$Month.Code, 
                                                          function(x) extract_month(x))
YM_POD_MCD_113_ADRD_2020_2022_short$Month.Code <- as.factor(YM_POD_MCD_113_ADRD_2020_2022_short$Month.Code)
YM_POD_MCD_113_ADRD_2020_2022_short <- YM_POD_MCD_113_ADRD_2020_2022_short[-which(is.na(YM_POD_MCD_113_ADRD_2020_2022_short$Deaths)),]

# ICD categories of interest
icd_list <- c( "Diseases of the circulatory system" , "Diseases of the nervous system" ,              
               "Neoplasms"                             ,        "Mental and behavioural disorders" ,            
               "Diseases of the respiratory system"     ,       "Endocrine, nutritional and metabolic diseases",
               "Diseases of the digestive system", "Diseases of the genitourinary system")

# 2020-2022
year1 <- c("2020/03","2020/04","2020/05","2020/06","2020/07","2020/08","2020/09","2020/10","2020/11",
           "2020/12","2021/01","2021/02")
print(length(year1))
year2 <- c("2021/03","2021/04","2021/05","2021/06","2021/07","2021/08","2021/09","2021/10","2021/11",
           "2021/12","2022/01","2022/02")
print(length(year2))
year1_year2 <- c(year1,year2)
print(length(year1_year2))

# Selecting relevant months and place of death strata
year2019 <- c("2019/03","2019/04","2019/05","2019/06","2019/07","2019/08","2019/09","2019/10","2019/11",
              "2019/12","2020/01","2020/02")
print(length(year2019))

# Adding year 2019 as baseline
year1_year2_year2019 <- c(year2019,year1_year2)

# Stratification by place of death: NH-LTCF, home, medical facilities
YM_POD_MCD_113_ADRD_2018_2022_short_NH_LTCF <- YM_POD_MCD_113_ADRD_2018_2022_short[YM_POD_MCD_113_ADRD_2018_2022_short$Place.of.Death == 'Nursing home/long term care',]
YM_POD_MCD_113_ADRD_2018_2022_short_NH_LTCF <- YM_POD_MCD_113_ADRD_2018_2022_short_NH_LTCF[YM_POD_MCD_113_ADRD_2018_2022_short_NH_LTCF$Month.Code %in% year2019,]

YM_POD_MCD_113_ADRD_2018_2022_short_home <- YM_POD_MCD_113_ADRD_2018_2022_short[YM_POD_MCD_113_ADRD_2018_2022_short$Place.of.Death == 'Decedent\'s home',]
YM_POD_MCD_113_ADRD_2018_2022_short_home  <- YM_POD_MCD_113_ADRD_2018_2022_short_home[YM_POD_MCD_113_ADRD_2018_2022_short_home$Month.Code %in% year2019,]

YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF <- YM_POD_MCD_113_ADRD_2020_2022_short[YM_POD_MCD_113_ADRD_2020_2022_short$Place.of.Death == 'Nursing home/long term care',]
YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF <- YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF[YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Month.Code %in% year1_year2_year2019,]

YM_POD_MCD_113_ADRD_2020_2022_short_home <- YM_POD_MCD_113_ADRD_2020_2022_short[YM_POD_MCD_113_ADRD_2020_2022_short$Place.of.Death == 'Decedent\'s home',]
YM_POD_MCD_113_ADRD_2020_2022_short_home  <- YM_POD_MCD_113_ADRD_2020_2022_short_home[YM_POD_MCD_113_ADRD_2020_2022_short_home$Month.Code %in% year1_year2_year2019,]

sum(table(YM_POD_MCD_113_ADRD_2020_2022_short_home$Month.Code)) # 180
sum(table(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Month.Code)) # 180

medfac <- c("Medical Facility - Dead on Arrival","Medical Facility - Inpatient","Medical Facility - Outpatient or ER")
YM_POD_MCD_113_ADRD_2020_2022_short_medfac <- YM_POD_MCD_113_ADRD_2020_2022_short[YM_POD_MCD_113_ADRD_2020_2022_short$Place.of.Death %in% medfac,]
YM_POD_MCD_113_ADRD_2020_2022_short_medfac  <- YM_POD_MCD_113_ADRD_2020_2022_short_medfac[YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Month.Code %in% year1_year2_year2019,]
sum(table(YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Month.Code)) # 539 (instead of 540)
# Note: for (2019/09, cerebrovascular diseases), the "medical facility - dead on arrival" category is missing (most likely, <10 deaths)

YM_POD_MCD_113_ADRD_2020_2022_short_medfac <- YM_POD_MCD_113_ADRD_2020_2022_short_medfac %>% dplyr::group_by(MCD...ICD.10.113.Cause.List,Year,Year.Code,Month,Month.Code,Month.Short) %>% dplyr::summarise(Deaths=sum(Deaths))

# Average number of monthly deaths in the 12 months preceding the start of the pandemic, by place of death
# 12-month pre-period defined as March 2019-February 2020 (inclusive)
YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$MCD...ICD.10.113.Cause.List <- factor(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$MCD...ICD.10.113.Cause.List)
YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_avg <- YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF %>% dplyr::filter(Month.Code %in% year2019) %>% dplyr::group_by(MCD...ICD.10.113.Cause.List) %>% 
    dplyr::summarise(avg_pre=mean(Deaths))

YM_POD_MCD_113_ADRD_2020_2022_short_home$MCD...ICD.10.113.Cause.List <- factor(YM_POD_MCD_113_ADRD_2020_2022_short_home$MCD...ICD.10.113.Cause.List)
YM_POD_MCD_113_ADRD_2020_2022_short_home_avg <- YM_POD_MCD_113_ADRD_2020_2022_short_home %>% dplyr::filter(Month.Code %in% year2019) %>% dplyr::group_by(MCD...ICD.10.113.Cause.List) %>% 
    dplyr::summarise(avg_pre=mean(Deaths))

YM_POD_MCD_113_ADRD_2020_2022_short_medfac$MCD...ICD.10.113.Cause.List <- factor(YM_POD_MCD_113_ADRD_2020_2022_short_medfac$MCD...ICD.10.113.Cause.List)
YM_POD_MCD_113_ADRD_2020_2022_short_medfac_avg <- YM_POD_MCD_113_ADRD_2020_2022_short_medfac %>% dplyr::filter(Month.Code %in% year2019) %>% dplyr::group_by(MCD...ICD.10.113.Cause.List) %>% 
    dplyr::summarise(avg_pre=mean(Deaths))

# Adding corresponding feature in order to scale as a function of the 2019 monthly average
YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_avg <- YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_avg[,c("MCD...ICD.10.113.Cause.List","avg_pre")]
YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_avg <- YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_avg[!duplicated(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_avg ),]

YM_POD_MCD_113_ADRD_2020_2022_short_home_avg<- YM_POD_MCD_113_ADRD_2020_2022_short_home_avg[,c("MCD...ICD.10.113.Cause.List","avg_pre")]
YM_POD_MCD_113_ADRD_2020_2022_short_home_avg <- YM_POD_MCD_113_ADRD_2020_2022_short_home_avg[!duplicated(YM_POD_MCD_113_ADRD_2020_2022_short_home_avg ),]

YM_POD_MCD_113_ADRD_2020_2022_short_medfac_avg<- YM_POD_MCD_113_ADRD_2020_2022_short_medfac_avg[,c("MCD...ICD.10.113.Cause.List","avg_pre")]
YM_POD_MCD_113_ADRD_2020_2022_short_medfac_avg <- YM_POD_MCD_113_ADRD_2020_2022_short_medfac_avg[!duplicated(YM_POD_MCD_113_ADRD_2020_2022_short_medfac_avg ),]

# Scaling as a function of the 2019 monthly average
YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF <- merge(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF,YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_avg,by="MCD...ICD.10.113.Cause.List")
YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc <- YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths/YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$avg_pre*100

YM_POD_MCD_113_ADRD_2020_2022_short_home <- merge(YM_POD_MCD_113_ADRD_2020_2022_short_home,YM_POD_MCD_113_ADRD_2020_2022_short_home_avg,by="MCD...ICD.10.113.Cause.List")
YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc <- YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths/YM_POD_MCD_113_ADRD_2020_2022_short_home$avg_pre*100

YM_POD_MCD_113_ADRD_2020_2022_short_medfac <- merge(YM_POD_MCD_113_ADRD_2020_2022_short_medfac,YM_POD_MCD_113_ADRD_2020_2022_short_medfac_avg,by="MCD...ICD.10.113.Cause.List")
YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc <- YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths/YM_POD_MCD_113_ADRD_2020_2022_short_medfac$avg_pre*100

# Distribution of scaled outcome, by place of death
summary(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 65.24   86.64   95.13   96.77  104.12  160.29 
summary(YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 89.13  103.72  120.42  121.11  134.59  187.65 
summary(YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 81.34   97.62  105.19  110.88  118.27  196.55 

# Place of death: NH - LTCF

# Absolute number of deaths per month
p_nh <- ggplot(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF, aes(x = Month.Code, y = MCD...ICD.10.113.Cause.List, fill = Deaths)) +
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1) + xlab("Month") + ylab("Multiple Cause of Death") + ggtitle('Deaths occurring in nursing homes and LTCF') +
    scale_fill_gradientn(colors = hcl.colors(20, "YlOrRd")[length(hcl.colors(20, "YlOrRd")):1]) + # scale_fill_jama() +
    # Experimenting with other color palettes
    # scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")[length(hcl.colors(20, "RdYlGn")):1]) + # scale_fill_jama() +
    # To be adjusted with: family="A"
    #theme(text = element_text(family = "Times New Roman"))
    theme(text=element_text(family="A"))

coord_fixed()

# Visualize
p_nh

# Scaled outcome, as a function of the 2019 monthly average
p_nh_scaled <- ggplot(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF, aes(x = Month.Code, y = MCD...ICD.10.113.Cause.List, 
                                                                       fill = Deaths_scaled_perc)) +
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1) + xlab("Year/Month") + ylab("Multiple Cause of Death") + 
    ggtitle('Deaths occurring in nursing homes and LTCF') +
    scale_fill_gradientn(colors = hcl.colors(20, "YlOrRd")[length(hcl.colors(20, "YlOrRd")):1]) + # scale_fill_jama() +
    
    # Experimenting with other color palettes
    #scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")[length(hcl.colors(20, "RdYlGn")):1]) + # scale_fill_jama() +
    # To be adjusted with: family="A"
    # theme(text = element_text(family = "Times New Roman")) +
    theme(text=element_text(family="A")) +
    guides(fill=guide_legend(title="Mortality intensity expressed as a % of average in previous 12 months")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

coord_fixed()

# Visualize
p_nh_scaled

# Place of death: Home

# Absolute number of deaths per month
p_home <- ggplot(YM_POD_MCD_113_ADRD_2020_2022_short_home, aes(x = Month.Code, y = MCD...ICD.10.113.Cause.List, fill = Deaths)) +
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1) + xlab("Month") + ylab("Multiple Cause of Death") + ggtitle('Deaths occurring at home') +
    scale_fill_gradientn(colors = hcl.colors(20, "YlOrRd")[length(hcl.colors(20, "YlOrRd")):1]) + # scale_fill_jama() +
    
    # Experimenting with other color palettes
    # scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")[length(hcl.colors(20, "RdYlGn")):1]) + # scale_fill_jama() +
    # To be adjusted with: family="A
    # theme(text = element_text(family = "Times New Roman"))
    theme(text=element_text(family="A"))

coord_fixed()

# Visualize
p_home

# Scaled outcome, as a function of the 2019 monthly average
p_home_scaled <- ggplot(YM_POD_MCD_113_ADRD_2020_2022_short_home, aes(x = Month.Code, y = MCD...ICD.10.113.Cause.List, 
                                                                      fill = Deaths_scaled_perc)) + 
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1) + xlab("Year/Month") + ylab("Multiple Cause of Death") + ggtitle('Deaths occurring at home')+
    # Experimenting with other color palettes
    # (colors = hcl.colors(20, "RdYlGn")[length(hcl.colors(20, "RdYlGn")):1]) + # scale_fill_jama() +
    scale_fill_gradientn(colors = hcl.colors(20, "YlOrRd")[length(hcl.colors(20, "YlOrRd")):1]) + # scale_fill_jama() +
    # To be adjusted with: family="A
    # theme(text = element_text(family = "Times New Roman")) +
    theme(text=element_text(family="A")) +
    guides(fill=guide_legend(title="Mortality intensity expressed as a % of average in previous 12 months")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

coord_fixed()

# Visualize
p_home_scaled

# Place of death: Medical facility

# Scaled outcome, as a function of the 2019 monthly average
p_medfac_scaled <- ggplot(YM_POD_MCD_113_ADRD_2020_2022_short_medfac, aes(x = Month.Code, y = MCD...ICD.10.113.Cause.List, 
                                                                          fill = Deaths_scaled_perc)) +
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1) + xlab("Year/Month") + ylab("Multiple Cause of Death") + 
    ggtitle('Deaths occurring in medical facilities') +
    scale_fill_gradientn(colors = hcl.colors(20, "YlOrRd")[length(hcl.colors(20, "YlOrRd")):1]) + # scale_fill_jama() +
    # Experimenting with other color palettes
    #scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")[length(hcl.colors(20, "RdYlGn")):1]) + # scale_fill_jama() +
    # To be adjusted with: family="A
    # theme(text = element_text(family = "Times New Roman")) +
    theme(text=element_text(family="A")) +
    guides(fill=guide_legend(title="Mortality intensity expressed as a % of average in previous 12 months")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

coord_fixed()

# Visualize
p_medfac_scaled 

# Figure combining two scaled heatmaps: NH/LTCF and home
fig_2 <- ggarrange(p_nh_scaled + rremove("xlab") + rremove("ylab") + rremove("x.text"), 
                   p_home_scaled + rremove("xlab") + rremove("ylab"),  
                   common.legend = FALSE, nrow=2,labels = NULL) + 
    # To be adjusted with: family="A"
    theme(text=element_text(family="A"))

annotate_figure(fig_2, left = textGrob("Multiple cause of death", rot = 90, vjust = 1),
                bottom = textGrob("Year/Month"))

# Figure combining the three scaled heatmaps
fig_3 <- ggarrange(p_nh_scaled + rremove("xlab") + rremove("ylab") + rremove("x.text"), 
                   p_home_scaled + rremove("xlab") + rremove("ylab") + rremove("x.text"),  
                   p_medfac_scaled + rremove("xlab") + rremove("ylab"),  
                   common.legend = FALSE, nrow=3,labels = NULL) + 
    # To be adjusted with family="A"
    theme(text=element_text(family="A"))

annotate_figure(fig_3, left = textGrob("Multiple cause of death", rot = 90, vjust = 1),
                bottom = textGrob("Year/Month"))

# Manually defining our own categories so that the three heatmaps have the same scaling system
# Categories "1"-"6" will correspond to manually selected colors that are sufficiently distinct
# to showcase differences both temporally and among leading causes of death.

YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc_manual <- ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc > 80 & YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc <= 100, "1", 
                                                                               ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc > 100 & YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc <= 120, "2",
                                                                                      ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc > 120 & YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc <= 140, "3",
                                                                                             ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc > 140 & YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc <= 160, "4",
                                                                                                    ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc > 160 & YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Deaths_scaled_perc <= 180, "5",
                                                                                                           "6")))))

YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc_manual <- ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc > 80 & YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc <= 100, "1", 
                                                                             ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc > 100 & YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc <= 120, "2",
                                                                                    ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc > 120 & YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc <= 140, "3",
                                                                                           ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc > 140 & YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc <= 160, "4",
                                                                                                  ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc > 160 & YM_POD_MCD_113_ADRD_2020_2022_short_home$Deaths_scaled_perc <= 180, "5",
                                                                                                         "6")))))

YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc_manual <- ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc > 80 & YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc <= 100, "1", 
                                                                                ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc > 100 & YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc <= 120, "2",
                                                                                       ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc > 120 & YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc <= 140, "3",
                                                                                              ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc > 140 & YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc <= 160, "4",
                                                                                                     ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc > 160 & YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Deaths_scaled_perc <= 180, "5",
                                                                                                            "6")))))

# Reduction in mortality associated with ADRD or another leading cause between year 1 and year 2, by place of death
YM_POD_MCD_113_ADRD_2020_2022_short_home$year1 <- ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_home$Month.Code %in% year1, 1, 0)
YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$year1 <- ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Month.Code %in% year1, 1, 0)
YM_POD_MCD_113_ADRD_2020_2022_short_medfac$year1 <- ifelse(YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Month.Code %in% year1, 1, 0)

table(YM_POD_MCD_113_ADRD_2020_2022_short_home$year1) # 5*12=60
table(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$year1) # 5*12=60
table(YM_POD_MCD_113_ADRD_2020_2022_short_medfac$year1) # 5*12=60

# Place of death: NH-LTCF - Reduction in mortality between year 1 and year 2
YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_comp <- YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF[YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF$Year != 2019,]
YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_comp <- YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_comp[-which(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_comp$Month.Code %in% c("2020/01","2020/02")),]

# Sanity check: there should be as many rows for year 1 and year 2
table(YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_comp$year1)

year1_year2_NH <- YM_POD_MCD_113_ADRD_2020_2022_short_NH_LTCF_comp %>% dplyr::group_by(year1,MCD...ICD.10.113.Cause.List) %>% 
    dplyr::summarise(n=sum(Deaths))
year1_NH <- year1_year2_NH[year1_year2_NH$year1==1,c("MCD...ICD.10.113.Cause.List","n")]
names(year1_NH)[2] <- "n_year1"
year1_year2_NH <- merge(year1_year2_NH,year1_NH,by=c("MCD...ICD.10.113.Cause.List"),all.x=TRUE)
year1_year2_NH$perc_year1 <- year1_year2_NH$n/year1_year2_NH$n_year1

# Place of death: Home - Reduction in mortality between year 1 and year 2
YM_POD_MCD_113_ADRD_2020_2022_short_home_comp <- YM_POD_MCD_113_ADRD_2020_2022_short_home[YM_POD_MCD_113_ADRD_2020_2022_short_home$Year != 2019,]
YM_POD_MCD_113_ADRD_2020_2022_short_home_comp <- YM_POD_MCD_113_ADRD_2020_2022_short_home_comp[-which(YM_POD_MCD_113_ADRD_2020_2022_short_home_comp$Month.Code %in% c("2020/01","2020/02")),]

# Sanity check: there should be as many rows for year 1 and year 2
table(YM_POD_MCD_113_ADRD_2020_2022_short_home_comp$year1)

year1_year2_home <- YM_POD_MCD_113_ADRD_2020_2022_short_home_comp %>% dplyr::group_by(year1,MCD...ICD.10.113.Cause.List) %>% 
    dplyr::summarise(n=sum(Deaths))
year1_home_only <- year1_year2_home[year1_year2_home$year1==1,c("MCD...ICD.10.113.Cause.List","n")]
names(year1_home_only)[2] <- "n_year1"
year1_year2_home <- merge(year1_year2_home,year1_home_only,by=c("MCD...ICD.10.113.Cause.List"),all.x=TRUE)
year1_year2_home$perc_year1 <- year1_year2_home$n/year1_year2_home$n_year1

# Place of death: Medical facilities - Reduction in mortality between year 1 and year 2
YM_POD_MCD_113_ADRD_2020_2022_short_medfac_comp <- YM_POD_MCD_113_ADRD_2020_2022_short_medfac[YM_POD_MCD_113_ADRD_2020_2022_short_medfac$Year != 2019,]
YM_POD_MCD_113_ADRD_2020_2022_short_medfac_comp <- YM_POD_MCD_113_ADRD_2020_2022_short_medfac_comp[-which(YM_POD_MCD_113_ADRD_2020_2022_short_medfac_comp$Month.Code %in% c("2020/01","2020/02")),]

# Sanity check: there should be as many rows for year 1 and year 2
table(YM_POD_MCD_113_ADRD_2020_2022_short_medfac_comp$year1)

year1_year2_medfac <- YM_POD_MCD_113_ADRD_2020_2022_short_medfac_comp %>% dplyr::group_by(year1,MCD...ICD.10.113.Cause.List) %>% 
    dplyr::summarise(n=sum(Deaths))
year1_medfac_only <- year1_year2_medfac[year1_year2_medfac$year1==1,c("MCD...ICD.10.113.Cause.List","n")]
names(year1_medfac_only)[2] <- "n_year1"
year1_year2_medfac <- merge(year1_year2_medfac,year1_medfac_only,by=c("MCD...ICD.10.113.Cause.List"),all.x=TRUE)
year1_year2_medfac$perc_year1 <- year1_year2_medfac$n/year1_year2_medfac$n_year1

# Visualize tabulated results for each place of death
year1_year2_NH[order(year1_year2_NH$MCD...ICD.10.113.Cause.List,year1_year2_NH$year1),]
year1_year2_home[order(year1_year2_home$MCD...ICD.10.113.Cause.List,year1_year2_home$year1),]
year1_year2_medfac[order(year1_year2_medfac$MCD...ICD.10.113.Cause.List,year1_year2_medfac$year1),]

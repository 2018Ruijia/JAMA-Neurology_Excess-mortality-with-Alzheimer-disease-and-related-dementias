library(ggplot2)
library(zoo)
library(dplyr)
library(nlme)

# Update to reflect today's date. 
# I am adding this suffix at the end of the file names with results for better version control.
today_date <- '20220903'

# Load data
# Change with your own path
vax_state_df <- read.csv('raw_data/2022.07.24.COVID19_Vaccinations_in_the_United_States_Jurisdiction.csv',header=TRUE)

# Data structure
head(vax_state_df)
names(vax_state_df)

# Format location
table(vax_state_df$Location)
vax_state_df$Recip_State <- factor(vax_state_df$Location)

# Format date
vax_state_df$Date <- as.Date(vax_state_df$Date,format='%m/%d/%Y')
table(vax_state_df$Date)

# Convert the percentage of the population being vaccinated in each age group to a numeric value
vax_state_df$Administered_Dose1_Recip_5PlusPop_Pct <- as.numeric(vax_state_df$Administered_Dose1_Recip_5PlusPop_Pct)
vax_state_df$Administered_Dose1_Recip_12PlusPop_Pct <- as.numeric(vax_state_df$Administered_Dose1_Recip_12PlusPop_Pct)
vax_state_df$Administered_Dose1_Recip_18PlusPop_Pct <- as.numeric(vax_state_df$Administered_Dose1_Recip_18PlusPop_Pct)
vax_state_df$Administered_Dose1_Recip_65PlusPop_Pct <- as.numeric(vax_state_df$Administered_Dose1_Recip_65PlusPop_Pct)

# Determine the underlying population, by age group
vax_state_df$Administered_Dose1_Recip_12PlusPop <- vax_state_df$Administered_Dose1_Recip_12Plus/vax_state_df$Administered_Dose1_Recip_12PlusPop_Pct * 100
vax_state_df$Administered_Dose1_Recip_18PlusPop <- vax_state_df$Administered_Dose1_Recip_18Plus/vax_state_df$Administered_Dose1_Recip_18PlusPop_Pct * 100
vax_state_df$Administered_Dose1_Recip_65PlusPop <- vax_state_df$Administered_Dose1_Recip_65Plus/vax_state_df$Administered_Dose1_Recip_65PlusPop_Pct * 100

# Add additional age groups: 12-17 and 18-64

# Vaccination outcome (n)
vax_state_df$Administered_Dose1_Recip_12_17 <- vax_state_df$Administered_Dose1_Recip_12Plus - vax_state_df$Administered_Dose1_Recip_18Plus
vax_state_df$Administered_Dose1_Recip_18_64 <- vax_state_df$Administered_Dose1_Recip_18Plus - vax_state_df$Administered_Dose1_Recip_65Plus

# Population size
vax_state_df$Administered_Dose1_Recip_12_17Pop <- vax_state_df$Administered_Dose1_Recip_12PlusPop - vax_state_df$Administered_Dose1_Recip_18PlusPop
vax_state_df$Administered_Dose1_Recip_18_64Pop <- vax_state_df$Administered_Dose1_Recip_18PlusPop - vax_state_df$Administered_Dose1_Recip_65PlusPop

# Vaccination outcome (%)
vax_state_df$Administered_Dose1_Recip_12_17Pop_Pct <- vax_state_df$Administered_Dose1_Recip_12_17/vax_state_df$Administered_Dose1_Recip_12_17Pop*100
vax_state_df$Administered_Dose1_Recip_18_64Pop_Pct <- vax_state_df$Administered_Dose1_Recip_18_64/vax_state_df$Administered_Dose1_Recip_18_64Pop*100

# Two handy functions

# 1 - Replace NA cases with zeros
not_na <- function(x){
  if (is.na(x) == TRUE){
    return(0)
  }
  else{
    return(x)
  }
}

# 2 - Truncate at 100% if the % of vaccinated is greater due to poor data quality 
min_100 <- function(x){
  if (x > 100){
    return(100)
  }
  else{
    return(x)
  }
}

# Add additional age groups: 12-17 and 18-64

# First dose

# Apply functions
vax_state_df$Administered_Dose1_Recip_12_17Pop_Pct <- sapply(vax_state_df$Administered_Dose1_Recip_12_17Pop_Pct,not_na)
vax_state_df$Administered_Dose1_Recip_18_64Pop_Pct <- sapply(vax_state_df$Administered_Dose1_Recip_18_64Pop_Pct,not_na)

vax_state_df$Administered_Dose1_Recip_12_17Pop_Pct <- sapply(vax_state_df$Administered_Dose1_Recip_12_17Pop_Pct,min_100)
vax_state_df$Administered_Dose1_Recip_18_64Pop_Pct <- sapply(vax_state_df$Administered_Dose1_Recip_18_64Pop_Pct,min_100)

# Full vaccination

# Vaccination outcome (n)
vax_state_df$Series_Complete_12_17 <- vax_state_df$Series_Complete_12Plus - vax_state_df$Series_Complete_18Plus
vax_state_df$Series_Complete_18_64 <- vax_state_df$Series_Complete_18Plus - vax_state_df$Series_Complete_65Plus

# Population size
vax_state_df$Series_Complete_12PlusPop <- vax_state_df$Series_Complete_12Plus/vax_state_df$Series_Complete_12PlusPop_Pct * 100
vax_state_df$Series_Complete_18PlusPop <- vax_state_df$Series_Complete_18Plus/vax_state_df$Series_Complete_18PlusPop_Pct * 100
vax_state_df$Series_Complete_65PlusPop <- vax_state_df$Series_Complete_65Plus/vax_state_df$Series_Complete_65PlusPop_Pct * 100

vax_state_df$Series_Complete_12_17_Pop <- vax_state_df$Series_Complete_12PlusPop - vax_state_df$Series_Complete_18PlusPop
vax_state_df$Series_Complete_18_64_Pop <- vax_state_df$Series_Complete_18PlusPop - vax_state_df$Series_Complete_65PlusPop

vax_state_df$Series_Complete_12_17_Pop_Pct <- vax_state_df$Series_Complete_12_17/vax_state_df$Series_Complete_12_17_Pop*100
vax_state_df$Series_Complete_18_64_Pop_Pct <- vax_state_df$Series_Complete_18_64/vax_state_df$Series_Complete_18_64_Pop*100

# Apply functions
vax_state_df$Series_Complete_12_17_Pop_Pct <- sapply(vax_state_df$Series_Complete_12_17_Pop_Pct,not_na)
vax_state_df$Series_Complete_18_64_Pop_Pct <- sapply(vax_state_df$Series_Complete_18_64_Pop_Pct,not_na)

vax_state_df$Series_Complete_12_17_Pop_Pct <- sapply(vax_state_df$Series_Complete_12_17_Pop_Pct,min_100)
vax_state_df$Series_Complete_18_64_Pop_Pct <- sapply(vax_state_df$Series_Complete_18_64_Pop_Pct,min_100)

# Apply cumulative maximum function
vax_state_df <- vax_state_df[order(vax_state_df$Date),] %>% group_by(Recip_State) %>% mutate(Administered_Dose1_Recip_12PlusPop_Pct_cummax = cummax(Administered_Dose1_Recip_12PlusPop_Pct))
vax_state_df <- vax_state_df[order(vax_state_df$Date),] %>% group_by(Recip_State) %>% mutate(Administered_Dose1_Recip_18PlusPop_Pct_cummax = cummax(Administered_Dose1_Recip_18PlusPop_Pct))
vax_state_df <- vax_state_df[order(vax_state_df$Date),] %>% group_by(Recip_State) %>% mutate(Administered_Dose1_Recip_65PlusPop_Pct_cummax = cummax(Administered_Dose1_Recip_65PlusPop_Pct))

vax_state_df <- vax_state_df[order(vax_state_df$Date),] %>% group_by(Recip_State) %>% mutate(Administered_Dose1_Recip_12_17Pop_Pct_cummax = cummax(Administered_Dose1_Recip_12_17Pop_Pct))
vax_state_df <- vax_state_df[order(vax_state_df$Date),] %>% group_by(Recip_State) %>% mutate(Administered_Dose1_Recip_18_64Pop_Pct_cummax = cummax(Administered_Dose1_Recip_18_64Pop_Pct))

# Convert the percentage of the population being vaccinated in each age group to a numeric value
vax_state_df$Series_Complete_Recip_12PlusPop_Pct <- as.numeric(vax_state_df$Series_Complete_12PlusPop_Pct)
vax_state_df$Series_Complete_Recip_18PlusPop_Pct <- as.numeric(vax_state_df$Series_Complete_18PlusPop_Pct)
vax_state_df$Series_Complete_Recip_65PlusPop_Pct <- as.numeric(vax_state_df$Series_Complete_65PlusPop_Pct)

vax_state_df$Series_Complete_Recip_12_17_Pop_Pct <- as.numeric(vax_state_df$Series_Complete_12_17_Pop_Pct)
vax_state_df$Series_Complete_Recip_18_64_Pop_Pct <- as.numeric(vax_state_df$Series_Complete_18_64_Pop_Pct)

# Apply cumulative maximum function
vax_state_df <- vax_state_df[order(vax_state_df$Date),] %>% group_by(Recip_State) %>% mutate(Series_Complete_12PlusPop_Pct_cummax = cummax(Series_Complete_12PlusPop_Pct))
vax_state_df <- vax_state_df[order(vax_state_df$Date),] %>% group_by(Recip_State) %>% mutate(Series_Complete_18PlusPop_Pct_cummax = cummax(Series_Complete_18PlusPop_Pct))
vax_state_df <- vax_state_df[order(vax_state_df$Date),] %>% group_by(Recip_State) %>% mutate(Series_Complete_65PlusPop_Pct_cummax = cummax(Series_Complete_65PlusPop_Pct))

vax_state_df <- vax_state_df[order(vax_state_df$Date),] %>% group_by(Recip_State) %>% mutate(Series_Complete_12_17_Pop_Pct_cummax = cummax(Series_Complete_12_17_Pop_Pct))
vax_state_df <- vax_state_df[order(vax_state_df$Date),] %>% group_by(Recip_State) %>% mutate(Series_Complete_18_64_Pop_Pct_cummax = cummax(Series_Complete_18_64_Pop_Pct))

# Add time variable
vax_state_df$t <- vax_state_df$Date - min(vax_state_df$Date)
vax_state_df$t <- as.integer(vax_state_df$t)
vax_state_df <- vax_state_df[order(vax_state_df$Date),] %>% group_by(Recip_State) %>% mutate(Series_Complete_18_64_Pop_Pct_cummax = cummax(Series_Complete_18_64_Pop_Pct))

# List of states
state_list <- unique(vax_state_df$Recip_State)
state_list <- state_list[7:length(state_list)]
state_list <- state_list[1:55]

# Remove locations other than the 50 states + the District of Columbia
state_list <-  setdiff(state_list,"DD2")
state_list <-  setdiff(state_list,"VA2")
state_list <-  setdiff(state_list,"IH2")

# Remove West Virginia due to poor data quality
state_list <-  setdiff(state_list,"WV")
print(paste0('Number of states to be modeled: ', length(state_list)))

state_list_Non65Plus <- setdiff(state_list,"ID")
state_list_Non65Plus <- setdiff(state_list_Non65Plus,"HI")

state_list_65Plus <- c("ID", "HI")

print(paste0('Number of states to be modeled for populations other than 65+: ', length(state_list_Non65Plus)))

# Define end of the time series
# t=442 corresponds with 2022/02/28, which is the end of Ruijia's time series for the analysis of ADRD excess mortality
max_t <- 442 

# Initiate an iterator to keep track of progress through the list of states
state_iter <- 0

# Optimization parameters
maxiter_nls <- 100
minfactor_nls <- 0.000001
# K, p0, v are all positive
lwr_nls <- c(0,0,0)
# K and p0 are both below 100%. The constraint on v is "artificial". In practice, is is below 1.
upr_nls <- c(100,100,10)

for (state in state_list_Non65Plus){
  
  # Select state
  print(paste0('State:', state))
  state_vax_df <- vax_state_df[vax_state_df$Recip_State == state,]
  # Dimension of the dataset
  print(paste0('Dimension of the dataset: ', dim(state_vax_df)))
  
  # Initiate the start of the time series at 0, but it will get updated later to ensure goodness-of-fit
  min_t <- 0
  
  # Truncate the time series on the right at t=max_t
  state_vax_df <- state_vax_df[state_vax_df$t <= max_t,]
  # Dimension of the updated dataset
  print(paste0('Dimension of the dataset after right truncation: ', dim(state_vax_df)))
  
  # Full vaccination - 12+
  print('Full vaccination - 12+')
  
  # Only consider the time points for which the outcome is > 0
  min_t <- min(state_vax_df[state_vax_df$Series_Complete_12PlusPop_Pct_cummax > 0,"t"])
  
  # Initiate parameter values for (K, v, p0)
  state_initVals <- getInitial(Series_Complete_12PlusPop_Pct_cummax ~ SSlogis(t-min_t, Asym, xmid, scal), data = state_vax_df[state_vax_df$t >= min_t,],control=nls.control(maxiter=maxiter_nls,minFactor=minfactor_nls),algorithm="port",lower=lwr_nls,upper=upr_nls)
  K_start <- min(max(state_vax_df[state_vax_df$Series_Complete_12PlusPop_Pct_cummax > 0,"Series_Complete_12PlusPop_Pct_cummax"]),state_initVals[["Asym"]])
  v_start <- 1/state_initVals[["scal"]]
  p0_start <- state_initVals[["Asym"]]/(exp(state_initVals[["xmid"]]/state_initVals[["scal"]])+1)
  
  # Fit the model
  model_Complete_12Plus <- nls(Series_Complete_12PlusPop_Pct_cummax~K*p0*exp(v*(t-min_t))/(K+p0*(exp(v*(t-min_t))-1)),start=list(p0=p0_start,K=K_start,v=v_start),data=state_vax_df[state_vax_df$t >= min_t,],control=nls.control(maxiter=maxiter_nls,minFactor=minfactor_nls),algorithm="port",lower=lwr_nls,upper=upr_nls)
  
  # Get a sense of goodness of fit: correlation and mean absolute error
  cor_Complete_12Plus <- cor(state_vax_df$Series_Complete_12PlusPop_Pct_cummax[state_vax_df$t >= min_t],predict(model_Complete_12Plus))
  mae_Complete_12Plus <- mean(abs(state_vax_df$Series_Complete_12PlusPop_Pct_cummax[state_vax_df$t >= min_t]-predict(model_Complete_12Plus)))
  
  # Results for state and age group 12+
  df_Complete_12Plus <- data.frame(t(c(state,coef(model_Complete_12Plus)[[1]],coef(model_Complete_12Plus)[[2]],coef(model_Complete_12Plus)[[3]],cor_Complete_12Plus,mae_Complete_12Plus)))
  names(df_Complete_12Plus) <- c("State","p0_Complete_12Plus","K_Complete_12Plus","v_Complete_12Plus","correlation_Complete_12Plus","MAE_Complete_12Plus")
  df_Complete_12Plus$p0_Complete_12Plus <- as.numeric(df_Complete_12Plus$p0_Complete_12Plus)
  df_Complete_12Plus$K_Complete_12Plus <- as.numeric(df_Complete_12Plus$K_Complete_12Plus)
  df_Complete_12Plus$v_Complete_12Plus <- as.numeric(df_Complete_12Plus$v_Complete_12Plus)
  df_Complete_12Plus$correlation_Complete_12Plus <- cor_Complete_12Plus
  df_Complete_12Plus$MAE_Complete_12Plus <- mae_Complete_12Plus
  print(paste0('Check the dimension of the table of results for age group 12+: ', dim(df_Complete_12Plus)))
  
  # Overlay actual and fitted vaccination curves 
  pdf(paste0(state,"_StateLevel_12Plus_Complete_with_constraints_", today_date,".pdf"),paper='USr')
  p <- plot(state_vax_df$Series_Complete_12PlusPop_Pct_cummax,type="l",xlab="Time (in days)", ylab="% fully vaccinated",main=paste0(state, " - 12+"),ylim=c(0,102),yat=seq(0,100,20))
  p <- p + lines(state_vax_df$t[state_vax_df$t >= min_t],predict(model_Complete_12Plus),col="red",lty=2,lwd=3)
  print(p)
  dev.off()
  
  # Full vaccination - 12-17
  
  # Only consider the time points for which the outcome is > 0
  min_t <- min(state_vax_df[state_vax_df$Series_Complete_12_17_Pop_Pct_cummax > 0,"t"])
  
  # Initiate parameter values for (K, v, p0)
  state_initVals <- getInitial(Series_Complete_12_17_Pop_Pct_cummax ~ SSlogis(t-min_t, Asym, xmid, scal), data = state_vax_df[state_vax_df$t >= min_t,],control=nls.control(maxiter=maxiter_nls,minFactor=minfactor_nls),algorithm="port",lower=lwr_nls,upper=upr_nls)
  K_start <- min(max(state_vax_df[state_vax_df$Series_Complete_12_17_Pop_Pct_cummax > 0,"Series_Complete_12_17_Pop_Pct_cummax"]),state_initVals[["Asym"]])
  v_start <- 1/state_initVals[["scal"]]
  p0_start <- state_initVals[["Asym"]]/(exp(state_initVals[["xmid"]]/state_initVals[["scal"]])+1)
  
  # Fit the model
  model_Complete_12_17 <- nls(Series_Complete_12_17_Pop_Pct_cummax~K*p0*exp(v*(t-min_t))/(K+p0*(exp(v*(t-min_t))-1)),start=list(p0=p0_start,K=K_start,v=v_start),data=state_vax_df[state_vax_df$t >= min_t,],control=nls.control(maxiter=maxiter_nls,minFactor=minfactor_nls),algorithm="port",lower=lwr_nls,upper=upr_nls)
  
  # Get a sense of goodness of fit: correlation and mean absolute error
  cor_Complete_12_17 <- cor(state_vax_df$Series_Complete_12_17_Pop_Pct_cummax[state_vax_df$t >= min_t],predict(model_Complete_12_17))
  mae_Complete_12_17 <- mean(abs(state_vax_df$Series_Complete_12_17_Pop_Pct_cummax[state_vax_df$t >= min_t]-predict(model_Complete_12_17)))
  df_Complete_12_17 <- data.frame(t(c(state,coef(model_Complete_12_17)[[1]],coef(model_Complete_12_17)[[2]],coef(model_Complete_12_17)[[3]],cor_Complete_12_17,mae_Complete_12_17)))
  
  # Results for state and age group 12-17
  names(df_Complete_12_17) <- c("State","p0_Complete_12_17","K_Complete_12_17","v_Complete_12_17","correlation_Complete_12_17","MAE_Complete_12_17")
  df_Complete_12_17$p0_Complete_12_17 <- as.numeric(df_Complete_12_17$p0_Complete_12_17)
  df_Complete_12_17$K_Complete_12_17 <- as.numeric(df_Complete_12_17$K_Complete_12_17)
  df_Complete_12_17$v_Complete_12_17 <- as.numeric(df_Complete_12_17$v_Complete_12_17)
  df_Complete_12_17$correlation_Complete_12_17 <- cor_Complete_12_17
  df_Complete_12_17$MAE_Complete_12_17 <- mae_Complete_12_17
  print(paste0('Check the dimension of the table of results for age group 12-17: ', dim(df_Complete_12_17)))
  
  # Overlay actual and fitted vaccination curves 
  pdf(paste0(state,"_StateLevel_12_17_Complete_bis_with_constraints_",today_date,".pdf"),paper='USr')
  p <- plot(state_vax_df$Series_Complete_12_17_Pop_Pct_cummax,type="l",xlab="Time (in days)", ylab="% fully vaccinated",main=paste0(state, " - 12-17"),ylim=c(0,102),yat=seq(0,100,20))
  p <- p + lines(state_vax_df$t[state_vax_df$t >= min_t],predict(model_Complete_12_17),col="red",lty=2,lwd=3)
  print(p)
  dev.off()
  
  # Full vaccination - 18+
  
  # Only consider the time points for which the outcome is > 0
  min_t <- min(state_vax_df[state_vax_df$Series_Complete_18PlusPop_Pct_cummax > 0,"t"])
  
  # Initiate parameter values for (K, v, p0)
  state_initVals <- getInitial(Series_Complete_18PlusPop_Pct_cummax ~ SSlogis(t-min_t, Asym, xmid, scal), data = state_vax_df[state_vax_df$t >= min_t,],control=nls.control(maxiter=maxiter_nls,minFactor=minfactor_nls),algorithm="port",lower=lwr_nls,upper=upr_nls)
  K_start <- min(max(state_vax_df[state_vax_df$Series_Complete_18PlusPop_Pct_cummax > 0,"Series_Complete_18PlusPop_Pct_cummax"]),state_initVals[["Asym"]])
  v_start <- 1/state_initVals[["scal"]]
  p0_start <- state_initVals[["Asym"]]/(exp(state_initVals[["xmid"]]/state_initVals[["scal"]])+1)
  
  # Fit the model
  model_Complete_18Plus <- nls(Series_Complete_18PlusPop_Pct_cummax~K*p0*exp(v*(t-min_t))/(K+p0*(exp(v*(t-min_t))-1)),start=list(p0=p0_start,K=K_start,v=v_start),data=state_vax_df[state_vax_df$t >= min_t,],control=nls.control(maxiter=maxiter_nls,minFactor=minfactor_nls),algorithm="port",lower=lwr_nls,upper=upr_nls)
  
  # Get a sense of goodness of fit
  cor_Complete_18Plus <- cor(state_vax_df$Series_Complete_18PlusPop_Pct_cummax[state_vax_df$t >= min_t],predict(model_Complete_18Plus))
  mae_Complete_18Plus <- mean(abs(state_vax_df$Series_Complete_18PlusPop_Pct_cummax[state_vax_df$t >= min_t]-predict(model_Complete_18Plus)))
  
  # Results for state and age group 18+
  df_Complete_18Plus <- data.frame(t(c(state,coef(model_Complete_18Plus)[[1]],coef(model_Complete_18Plus)[[2]],coef(model_Complete_18Plus)[[3]],cor_Complete_18Plus,mae_Complete_18Plus)))
  names(df_Complete_18Plus) <- c("State","p0_Complete_18Plus","K_Complete_18Plus","v_Complete_18Plus","correlation_Complete_18Plus","MAE_Complete_18Plus")
  df_Complete_18Plus$p0_Complete_18Plus <- as.numeric(df_Complete_18Plus$p0_Complete_18Plus)
  df_Complete_18Plus$K_Complete_18Plus <- as.numeric(df_Complete_18Plus$K_Complete_18Plus)
  df_Complete_18Plus$v_Complete_18Plus <- as.numeric(df_Complete_18Plus$v_Complete_18Plus)
  df_Complete_18Plus$correlation_Complete_18Plus <- cor_Complete_18Plus
  df_Complete_18Plus$MAE_Complete_18Plus <- mae_Complete_18Plus
  print(paste0('Check the dimension of the table of results for age group 18+: ', dim(df_Complete_18Plus)))
  
  # Overlay actual and fitted vaccination curves 
  pdf(paste0(state,"_StateLevel_18Plus_Complete_bis_with_constraints_",today_date,".pdf"),paper='USr')
  p <- plot(state_vax_df$Series_Complete_18PlusPop_Pct_cummax,type="l",xlab="Time (in days)", ylab="% fully vaccinated",main=paste0(state, " - 18+"),ylim=c(0,102),yat=seq(0,100,20))
  p <- p + lines(state_vax_df$t[state_vax_df$t >= min_t],predict(model_Complete_18Plus),col="red",lty=2,lwd=3)
  print(p)
  dev.off()
  
  # Full vaccination - 18-64
  
  # Only consider the time points for which the outcome is > 0
  min_t <- min(state_vax_df[state_vax_df$Series_Complete_18_64_Pop_Pct_cummax > 0,"t"])
  
  # Initiate parameter values for (K, v, p0)
  state_initVals <- getInitial(Series_Complete_18_64_Pop_Pct_cummax ~ SSlogis(t-min_t, Asym, xmid, scal), data = state_vax_df[state_vax_df$t >= min_t,],control=nls.control(maxiter=maxiter_nls,minFactor=minfactor_nls),algorithm="port",lower=lwr_nls,upper=upr_nls)
  K_start<-min(max(state_vax_df[state_vax_df$Series_Complete_18_64_Pop_Pct_cummax > 0,"Series_Complete_18_64_Pop_Pct_cummax"]),state_initVals[["Asym"]])
  v_start<-1/state_initVals[["scal"]]
  p0_start<-state_initVals[["Asym"]]/(exp(state_initVals[["xmid"]]/state_initVals[["scal"]])+1)
  
  # Fit the model
  model_Complete_18_64 <- nls(Series_Complete_18_64_Pop_Pct_cummax~K*p0*exp(v*(t-min_t))/(K+p0*(exp(v*(t-min_t))-1)),start=list(p0=p0_start,K=K_start,v=v_start),data=state_vax_df[state_vax_df$t >= min_t,],control=nls.control(maxiter=maxiter_nls,minFactor=minfactor_nls),algorithm="port",lower=lwr_nls,upper=upr_nls)
  
  # Get a sense of goodness of fit
  cor_Complete_18_64 <- cor(state_vax_df$Series_Complete_18_64_Pop_Pct_cummax[state_vax_df$t >= min_t],predict(model_Complete_18_64))
  mae_Complete_18_64 <- mean(abs(state_vax_df$Series_Complete_18_64_Pop_Pct_cummax[state_vax_df$t >= min_t]-predict(model_Complete_18_64)))
  
  # Results for state and age group 18-64
  df_Complete_18_64 <- data.frame(t(c(state,coef(model_Complete_18_64)[[1]],coef(model_Complete_18_64)[[2]],coef(model_Complete_18_64)[[3]],cor_Complete_18_64,mae_Complete_18_64)))
  names(df_Complete_18_64) <- c("State","p0_Complete_18_64","K_Complete_18_64","v_Complete_18_64","correlation_Complete_18_64","MAE_Complete_18_64")
  df_Complete_18_64$p0_Complete_18_64 <- as.numeric(df_Complete_18_64$p0_Complete_18_64)
  df_Complete_18_64$K_Complete_18_64 <- as.numeric(df_Complete_18_64$K_Complete_18_64)
  df_Complete_18_64$v_Complete_18_64 <- as.numeric(df_Complete_18_64$v_Complete_18_64)
  df_Complete_18_64$correlation_Complete_18_64 <- cor_Complete_18_64
  df_Complete_18_64$MAE_Complete_18_64 <- mae_Complete_18_64
  print(paste0('Check the dimension of the table of results for age group 18-64: ', dim(df_Complete_18_64)))
  
  # Overlay actual and fitted vaccination curves
  pdf(paste0(state,"_StateLevel_18_64_Complete_bis_with_constraints_",today_date,".pdf"),paper='USr')
  p <- plot(state_vax_df$Series_Complete_18_64_Pop_Pct_cummax,type="l",xlab="Time (in days)", ylab="% fully vaccinated",main=paste0(state, " - 18-64"),ylim=c(0,102),yat=seq(0,100,20))
  p <- p + lines(state_vax_df$t[state_vax_df$t >= min_t],predict(model_Complete_18_64),col="red",lty=2,lwd=3)
  print(p)
  dev.off()
  
  # Full vaccination - 65+
  
  # Only consider the time points for which the outcome is > 0
  min_t <- min(state_vax_df[state_vax_df$Series_Complete_65PlusPop_Pct_cummax > 0,"t"])
  
  # Initiate parameter values for (K, v, p0)
  state_initVals <- getInitial(Series_Complete_65PlusPop_Pct_cummax ~ SSlogis(t-min_t, Asym, xmid, scal), data = state_vax_df[state_vax_df$t >= min_t,],control=nls.control(maxiter=maxiter_nls,minFactor=minfactor_nls),algorithm="port",lower=lwr_nls,upper=upr_nls)
  K_start <- min(max(max(state_vax_df[state_vax_df$Series_Complete_65PlusPop_Pct_cummax > 0,"Series_Complete_65PlusPop_Pct_cummax"])),state_initVals[["Asym"]])
  v_start <- 1/state_initVals[["scal"]]
  p0_start <- state_initVals[["Asym"]]/(exp(state_initVals[["xmid"]]/state_initVals[["scal"]])+1)
  
  # Fit the model
  model_Complete_65Plus <- nls(Series_Complete_65PlusPop_Pct_cummax~K*p0*exp(v*(t-min_t))/(K+p0*(exp(v*(t-min_t))-1)),start=list(p0=p0_start,K=K_start,v=v_start),data=state_vax_df[state_vax_df$t >= min_t,],control=nls.control(maxiter=maxiter_nls,minFactor=minfactor_nls),algorithm="port",lower=lwr_nls,upper=upr_nls)
  
  # Get a sense of goodness of fit
  cor_Complete_65Plus <- cor(state_vax_df$Series_Complete_65PlusPop_Pct_cummax[state_vax_df$t >= min_t],predict(model_Complete_65Plus))
  mae_Complete_65Plus <- mean(abs(state_vax_df$Series_Complete_65PlusPop_Pct_cummax[state_vax_df$t >= min_t]-predict(model_Complete_65Plus)))
  
  # Results for state and age group 65+
  df_Complete_65Plus <- data.frame(t(c(coef(model_Complete_65Plus)[[1]],coef(model_Complete_65Plus)[[2]],coef(model_Complete_65Plus)[[3]],cor_Complete_65Plus,mae_Complete_65Plus)))
  names(df_Complete_65Plus) <- c("p0_Complete_65Plus","K_Complete_65Plus","v_Complete_65Plus","correlation_Complete_65Plus","MAE_Complete_65Plus")
  df_Complete_65Plus$p0_Complete_65Plus <- as.numeric(df_Complete_65Plus$p0_Complete_65Plus)
  df_Complete_65Plus$K_Complete_65Plus <- as.numeric(df_Complete_65Plus$K_Complete_65Plus)
  df_Complete_65Plus$v_Complete_65Plus <- as.numeric(df_Complete_65Plus$v_Complete_65Plus)
  df_Complete_65Plus$correlation_Complete_65Plus <- cor_Complete_65Plus
  df_Complete_65Plus$MAE_Complete_65Plus <- mae_Complete_65Plus
  print(paste0('Check the dimension of the table of results for age group 65+: ', dim(df_Complete_65Plus)))
  
  # Overlay actual and fitted vaccination curves
  pdf(paste0(state,"_StateLevel_65Plus_Complete_bis_with_constraints_",today_date,".pdf"),paper='USr')
  p <- plot(state_vax_df$Series_Complete_65PlusPop_Pct_cummax,type="l",xlab="Time (in days)", ylab="% fully vaccinated",main=paste0(state, " - 65+"),ylim=c(0,102),yat=seq(0,100,20))
  p <- p + lines(state_vax_df$t[state_vax_df$t >= min_t],predict(model_Complete_65Plus),col="red",lty=2,lwd=3)
  print(p)
  dev.off()
  
  # Compile results for all age groups
  
  # If we are fitting the first state of the list
  if (state_iter == 0){
    state_Complete_12Plus <- df_Complete_12Plus
    state_Complete_12_17 <- df_Complete_12_17
    state_Complete_18Plus <- df_Complete_18Plus
    state_Complete_18_64 <- df_Complete_18_64
    state_Complete_65Plus <- df_Complete_65Plus
  }
  # If we are further in the process
  else{
    state_Complete_12Plus <- rbind(state_Complete_12Plus,df_Complete_12Plus)
    state_Complete_12_17 <- rbind(state_Complete_12_17,df_Complete_12_17)
    state_Complete_18Plus <- rbind(state_Complete_18Plus,df_Complete_18Plus)
    state_Complete_18_64 <- rbind(state_Complete_18_64,df_Complete_18_64)
    state_Complete_65Plus <- rbind(state_Complete_65Plus,df_Complete_65Plus)
  }
  # Update iterator to reflect the completion of the fitting process for an additional state
  state_iter <- state_iter + 1
  
  # Compile results for all age groups into a single dataframe
  state_Complete <- cbind(state_Complete_12Plus,state_Complete_12_17,state_Complete_18Plus,state_Complete_18_64,state_Complete_65Plus)
  # Write results for all considered states
  write.csv(state_Complete,paste0("C:/Users/rchen13/Box/Dementia Excess mortality/Paper 2_vaccination and ADRD excess deaths/writing/submission/JAMA neurology/code/StateLevel_Complete_with_constraints_",today_date,".csv"),row.names=FALSE)
}

state_iter <- 0
for (state in state_list_65Plus){
  
  # Select state
  print(paste0('State:', state))
  state_vax_df <- vax_state_df[vax_state_df$Recip_State == state,]
  # Dimension of the dataset
  print(paste0('Dimension of the dataset: ', dim(state_vax_df)))
  
  # Initiate the start of the time series at 0, but it will get updated later to ensure goodness-of-fit
  min_t <- 0
  
  # Truncate the time series on the right at t=max_t
  state_vax_df <- state_vax_df[state_vax_df$t <= max_t,]
  # Dimension of the updated dataset
  print(paste0('Dimension of the dataset after right truncation: ', dim(state_vax_df)))
  
  # Full vaccination - 65+
  
  # Only consider the time points for which the outcome is > 0
  min_t <- min(state_vax_df[state_vax_df$Series_Complete_65PlusPop_Pct_cummax > 0,"t"])
  
  # Initiate parameter values for (K, v, p0)
  state_initVals <- getInitial(Series_Complete_65PlusPop_Pct_cummax ~ SSlogis(t-min_t, Asym, xmid, scal), data = state_vax_df[state_vax_df$t >= min_t,],control=nls.control(maxiter=maxiter_nls,minFactor=minfactor_nls),algorithm="port",lower=lwr_nls,upper=upr_nls)
  K_start <- min(max(max(state_vax_df[state_vax_df$Series_Complete_65PlusPop_Pct_cummax > 0,"Series_Complete_65PlusPop_Pct_cummax"])),state_initVals[["Asym"]])
  v_start <- 1/state_initVals[["scal"]]
  p0_start <- state_initVals[["Asym"]]/(exp(state_initVals[["xmid"]]/state_initVals[["scal"]])+1)
  
  # Fit the model
  model_Complete_65Plus <- nls(Series_Complete_65PlusPop_Pct_cummax~K*p0*exp(v*(t-min_t))/(K+p0*(exp(v*(t-min_t))-1)),start=list(p0=p0_start,K=K_start,v=v_start),data=state_vax_df[state_vax_df$t >= min_t,],control=nls.control(maxiter=maxiter_nls,minFactor=minfactor_nls),algorithm="port",lower=lwr_nls,upper=upr_nls)
  
  # Get a sense of goodness of fit
  cor_Complete_65Plus <- cor(state_vax_df$Series_Complete_65PlusPop_Pct_cummax[state_vax_df$t >= min_t],predict(model_Complete_65Plus))
  mae_Complete_65Plus <- mean(abs(state_vax_df$Series_Complete_65PlusPop_Pct_cummax[state_vax_df$t >= min_t]-predict(model_Complete_65Plus)))
  
  # Results for state and age group 65+
  df_Complete_65Plus <- data.frame(t(c(state,coef(model_Complete_65Plus)[[1]],coef(model_Complete_65Plus)[[2]],coef(model_Complete_65Plus)[[3]],cor_Complete_65Plus,mae_Complete_65Plus)))
  names(df_Complete_65Plus) <- c("State","p0_Complete_65Plus","K_Complete_65Plus","v_Complete_65Plus","correlation_Complete_65Plus","MAE_Complete_65Plus")
  df_Complete_65Plus$p0_Complete_65Plus <- as.numeric(df_Complete_65Plus$p0_Complete_65Plus)
  df_Complete_65Plus$K_Complete_65Plus <- as.numeric(df_Complete_65Plus$K_Complete_65Plus)
  df_Complete_65Plus$v_Complete_65Plus <- as.numeric(df_Complete_65Plus$v_Complete_65Plus)
  df_Complete_65Plus$correlation_Complete_65Plus <- cor_Complete_65Plus
  df_Complete_65Plus$MAE_Complete_65Plus <- mae_Complete_65Plus
  print(paste0('Check the dimension of the table of results for age group 65+: ', dim(df_Complete_65Plus)))
  
  # Overlay actual and fitted vaccination curves
  pdf(paste0(state,"_StateLevel_65Plus_Complete_bis_with_constraints_",today_date,".pdf"),paper='USr')
  p <- plot(state_vax_df$Series_Complete_65PlusPop_Pct_cummax,type="l",xlab="Time (in days)", ylab="% fully vaccinated",main=paste0(state, " - 65+"),ylim=c(0,102),yat=seq(0,100,20))
  p <- p + lines(state_vax_df$t[state_vax_df$t >= min_t],predict(model_Complete_65Plus),col="red",lty=2,lwd=3)
  print(p)
  dev.off()
  
  # Compile results for all age groups
  
  # If we are fitting the first state of the list
  if (state_iter == 0){
    state_Complete_65Plus <- df_Complete_65Plus
  }
  # If we are further in the process
  else{
    state_Complete_65Plus <- rbind(state_Complete_65Plus,df_Complete_65Plus)
  }
  # Update iterator to reflect the completion of the fitting process for an additional state
  state_iter <- state_iter + 1
  
  # Write results for the remaining states
  write.csv(state_Complete_65Plus,paste0("C:/Users/rchen13/Box/Dementia Excess mortality/Paper 2_vaccination and ADRD excess deaths/writing/submission/JAMA neurology/code/StateLevel_Complete_Remaining_with_constraints_",today_date,".csv"),row.names=FALSE)
}
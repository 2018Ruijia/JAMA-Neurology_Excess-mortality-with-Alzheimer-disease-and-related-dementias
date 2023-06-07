###read non-standardized data 

state_non_stand_df<- readRDS('results/01_data/state_non_stand_year_df.RDS')
#state_stand_df<-readRDS('results/01_data/state_stand_year_df.RDS')

##################################create a change score using year 1- year 2/year 1

state_change_non_stand_df <- state_non_stand_df %>% select(state.x, Year, excess_capita) %>% dplyr::rename(state=state.x)

###############convert long to wide##################################
state_change_non_stand_df <- state_change_non_stand_df %>%  pivot_wider (
    names_from = Year,
    names_sep = "",
    values_from= excess_capita) %>% 
    dplyr::rename (Year1="Year 1",
                   Year2="Year 2") %>% 
    mutate (exc_diff= (Year2-Year1)/Year1*100,
            exc_diff_ter = ntile(Year1, 3),
            exc_diff_med = ntile(Year1, 2))%>% 
    mutate (exc_diff_ter_new=ifelse (exc_diff_ter==1, 1, 2)) %>% 
    select(state, exc_diff, exc_diff_med, exc_diff_ter,exc_diff_ter_new)



# ###read the k and v file
 kv_df_complete <- read.csv('results/01_data/StateLevel_65Plus_FullVaccination_20220903.csv')
 kv_df_complete$state <- abbr2state(kv_df_complete$State)


####merge the k and v file 

state_all_Vax_df <-
    state_change_non_stand_df  %>%
    full_join(kv_df_complete, by = "state", all = TRUE) 



saveRDS(state_all_Vax_df, 'results/01_data/state_all_Vax_df_updated.RDS')


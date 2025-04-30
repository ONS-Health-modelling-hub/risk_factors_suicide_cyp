#--------------------------
# Set up the spark connection
#--------------------------

project_directory <- "project_name"

source(paste0(project_directory, "/", "0_spark_setup.R"))

#--------------------------
# Read in PHDA 
#--------------------------

df_hes_phda <- sparklyr::sdf_sql(
  sc, "Select * From project_name.nihr_aim_1b_person_level_2025_02_14")

#--------------------------
#  Quality assurance - PHDA  
#--------------------------

# Check deaths by sex
check_deaths_s2 <- df_hes_phda %>% 
  dplyr::group_by(sex, cause_death) %>% 
  dplyr::count() %>% 
  dplyr::arrange(sex, cause_death) %>%
  sparklyr::collect() %>% 
  print()

# Check deaths by age at tod
check_deaths_s3 <- df_hes_phda %>% 
  dplyr::group_by(age_at_tod, cause_death) %>% 
  dplyr::count() %>% 
  dplyr::arrange(age_at_tod, cause_death) %>% 
  sparklyr::collect() %>% 
  print()

### Age distribution
age_distribution <- df_hes_phda %>%
  dplyr::group_by(death, 
                  cause_death, 
                  age_on_cd, 
                  ethnicity_short,
                  disability,
                  tenure_short, 
                  fam_type_short,
                  nssec_3) %>% 
  dplyr::count() %>%
  collect() %>%
  print()


#--------------------------
#  Quality assurance - deaths by suicide    
#--------------------------

## Create total number of events by age at death, month and year
total_events <- df_hes_phda %>%
  sparklyr::filter(!(cause_death == "Alive")) %>%
  sparklyr::mutate(
    dod_month_year = substr(dod_deaths, 1, 7),
    dod_year = substr(dod_deaths, 1, 4),
    dod_month = substr(dod_deaths, 6, 7)) %>%
  sparklyr::select(sex, cause_death, age_at_tod, dod_month_year, dod_year, dod_month) %>%
  dplyr::group_by(sex, cause_death, age_at_tod, dod_month_year, dod_year, dod_month) %>%    
  dplyr::summarize(count = n())

write.csv(total_events, "project_name/output/total_events.csv")

# By sex and year
plot_year <- total_events %>%
  dplyr::group_by(cause_death, dod_year) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::arrange(cause_death, dod_year)

write.csv(plot_year, paste0(project_directory, "plot_year.csv")

p_year <- ggplot(plot_year, aes(x = dod_year, y = count, fill = as.factor(cause_death))) +
  geom_bar(stat = 'identity')

p_year

ggsave(paste0(project_directory, "/plots/p_year.jpg"))

# By sex and month
plot_month <- total_events %>%
  dplyr::group_by(sex, dod_month) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::arrange(sex, dod_month)

write.csv(plot_month, paste0(project_directory, "plot_month.csv")

p_month <- ggplot(plot_month, aes(x = dod_month, y = count, fill = as.factor(sex))) +
  geom_bar(stat = 'identity')

p_month

ggsave(paste0(project_directory, "/plots/p_month.jpg"))

# By age
plot_age <- total_events %>%
  dplyr::group_by(sex, age_at_tod) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::arrange(sex, age_at_tod)

write.csv(plot_age, paste0(project_directory, "plot_age.csv")

p_age <- ggplot(plot_age, aes(x = age_at_tod, y = count, fill = as.factor(sex))) +
  geom_bar(stat = 'identity')

p_age
          
ggsave(paste0(project_directory, "/plots/p_age.jpg"))

#--------------------------
#  Quality assurance - Time at risk    
#--------------------------

time_at_risk_summary <- df_hes_phda %>% 
 sparklyr::mutate(time_in_years = time_at_risk / 365.25,
                  total_time_in_years = total_time_at_risk / 365.25) %>%
  dplyr::summarize(min_years = min(total_time_in_years),
                   max_years = max(total_time_in_years),
                   mean_years = mean(total_time_in_years),
                   min_days = min(time_at_risk),                
                   max_days = max(time_at_risk),
                   mean_days = mean(time_at_risk)) %>%
          print()
          
time_at_risk_qa <- df_hes_phda %>%
  sparklyr::mutate(time_in_years = time_at_risk / 365.25,
                   total_time_in_years = total_time_at_risk / 365.25) %>%
  dplyr::group_by(age_on_cd, sex) %>%   
  dplyr::summarise(mean =  mean(total_time_in_years)) %>%
          arrange(age_on_cd, sex) %>%
          collect()
              
time_at_risk_qa %>% print()
          
p_tar_age_sex <- ggplot(time_at_risk_qa, aes(x = age_on_cd, y = mean, fill = as.factor(sex))) +
  geom_bar(stat = 'identity')
          
p_tar_age_sex


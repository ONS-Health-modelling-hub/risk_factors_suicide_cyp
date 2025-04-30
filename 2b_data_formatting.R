#--------------------------
## Set up the spark connection
#--------------------------

project_directory <- "project_name"

source(paste0(project_directory, "/", "1_spark_setup.R"))

#--------------------------
## Read in PHDA 
#--------------------------

df_hes_phda <- sparklyr::sdf_sql(
  sc, "Select * From project_name.nihr_aim_1b_person_level")

# Select only variables needed for analysis
df_hes_phda %<>%
  select("il4_person_id_census", 
         "age_on_cd", 
         "age_at_tod", 
         "date_of_10bday",
         "death_18b_flag", 
         "time_at_risk", 
         "time_at_risk_recalculate",
         "total_time_at_risk", 
         "cause_death"
         "outcome_suicide", 
         "outcome_death",
         "death",
         # Individual characteristics
         "sex", 
         "ethnicity_short",
         "main_language",
         "carer_binary", 
         "disability",
         # Household characteristics         
         "qualifications_hrp_short",       
         "religion_hrp_short",
         "nssec_3",
         "fam_type_short",
         "tenure_modelling",
         "region",
         "hrp_age") 

#--------------------------
## Sampling  
#--------------------------

frac_death = 1
frac_alive = 0.20

df_sample = rbind(
  df_hes_phda %>%
    sparklyr::filter(death == 1) %>%
    sparklyr::mutate(weight = 1 / frac_death),
  df_hes_phda %>%
    sparklyr::filter(death == 0) %>%
    sparklyr::sdf_sample(fraction = frac_alive, replacement = FALSE, seed = 1) %>%
    sparklyr::mutate(weight = 1 / frac_alive))

# Check expected rows looks appropriate 
sparklyr::sdf_nrow(df_sample)

# Check proportions of person level data and sampled data
sex_df_hes_phda <- df_hes_phda %>%
  filter(death == 0) %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = round((count/sum(count) * 100), 2))

sex_df_sample <- df_sample %>%
  filter(death == 0) %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = round((count/sum(count) * 100), 2))

ethnicity_df_hes_phda <- df_hes_phda %>%
  filter(death == 0) %>%
  dplyr::group_by(ethnicity_short) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = round((count/sum(count) * 100), 2))

ethnicity_df_sample <- df_sample %>%
  filter(death == 0) %>%
  dplyr::group_by(ethnicity_short) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = round((count/sum(count) * 100), 2))

nssec_df_hes_phda <- df_hes_phda %>%
  filter(death == 0) %>%
  dplyr::group_by(nssec_3) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = round((count/sum(count) * 100), 2))

nssec_df_sample <- df_sample %>%
  filter(death == 0) %>%
  dplyr::group_by(nssec_3) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = round((count/sum(count) * 100), 2))

#--------------------------
## survSplit  
#--------------------------

df_split <- survSplit(Surv(time_at_risk, death) ~ ., # updated to death - recalculate suicide variable after split
                      data = df_sample,
                      cut = c(365.25, 730.5, 1095.75, 1461, 1826.25, 2191.5, 2556.75, 2922),
                      episode = "time_ep")

nrow(df_split)

# Change time_at_risk to non-cumulative
df_split <- df_split %>%
  sparklyr::mutate(time_at_risk_new = time_at_risk - tstart)

# Change time at risk for those who age in so their first episode is from Census day
df_split <- df_split %>% 
  mutate(
    time_at_risk_new = case_when(
      date_of_10bday < start_date & time_ep == 1 ~ time_at_risk - time_at_risk_recalculate,
      TRUE ~ time_at_risk_new))

# Update outcome_suicide to be unique to event per person rather than 1 for all rows a person appears not just their death event row 
df_split %<>%
  mutate(
    outcome_suicide_new = case_when(
      outcome_suicide == 1 & death == 1 ~ 1,
      TRUE ~ 0))

# Calculate age in each interval
df_split <- df_split %>%
 mutate(
    age_in_interval = case_when(
      (date_of_10bday <  start_date) & (time_ep == 1) ~ age_on_cd,
      (date_of_10bday <  start_date) & (time_ep == 2) ~ age_on_cd + 1,
      (date_of_10bday <  start_date) & (time_ep == 3) ~ age_on_cd + 2,
      (date_of_10bday <  start_date) & (time_ep == 4) ~ age_on_cd + 3,
      (date_of_10bday <  start_date) & (time_ep == 5) ~ age_on_cd + 4,
      (date_of_10bday <  start_date) & (time_ep == 6) ~ age_on_cd + 5,
      (date_of_10bday <  start_date) & (time_ep == 7) ~ age_on_cd + 6,
      (date_of_10bday <  start_date) & (time_ep == 8) ~ age_on_cd + 7,
      (date_of_10bday <  start_date) & (time_ep == 9) ~ age_on_cd + 8,
      (date_of_10bday >= start_date) & (time_ep == 1) ~ 10,
      (date_of_10bday >= start_date) & (time_ep == 2) ~ 11,
      (date_of_10bday >= start_date) & (time_ep == 3) ~ 12,
      (date_of_10bday >= start_date) & (time_ep == 4) ~ 13,
      (date_of_10bday >= start_date) & (time_ep == 5) ~ 14,
      (date_of_10bday >= start_date) & (time_ep == 6) ~ 15,
      (date_of_10bday >= start_date) & (time_ep == 7) ~ 16,
      (date_of_10bday >= start_date) & (time_ep == 8) ~ 17,
      (date_of_10bday >= start_date) & (time_ep == 9) ~ 18))

# Create calender time variable
df_split <- df_split %>%
 sparklyr::mutate(
   year_of_10bday = as.integer(substr(date_of_10bday, 1, 4)),
    calender_time = dplyr::case_when(
      (date_of_10bday <  start_date) & (time_ep == 1) ~ 2011,
      (date_of_10bday <  start_date) & (time_ep == 2) ~ 2012,
      (date_of_10bday <  start_date) & (time_ep == 3) ~ 2013,
      (date_of_10bday <  start_date) & (time_ep == 4) ~ 2014,
      (date_of_10bday <  start_date) & (time_ep == 5) ~ 2015,
      (date_of_10bday <  start_date) & (time_ep == 6) ~ 2016,
      (date_of_10bday <  start_date) & (time_ep == 7) ~ 2017,
      (date_of_10bday <  start_date) & (time_ep == 8) ~ 2018,
      (date_of_10bday <  start_date) & (time_ep == 9) ~ 2019,
      (date_of_10bday >= start_date) & (time_ep == 1) ~ year_of_10bday,
      (date_of_10bday >= start_date) & (time_ep == 2) ~ year_of_10bday + 1,
      (date_of_10bday >= start_date) & (time_ep == 3) ~ year_of_10bday + 2,
      (date_of_10bday >= start_date) & (time_ep == 4) ~ year_of_10bday + 3,
      (date_of_10bday >= start_date) & (time_ep == 5) ~ year_of_10bday + 4,
      (date_of_10bday >= start_date) & (time_ep == 6) ~ year_of_10bday + 5,
      (date_of_10bday >= start_date) & (time_ep == 7) ~ year_of_10bday + 6,
      (date_of_10bday >= start_date) & (time_ep == 8) ~ year_of_10bday + 7,
      (date_of_10bday >= start_date) & (time_ep == 9) ~ year_of_10bday + 8))

# Filter out 18 year olds -these are filtered in 2_data_preparation.R KEEP as the age in interval code adds a row for age 18 year olds back in
df_split <- df_split %>%
  filter(age_in_interval < 18,
         time_at_risk_new > 0)

# Can't use outcome_suicide or cause_death as listed for each time_episode
# Create new variable to indicate outcome at time_episode of death variables 
df_split <- df_split %>% 
  mutate(
    outcome_death_interval = case_when(
      outcome_suicide == 1 & death == 1  ~ "Death_Suicide",
      outcome_suicide != 1 & death == 1 ~ "Death_Other",
      TRUE ~ "Alive"))

#--------------------------
## Quality assurance - interval level data  
#--------------------------

## Check weights
## Data split by death
df_split %>%
  group_by(outcome_death_interval, weight) %>%
  count() %>%
  print()

#--------------------------
## Save out interval data 
#--------------------------

## Store to RDS
saveRDS(df_split, paste0(results_path, "df_split_20.RDS"))
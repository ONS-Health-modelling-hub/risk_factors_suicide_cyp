#--------------------------
## Set up the spark connection
#--------------------------

project_directory <- "project_name"

source(paste0(project_directory, "/1_spark_setup.R"))

#--------------------------
## Crude rates by demographics
#--------------------------

# Read in person level data
df_hes_phda <- sparklyr::sdf_sql(
  sc, paste0("Select * From ", project_directory, ".nihr_aim_1b_person_level"))

#--------------------------
## All crude rates by demographics
#--------------------------

# Set-up purr loop for all characteristics
demog_varsnames <- c(#"disability",
                     "ethnicity_short",
                     "qualifications_hrp_short",
                     "main_language",
                     "nssec_3",
                     "region",
                     "religion_hrp_short",
                     "sex")


suicide_cols_to_suppress <- c("Death_Suicide", "Rate_Suicide")
other_cols_to_suppress <- c("Death_Other", "Rate_Other")

demog_table_suicides <- purrr::pmap_dfr(
  .l = list(data = purrr::map(1:length(demog_varsnames), function(e) {return(df_hes_phda)}),
            by_var = demog_varsnames),
  .f = crude_rates_fun) %>%
  dplyr::mutate(
    totalsample = ifelse(totalsample <10 | is.na(totalsample), "[c]", totalsample),     
    population_percent = ifelse(totalsample <10 | is.na(totalsample), "[c]", population_percent), 
    across(all_of(suicide_cols_to_suppress), ~ifelse(Death_Suicide < 10 | is.na(Death_Suicide), "[c]", .)),
    across(all_of(other_cols_to_suppress), ~ifelse(Death_Other < 10 | is.na(Death_Other), "[c]", .))) 

write.csv(demog_table_suicides,
          paste0(results_path, "crude_rates.csv"))

#--------------------------
## Crude rates by demographics by sex
#--------------------------

# Set up for rates by sex
# Set-up purr loop for all characteristics
demog_by_sex_varsnames <- c("disability", 
                            "ethnicity_short",
                            "qualifications_hrp_short",
                            "main_language",
                            "nssec_3",
                            "region",
                            "religion_hrp_short")


demog_table_suicides_sex <- purrr::pmap_dfr(
   .l = list(data = purrr::map(1:length(demog_by_sex_varsnames), function(e) {return(df_hes_phda)}),
             by_var = demog_by_sex_varsnames),
   .f = crude_rates_sex_fun) %>%
  dplyr::mutate(
    totalsample = ifelse(totalsample <10, "[c]", totalsample), 
    population_percent = ifelse(totalsample <10 | is.na(totalsample), "[c]", population_percent), 
    across(all_of(suicide_cols_to_suppress), ~ifelse(Death_Suicide < 10 | is.na(Death_Suicide), "[c]", .)),
    across(all_of(other_cols_to_suppress), ~ifelse(Death_Other < 10 | is.na(Death_Other), "[c]", .)))

write.csv(demog_table_suicides_sex,
          paste0(results_path, "crude_sex_rates.csv"))

#--------------------------
## Person year rates by demographics
#--------------------------
cols_to_suppress <- c("deaths", "Rate_Death", "lower_ci", "upper_ci")

person_year_rates <- purrr::pmap_dfr(
  .l = list(data = purrr::map(1:length(demog_varsnames), function(e) {return(df_hes_phda)}),
            by_var = demog_varsnames),
  .f = person_year_rates_fun) %>%
  dplyr::mutate(
    totalsample = ifelse(totalsample <10 | is.na(totalsample), "[c]", totalsample),
    across(all_of(cols_to_suppress), ~ifelse(deaths < 10 | is.na(deaths), "[c]", .)))  

write.csv(person_year_rates,
          paste0(results_path, "person_year_rates.csv"))

#--------------------------
## Person year rates by demographics and by sex
#--------------------------

person_year_sex_rates <- purrr::pmap_dfr(
  .l = list(data = purrr::map(1:length(demog_by_sex_varsnames), function(e) {return(df_hes_phda)}),
            by_var = demog_by_sex_varsnames),
  .f = person_year_rates_sex_fun) %>%
  dplyr::mutate(
    totalsample = ifelse(totalsample <10 | is.na(totalsample), "[c]", totalsample),
    across(all_of(cols_to_suppress), ~ifelse(deaths < 10 | is.na(deaths), "[c]", .)))  


write.csv(person_year_sex_rates,
          paste0(results_path, "person_year_rates_by_sex.csv"))

#--------------------------
## Person year rates by age in interval on split dataset
#--------------------------

# Run on interval level dataset
df_split <- read_rds(paste0(results_path, "df_split_20.RDS")

#--------------------------
## Rates in person years
#--------------------------

person_year_age_rates <- calculate_rates_in_person_years(
  df_split, age = "age_in_interval") %>%
  dplyr::mutate(
    totalsample = ifelse(totalsample <10 | is.na(totalsample), "[c]", totalsample),
    across(all_of(cols_to_suppress), ~ifelse(deaths < 10 | is.na(deaths), "[c]", .)))

write.csv(person_year_age_rates,
          paste0(results_path, "person_year_age_rates.csv"))

#--------------------------
## Set up the spark connection
#--------------------------

project_directory <- "project_name"

source(paste0(project_directory, "/", "1_spark_setup.R"))

#--------------------------
## Census and deaths
#--------------------------

# Import Census 
census <- sparklyr::sdf_sql(
  sc, "Select * From project_name.deidentified_census_2011_v2")

# Import deaths
deaths <- sdf_sql(sc,
                  "SELECT doddy, dodmt, dodyr, fic10und, census2011_person_id
                    FROM project_name.deaths_2023_std_v2
                    UNION ALL

                  SELECT doddy, dodmt, dodyr, fic10und, census2011_person_id
                    FROM project_name.deaths_2022_std_v2
                    UNION ALL

                  SELECT doddy, dodmt, dodyr, fic10und, census2011_person_id
                    FROM project_name.deaths_2021_std_v2
                    UNION ALL

                  SELECT doddy, dodmt, dodyr, fic10und,  census2011_person_id
                    FROM project_name.deaths_2020_final_v2_std_v2
                    UNION ALL

                  SELECT doddy, dodmt, dodyr, fic10und, census2011_person_id
                    FROM project_name.deaths_2019_v2_std_v2 
                    UNION ALL

                  SELECT doddy, dodmt, dodyr, fic10und, census2011_person_id 
                    FROM project_name.deaths_2018_std_v2

                    UNION ALL
                  SELECT doddy, dodmt, dodyr, fic10und, census2011_person_id 
                    FROM project_name.deaths_2017_std_v2

                    UNION ALL
                  SELECT doddy, dodmt, dodyr, fic10und, census2011_person_id 
                    FROM project_name.deaths_2016_v5_std_v2

                    UNION ALL
                  SELECT doddy, dodmt, dodyr, fic10und, census2011_person_id 
                    FROM project_name.deaths_2015_v2_std_v2

                    UNION ALL
                  SELECT doddy, dodmt, dodyr, fic10und, census2011_person_id 
                    FROM project_name.deaths_2014_std_v2

                    UNION ALL
                  SELECT doddy, dodmt, dodyr, fic10und, census2011_person_id 
                    FROM project_name.deaths_2013_std_v2

                    UNION ALL
                  SELECT doddy, dodmt, dodyr, fic10und, census2011_person_id 
                    FROM project_name.deaths_2012_std_v2

                    UNION ALL
                  SELECT doddy, dodmt, dodyr, fic10und, census2011_person_id 
                    FROM project_name.deaths_2011_std_v2")

census <- dplyr::rename_with(census, ~ paste0(tolower(.), "_census"))
deaths <- dplyr::rename_with(deaths, ~ paste0(tolower(.), "_deaths"))

census_deaths <- sparklyr::left_join(
  census,
  deaths,
  by = c("il4_person_id_census" = "census2011_person_id_deaths"))

rm(deaths)

#--------------------------
## Data cleaning
#--------------------------

df_hes_phda <- census_deaths

sample_flow_file_name <- "sample_flow_"

df_hes_phda %<>%
  sparklyr::select(
    il4_person_id_census, # Person ID
    hrppuk11_census,      # Household Reference Person
    household_id_census,  # Household ID
    
    cen_pr_flag_census,   # A flag to indicate if the PR linked to the Census
    uresindpuk11_census,  # Usual resident (Indicator)
    country_code_census,  # Country (to filter on England)
    dob_quarter_census,   # Date of birth quarter
    
   # From deaths    
    doddy_deaths,         # Date of death day
    dodmt_deaths,         # Date of death month
    dodyr_deaths,         # Date of death year
    fic10und_deaths,      # Final underlying cause of death
    
   # Individual characteristics
    sex_census,           # Sex
    ethpuk11_census,      # Ethnic group
    mainlangprf11_census, # Proficiency in English
    carer_census,         # Provision of unpaid care
    relpuk11_census,      # Religion
    disability_census,    # Long-term health problem or disability
    health_census,        # General health
    hlqpuk11_census,      # Highest level of qualification
    
   # Household characteristics
    region_code_census,   # Region
    tenhuk11_census,      # Tenure of household
    famtype_1_census,     # Family Type
    nsshuk11_census,      # NS-SEC of Household Reference Person
  )

sample_flow <- tibble(stage = "total sample",
                      count = sparklyr::sdf_nrow(df_hes_phda))

# PR linked to census, and usual resident
df_hes_phda %<>%
  sparklyr::filter(cen_pr_flag_census == 1 & uresindpuk11_census == 1)

sample_flow <- sample_flow %>%
  dplyr::bind_rows(tibble(stage = "sample which were Census respondents",
                          count = sparklyr::sdf_nrow(df_hes_phda)))

# Filter to England only
df_hes_phda %<>%
  sparklyr::filter(country_code_census == "E92000001")

sample_flow <- sample_flow %>%
  dplyr::bind_rows(tibble(stage = "sample England only respondents",
                          count = sparklyr::sdf_nrow(df_hes_phda)))

#--------------------------
## Create birth and death dates
#--------------------------

# Remove nonsense dods and filter people who are alive, or dod > day after census
df_hes_phda %<>%
  sparklyr::mutate(
    dod_deaths = paste0(as.integer(dodyr_deaths), "-",                        
                        as.integer(dodmt_deaths), "-",
                        as.integer(doddy_deaths)),  
    dod_deaths = to_date(dod_deaths, "yyyy-MM-dd")) %>%
  sparklyr::filter(
    is.na(dod_deaths) | (!is.na(dod_deaths) & dod_deaths >= start_date))

sample_flow <- sample_flow %>%
  dplyr::bind_rows(tibble(stage = "sample which were alive on Census day and dod < end date",
                          count = sparklyr::sdf_nrow(df_hes_phda)))

# Calcualte age at time of census and floor (instead of round to stop rounding up)
df_hes_phda %<>%
  sparklyr::mutate(
    dob_census = dplyr::case_when(
      substr(dob_quarter_census, 6, 7) == "01" ~ paste0(substr(dob_quarter_census, 1, 4), "-02-15"),
      substr(dob_quarter_census, 6, 7) == "02" ~ paste0(substr(dob_quarter_census, 1, 4), "-05-15"),
      substr(dob_quarter_census, 6, 7) == "03" ~ paste0(substr(dob_quarter_census, 1, 4), "-08-15"),
      substr(dob_quarter_census, 6, 7) == "04" ~ paste0(substr(dob_quarter_census, 1, 4), "-11-15")),
    dob_census = to_date(dob_census, "yyyy-MM-dd"),
    age_on_cd = datediff(start_date, dob_census) / 365.25,
    age_on_cd = floor(age_on_cd))

# Calcualte age at time of death and floor (instead of round to stop rounding up)
df_hes_phda %<>%
  sparklyr::mutate(
    age_at_tod = ifelse(!is.na(dod_deaths) & dod_deaths <= end_date,
                        datediff(dod_deaths, dob_census) / 365.25,
                        NA),
    age_at_tod = floor(age_at_tod))

# Flag for deaths - death is when dod is not NA & dod < study end date
df_hes_phda %<>%
  sparklyr::mutate(
    death = ifelse(!is.na(dod_deaths) & dod_deaths <= end_date, 1, 0),
    death = ifelse(death == 1 & age_at_tod < 18, 1, 0),
    underlying_code = str_sub(fic10und_deaths, 1, 3))

#--------------------------
## Read in look up for causes of death and link to PHDA
#--------------------------

death_lookup <- readr::read_csv("project_name/suicide_lookup.csv")

# Suicide defintion exludes undermined intent aged < 15
df_hes_phda %<>%
  sparklyr::left_join(death_lookup, by = c("underlying_code" = "Code"), copy = TRUE) %>% 
  rename(Type_of_death = "Type of death") %>%
  sparklyr::mutate(
    cause_death = dplyr::case_when(
      death == 1 & age_at_tod >= 10 & Type_of_death == "Intentional self-harm" ~ "Death_Suicide",
      death == 1 & age_at_tod <  10 & Type_of_death == "Intentional self-harm" ~ "Death_Other",
      death == 1 & age_at_tod >= 15 & Type_of_death == "Injury or poisoning of undetermined intent" ~ "Death_Suicide",
      death == 1 & age_at_tod <  15 & Type_of_death == "Injury or poisoning of undetermined intent" ~ "Death_Other",
      death == 1 & is.na(Type_of_death) ~ "Death_Other",
      death == 0 ~ "Alive",
      TRUE ~ "Check"),
    outcome_suicide = ifelse(cause_death == "Death_Suicide", 1, 0),
    outcome_death = ifelse(cause_death == "Alive", 0, 1))

check_deaths_s1 <- df_hes_phda %>% 
  dplyr::group_by(cause_death) %>% 
  dplyr::count() %>% 
  sparklyr::collect() %>% 
  print()

sample_flow <- sample_flow %>%
  dplyr::bind_rows(tibble(stage = "sample which contains more vars",
                          count = sparklyr::sdf_nrow(df_hes_phda)))

rm(death_lookup)

#--------------------------
## Create 10th, 18th and Census year birthdays
#--------------------------

df_hes_phda %<>%
  sparklyr::mutate(
    year_of_birth = substr(dob_census, 1, 4),
    date_of_10bday = paste0(as.integer(year_of_birth + 10), substr(dob_census, 5, 10)),
    date_of_18bday = paste0(as.integer(year_of_birth + 18), substr(dob_census, 5, 10)))

df_hes_phda %<>%
  sparklyr::mutate(death_18b_flag = ifelse(dod_deaths < date_of_18bday, 1, 0))

# Birthday in 2010/2011 prior to Census day
df_hes_phda %<>%
  sparklyr::mutate(
    census_year_bday = dplyr::case_when(
      substr(dob_quarter_census, 6, 7) == "01" ~ paste0("2011-", substr(dob_census, 6, 10)),
      substr(dob_quarter_census, 6, 7) == "02" ~ paste0("2010-", substr(dob_census, 6, 10)),
      substr(dob_quarter_census, 6, 7) == "03" ~ paste0("2010-", substr(dob_census, 6, 10)),
      substr(dob_quarter_census, 6, 7) == "04" ~ paste0("2010-", substr(dob_census, 6, 10))),
    census_year_bday = to_date(census_year_bday, "yyyy-MM-dd"))

df_hes_phda %<>%
  sparklyr::filter(age_on_cd > 3 & age_on_cd < 18)

sample_flow <- sample_flow %>%
  dplyr::bind_rows(tibble(stage = "sample which has filtered on age between 3 and 17",
                          count = sparklyr::sdf_nrow(df_hes_phda)))

#--------------------------
## Calculate time at risk 
#--------------------------

# Cohort end - end of follow up period based on 18th birthday or end date whichever comes first
df_hes_phda %<>% 
   sparklyr::mutate(cohort_end = ifelse(date_of_18bday < end_date, date_of_18bday, end_date))

# Calculate time at risk
df_hes_phda %<>%
  sparklyr::mutate(
    time_at_risk = dplyr::case_when(
    death == 0 & age_on_cd %in% 0:9 ~ datediff(cohort_end, date_of_10bday),
    death == 0 & age_on_cd %in% 10:17 ~ datediff(cohort_end, census_year_bday),
    death == 0 & age_on_cd >= 18 ~ NA,
    death == 1 & death_18b_flag == 0 & age_on_cd %in% 0:9 ~ datediff(cohort_end, date_of_10bday),
    death == 1 & death_18b_flag == 1 & age_on_cd %in% 0:9 ~ datediff(dod_deaths, date_of_10bday),  
    death == 1 & death_18b_flag == 0 & age_on_cd %in% 10:17 ~ datediff(cohort_end, census_year_bday),
    death == 1 & death_18b_flag == 1 & age_on_cd %in% 10:17 ~ datediff(dod_deaths, census_year_bday),    
    death == 1 & age_on_cd >= 18 ~ NA)) %>%
  sparklyr::filter(time_at_risk > 0)

# To update time at risk in the first age interval (when dataset is split) for those who age in after the survSplit
df_hes_phda %<>% 
   mutate(time_at_risk_recalculate = datediff(start_date, census_year_bday))

# Total time at risk for person level dataset (to calculate demographic suicide rates)
df_hes_phda %<>%
  sparklyr::mutate(
    total_time_at_risk = dplyr::case_when(date_of_10bday < start_date ~ time_at_risk - time_at_risk_recalculate,
                                          TRUE ~ time_at_risk))

sample_flow <- sample_flow %>%
  dplyr::bind_rows(tibble(stage = "sample which has filtered on time_at_risk >= 0",
                          count = sparklyr::sdf_nrow(df_hes_phda)))

#--------------------------
## Create cohort id's and create HRP indicator
#--------------------------
    
cohort_ids <- df_hes_phda %>%
  sparklyr::select(il4_person_id_census, household_id_census)

# Filter to the houeshold reference person 
census_hrp <- census %>%
  sparklyr::select(hrppuk11_census, ethpuk11_census, relpuk11_census,
                   hlqpuk11_census, household_id_census, age_census) %>%
  dplyr::rename(hrp_age = age_census) %>%
  sparklyr::filter(hrppuk11_census == 1) %>%
  sparklyr::mutate(
    ethnicity_hrp = dplyr::case_when(
      ethpuk11_census == "01" ~ "English/Welsh/Scottish/Northern Irish/British", #    White
      ethpuk11_census == "02" ~ "Irish",                        #    White
      ethpuk11_census == "03" ~ "Gypsy or Irish Traveller",     #    White 
      ethpuk11_census == "04" ~ "Other White",                  #    White
      ethpuk11_census == "05" ~ "White and Black Caribbean",    #    Mixed or multiple ethnic groups
      ethpuk11_census == "06" ~ "White and Black African",      #    Mixed or multiple ethnic groups
      ethpuk11_census == "07" ~ "White and Asian",              #    Mixed or multiple ethnic groups
      ethpuk11_census == "08" ~ "Other Mixed",                  #    Mixed or multiple ethnic groups
      ethpuk11_census == "09" ~ "Indian",                       #    Asian or Asian British
      ethpuk11_census == "10" ~ "Pakistani",                    #    Asian or Asian British
      ethpuk11_census == "11" ~ "Bangladeshi",                  #    Asian or Asian British
      ethpuk11_census == "12" ~ "Chinese",                      #    Asian or Asian British
      ethpuk11_census == "13" ~ "Other Asian",                  #    Asian or Asian British
      ethpuk11_census == "14" ~ "African",                      #    Black, African, Caribbean or Black British
      ethpuk11_census == "15" ~ "Caribbean",                    #    Black, African, Caribbean or Black British
      ethpuk11_census == "16" ~ "Other Black",                  #    Black, African, Caribbean or Black British
      ethpuk11_census == "17" ~ "Arab",                         #    Other ethnic group
      ethpuk11_census == "18" ~ "Other Ethnic Group",           #    Other ethnic group
      TRUE ~ "Unknown"),
    ethnicity_hrp_short = dplyr::case_when(
      ethpuk11_census %in% c("01", "02", "03", "04") ~ "White",
      ethpuk11_census %in% c("05", "06", "07", "08") ~ "Mixed/multiple ethnic groups",
      ethpuk11_census %in% c("09", "10", "11", "12", "13") ~ "Asian/Asian British",
      ethpuk11_census %in% c("14", "15", "16") ~ "Black/African/Caribbean/Black British",
      ethpuk11_census %in% c("17", "18") ~ "Other ethnic group",
      TRUE ~ "Unknown"),
    religion_hrp = dplyr::case_when(
      relpuk11_census == 1 ~ "No religion",
      relpuk11_census == 2 ~ "Christian",
      relpuk11_census == 3 ~ "Buddhist",
      relpuk11_census == 4 ~ "Hindu",
      relpuk11_census == 5 ~ "Jewish",
      relpuk11_census == 6 ~ "Muslim",
      relpuk11_census == 7 ~ "Sikh",
      relpuk11_census == 8 ~ "Other",
      relpuk11_census == 9 ~ "Not stated",
      relpuk11_census == "X" ~ "Unknown",
      TRUE ~ "Unknown"),
    religion_hrp_short = dplyr::case_when(
      relpuk11_census == 1 ~  "No religion",
      relpuk11_census == 2 ~ "Christian",
      relpuk11_census %in% c(3:5, 7:8) ~ "Other religion",
      relpuk11_census == 6 ~ "Muslim",
      relpuk11_census == 9 ~ "Not stated",
      is.na(relpuk11_census) ~ "Unknown",
      TRUE ~ "Uknown"),
    qualifications_hrp = dplyr::case_when(
      hlqpuk11_census == "10" ~ "No academic or professional qualifications",
      hlqpuk11_census == "11" ~ "Level 1",
      hlqpuk11_census == "12" ~ "Level 2",
      hlqpuk11_census == "13" ~ "Apprenticeship",
      hlqpuk11_census == "14" ~ "Level 3",
      hlqpuk11_census == "15" ~ "Level 4",
      hlqpuk11_census == "16" ~ "Other",
      hlqpuk11_census == "XX" ~ "Unknown",
      TRUE ~ "Unknown"),
    qualifications_hrp_short = dplyr::case_when(
      hlqpuk11_census == "15" ~ "Has degree or above",
      hlqpuk11_census %in% c("11", "12", "13", "14", "16") ~ "Has below degree",
      hlqpuk11_census == "10" ~ "No qualifications",
      TRUE ~ "Unknown"))

# Link to cohort id's
df_hes_phda_hrp <- cohort_ids %>% 
  sparklyr::left_join(census_hrp, by = "household_id_census") %>%
  select(!(c("household_id_census", 
             "hrppuk11_census", 
             "hlqpuk11_census", 
             "relpuk11_census", 
             "ethpuk11_census")))
  
df_hes_phda_hrp %<>%
  sparklyr::mutate(
    qualifications_hrp = ifelse(is.na(qualifications_hrp), "Unknown", qualifications_hrp),
    qualifications_hrp_short = ifelse(is.na(qualifications_hrp_short), "Unknown", qualifications_hrp_short),  
    religion_hrp = ifelse(is.na(religion_hrp), "Unknown", religion_hrp),
    religion_hrp_short = ifelse(is.na(religion_hrp_short), "Unknown", religion_hrp_short))

df_hes_phda <- df_hes_phda %>%
  sparklyr::left_join(df_hes_phda_hrp, by = "il4_person_id_census")

sample_flow <- sample_flow %>%
  dplyr::bind_rows(tibble(stage = "sample with HRP added",
                          count = sparklyr::sdf_nrow(df_hes_phda)))

# Write to csv
write.csv(sample_flow, paste0(results_path, "sample_flow.csv")

rm(census, df_hes_phda_hrp, census_hrp, cohort_ids)

#--------------------------
## Census 2011 cleaning   
#--------------------------

df_hes_phda %<>% 
  sparklyr::mutate(
    sex = dplyr::case_when(
      sex_census == 1 ~ "Male",
      sex_census == 2 ~ "Female",
      TRUE ~ "Unknown"),
    ethnicity = dplyr::case_when(
      ethpuk11_census == "01" ~ "English/Welsh/Scottish/Northern Irish/British", #    White
      ethpuk11_census == "02" ~ "Irish",                        #    White
      ethpuk11_census == "03" ~ "Gypsy or Irish Traveller",     #    White 
      ethpuk11_census == "04" ~ "Other White",                  #    White
      ethpuk11_census == "05" ~ "White and Black Caribbean",    #    Mixed or multiple ethnic groups
      ethpuk11_census == "06" ~ "White and Black African",      #    Mixed or multiple ethnic groups
      ethpuk11_census == "07" ~ "White and Asian",              #    Mixed or multiple ethnic groups
      ethpuk11_census == "08" ~ "Other Mixed",                  #    Mixed or multiple ethnic groups
      ethpuk11_census == "09" ~ "Indian",                       #    Asian or Asian British
      ethpuk11_census == "10" ~ "Pakistani",                    #    Asian or Asian British
      ethpuk11_census == "11" ~ "Bangladeshi",                  #    Asian or Asian British
      ethpuk11_census == "12" ~ "Chinese",                      #    Asian or Asian British
      ethpuk11_census == "13" ~ "Other Asian",                  #    Asian or Asian British
      ethpuk11_census == "14" ~ "African",                      #    Black, African, Caribbean or Black British
      ethpuk11_census == "15" ~ "Caribbean",                    #    Black, African, Caribbean or Black British
      ethpuk11_census == "16" ~ "Other Black",                  #    Black, African, Caribbean or Black British
      ethpuk11_census == "17" ~ "Arab",                         #    Other ethnic group
      ethpuk11_census == "18" ~ "Other Ethnic Group",           #    Other ethnic group
      TRUE ~ "Unknown"),
    ethnicity_short = dplyr::case_when(
      ethpuk11_census %in% c("01", "02", "03", "04") ~ "White",
      ethpuk11_census %in% c("05", "06", "07", "08") ~ "Mixed/multiple ethnic groups",
      ethpuk11_census %in% c("09", "10", "11", "12", "13") ~ "Asian/Asian British",
      ethpuk11_census %in% c("14", "15", "16") ~ "Black/African/Caribbean/Black British",
      ethpuk11_census %in% c("17", "18") ~ "Other ethnic group",
      TRUE ~ "Unknown"),    
    main_language = dplyr::case_when(
      mainlangprf11_census == 1 ~ "Main language is English",
      mainlangprf11_census %in% c(2:5) ~ "Main language is not English",
      TRUE ~ "Unknown"),
    carer = dplyr::case_when(
      carer_census == 1 ~ "Not a carer",
      carer_census == 2 ~ "Yes, 1 to 19 hours a week",
      carer_census == 3 ~ "Yes, 20 to 49 hours a week",
      carer_census == 4 ~ "Yes, 50 or more hours a week",
      TRUE ~ "Missing or not stated"),
    carer_binary = dplyr::case_when(
      carer_census == 1 ~ "Non-carer",
      carer_census >= 2 & carer_census <= 4 ~ "Carer",
      TRUE ~ "Missing or not stated"),
    disability = dplyr::case_when(
      disability_census %in% c("1", "2") ~ "Day to day activities limited a lot/a little",
      disability_census == "3" ~ "Day to day activities not limited",
      TRUE ~ "Unknown"),

    # Household characteristics
    
    region = dplyr::case_when(
      region_code_census == "E12000001" ~ "North East",
      region_code_census == "E12000002" ~ "North West",
      region_code_census == "E12000003" ~ "Yorkshire and The Humber",
      region_code_census == "E12000004" ~ "East Midlands",
      region_code_census == "E12000005" ~ "West Midlands",
      region_code_census == "E12000006" ~ "East of England",
      region_code_census == "E12000007" ~ "London",
      region_code_census == "E12000008" ~ "South East",
      region_code_census == "E12000009" ~ "South West",
      region_code_census == "W92000004" ~ "Wales",
      TRUE ~ "Unknown"),
    tenure = dplyr::case_when(
      tenhuk11_census == 0 ~ "Owned: Owned outright",
      tenhuk11_census == 1 ~ "Owned: Owned with a mortgage or loan",
      tenhuk11_census == 2 ~ "Shared ownership (part owned and part rented)",
      tenhuk11_census == 3 ~ "Social rented: Rented from council (Local Authority)",
      tenhuk11_census == 4 ~ "Social rented: Other social rented",
      tenhuk11_census == 5 ~ "Private rented: Private landlord or letting agency",
      tenhuk11_census == 6 ~ "Private rented: Employer of a household member",
      tenhuk11_census == 7 ~ "Private rented: Relative or friend of household member",
      tenhuk11_census == 8 ~ "Private rented: Other",
      tenhuk11_census == 9 ~ "Living rent free",
      TRUE ~ "Unknown"),
    tenure_short = dplyr::case_when(
      tenhuk11_census %in% c("0", "1", "2") ~ "Owned",
      tenhuk11_census %in% c("3", "4") ~ "Social rented",
      tenhuk11_census %in% c("5", "6", "7", "8") ~ "Private rented",
      tenhuk11_census == 9 ~ "Living rent free",
      TRUE ~ "Unknown"),
     tenure_modelling = dplyr::case_when(
      tenure_short %in% c("Unknown", "Living rent free") ~ "Unknown",
      TRUE ~ tenure_short),
    fam_type = dplyr::case_when(
      famtype_1_census == 0 ~ "Ungrouped individual",
      famtype_1_census == 1 ~ "Married couple family",
      famtype_1_census == 2 ~ "Same-sex civil partner family (male)",
      famtype_1_census == 3 ~ "Same-sex civil partner family (female)",
      famtype_1_census == 4 ~ "Opposite cohabiting couple family",
      famtype_1_census == 5 ~ "Same-sex cohabiting couple family (male)",
      famtype_1_census == 6 ~ "Same-sex cohabiting couple family (female)",
      famtype_1_census == 7 ~ "A lone parent family (male)",
      famtype_1_census == 8 ~ "A lone parent family (female)",
      famtype_1_census == 9 ~ "Other related family",
      TRUE ~ "Unknown"),
    fam_type_short = dplyr::case_when(
      famtype_1_census == 0 ~ "Ungrouped individual",
      famtype_1_census %in% c(1:6) ~ "Couple family",
      famtype_1_census %in% c(7:8) ~ "Lone parent family",
      famtype_1_census == 9 ~ "Other related family",
      TRUE ~ "Unknown"),
    nssec = case_when(
      nsshuk11_census >= 1 & nsshuk11_census < 3 ~ "Class 1.1",
      nsshuk11_census >= 3 & nsshuk11_census < 4 ~ "Class 1.2",
      nsshuk11_census >= 4 & nsshuk11_census < 7 ~ "Class 2",
      nsshuk11_census >= 7 & nsshuk11_census < 8 ~ "Class 3",
      nsshuk11_census >= 8 & nsshuk11_census < 10 ~ "Class 4",
      nsshuk11_census >= 10 & nsshuk11_census < 12 ~ "Class 5",
      nsshuk11_census >= 12 & nsshuk11_census < 13 ~ "Class 6",
      nsshuk11_census >= 13 & nsshuk11_census < 14 ~ "Class 7",
      nsshuk11_census >= 14 & nsshuk11_census < 15 ~ "Class 8",
      nsshuk11_census == 15 ~ "Full-time students",
      nsshuk11_census > 15 ~ "Not classifiable",
      TRUE ~ "Unknown"),    
    nssec_5 = dplyr::case_when(
      nssec %in% c("Class 1.1", " Class 1.2", "Class 2") ~  "Higher managerial, administrative and professional occupations",
      nssec == "Class 3" ~ "Intermediate occupations",
      nssec == "Class 4" ~ "Small employers and own account workers",
      nssec == "Class 5" ~ "Lower supervisory and technical occupations",      
      nssec %in% c("Class 6", "Class 7") ~ "Semi-routine and routine occupations",
      nssec == "Class 8" ~ "Never worked and long-term unemployed",
      TRUE ~ "Unknown"),     
    nssec_3 = dplyr::case_when(
      nssec_5 %in% c("Intermediate occupations", "Small employers and own account workers") ~ "Intermediate occupations",
      nssec_5 %in% c("Lower supervisory and technical occupations", "Semi-routine and routine occupations") ~ "Routine and manual occupations",
      TRUE ~ nssec_5)
  )

# Drop vars not looking into
df_hes_phda %<>%
  sparklyr::select(
    !(c(doddy_deaths, dodmt_deaths, dodyr_deaths, fic10und_deaths, underlying_code,
        `ICD-10 definition`, Type_of_death, dob_quarter_census, country_code_census,
        sex_census, ethpuk11_census, mainlangprf11_census, carer_census, relpuk11_census, 
        disability_census, tenhuk11_census, famtype_1_census, nsshuk11_census)))

#--------------------------
## Save out person level data to HUE
#--------------------------

# Save out table
table_name_1 = 'nihr_aim_1b_person_level'
tbl_change_db(sc, "project_name")

# Delete table if already exists
DBI::dbExecute(sc, paste0('DROP TABLE IF EXISTS ', table_name_1))

sdf_register(df_hes_phda, 'df_hes_phda')

DBI::dbExecute(
  sc,
  paste('CREATE TABLE', table_name_1, 'AS SELECT * FROM df_hes_phda'))

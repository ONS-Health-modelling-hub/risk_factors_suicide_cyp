#--------------------------
# Modelling configuration
#--------------------------

project_directory <- "project_name"

# spark session is not required 

library(sparklyr)
library(tidyverse)
library(magrittr)
library(stringi)
library(survival)
library(splines)
library(emmeans)
library(janitor)
library(scales)

options(max.print = 9999)

results_path <- "project_name/output/"

source(paste0(project_directory, "/functions.R"))

#--------------------------
# Read in data and set reference groups
#--------------------------

df_interval <- readr::read_rds(paste0(results_path, "df_split_20.RDS"))

# Change columns in modelling to factor

columns_to_factor <- c("sex", 
                       "ethnicity_short", 
                       "region",
                       "disability", 
                       "carer_binary",
                       "main_language", 
                       "tenure_modelling",
                       "fam_type_short", 
                       "qualifications_hrp_short",
                       "religion_hrp_short", 
                       "nssec_3")

df_interval <- df_interval %>%
  dplyr::mutate(across(all_of(columns_to_factor), as.factor))

# set reference groups

df_interval %<>%
  dplyr::mutate(
    sex = relevel(sex, ref = "Male"),
    ethnicity_short = relevel(ethnicity_short, ref = "White"),
    main_language = relevel(main_language,  ref = "Main language is English"),
    disability = relevel(disability, ref = "Day to day activities not limited"),
    carer_binary = relevel(carer_binary, ref = "Non-carer"),
    region = relevel(region, ref = "London"),
    nssec_3 = relevel(nssec_3, ref = "Higher occupations"),
    tenure_modelling = relevel(tenure_modelling, ref = "Owned"),
    fam_type_short = relevel(fam_type_short, ref = "Couple family"),
    qualifications_hrp_short = relevel(qualifications_hrp_short, ref = "No qualifications"), 
    religion_hrp_short = relevel(religion_hrp_short, ref = "No religion"))

# create time at risk weight as data is sampled

df_interval <- df_interval %>%
  dplyr::mutate(time_weight = time_at_risk_new * weight)

###--------------------------
### Sensitivty analysis - filter data to subset
###--------------------------
#
#df_int <- df_interval %>%
#  # filter(calender_time %in% 2011:2016) %>%
#  # filter(calender_time %in% 2017:2022) %>%
#  # filter(age_in_interval %in% 14:17) %>%
#  filter(as.integer(hrp_age) %in% 25:69) %>%
#  select("sex", "age_in_interval", "outcome_suicide_new", "death", "ethnicity_short",
#         "main_language", "carer_binary", "disability", "qualifications_hrp",
#         "qualifications_hrp_short", "nssec", "nssec_3", "nssec_5", "fam_type_short", "tenure_modelling",
#         "region", "disability", "religion_hrp_short", "religion_hrp", "time_weight")

##--------------------------
## Creating other deaths flag for models   
##--------------------------      
      
df_interval %<>%
  mutate(
    outcome_other = case_when(
      outcome_death_interval == "Death_Other" & death == 1 ~ 1,
      TRUE ~ 0))
    
# create smaller dataset to model 
df_int <- df_interval %>%
  select("sex", 
         "age_in_interval", 
         "outcome_suicide_new",
         "outcome_other",
         "outcome_death_interval",
         "death", 
         "ethnicity_short",
         "main_language", 
         "carer_binary", 
         "disability",
         "qualifications_hrp_short", 
         "nssec_3",
         "fam_type_short", 
         "tenure_modelling",
         "region", 
         "religion_hrp_short", 
         "time_weight")

df_int %<>% rename("outcome_suicide" = "outcome_suicide_new")
   
##--------------------------
## Set outcome + df
##--------------------------

# Set death type for modelling either suicide or non-suicide deaths

death_type <- "suicide" 
#death_type <- "other" 

#if running sensitivty analysis on subset of the data add here e.g. "suicide_2011_2016"
outcome <- "suicide"
#outcome <- "other"

outcome_plot <- "Suicide"
#outcome_plot <- "Non-suicide"

##--------------------------
## Sense checking models
##--------------------------

# Include age as factor for now to see if estimated risk is sensible by ages
simple_model_formula <- paste0("outcome_", death_type, " ~ sex + as.factor(age_in_interval) + offset(log(time_weight))")

simple_model <- model_all(df_int, formula = simple_model_formula)

print(simple_model)

#--------------------------
# Testing spline and determining number of knots to inlcude in models
#--------------------------

#Checking the total number of df (knots) for age spline to test in the model 
#specify a sensible maximum number of knots (over 12 would likely be overfitting the spline)

max_df = 4#set to 6 if you sample 20% as code took a long time to run max_df = 12 
           #sampled at 5% you can set to 12 and it doesn't take too long to run (same result is returned)

#creates blank formula list

formula_list <- list()

#Populates formula list going through each iteration of the model with df set to 1, 2, 3, etc. up to max_df

for (i in 1:max_df){  
  formula <- paste0('outcome_', death_type, ' ~ sex + ns(age_in_interval, df=',               
                    i,       
                    ', Boundary.knots=quantile(age_in_interval, c(.01, .99)))', 
                    " + offset(log(time_weight))")  
  
  formula_list <- c(formula_list, formula)
}

#creates blank bic list

bic_list <- list()  

#creates blank list for model outputs

age_sex_glm <- list()

#Runs each iteration of model specified in formula_list

for (i in 1:max_df){
  
  age_sex_glm$model[[i]] <- model_all(df_int, formula = formula_list[[i]]) %>%
                                    mutate(model_number = paste0(i))


  bic <- bind_rows(age_sex_glm) %>% 
         select(model_number, bic)
  
  bic_list <- bic %>% 
          distinct()
  
}                   
                             
# idtentifies model with the lowest bic to determine number of df (knots) to use in model

min_df <- which.min(as.numeric(bic_list$bic))

write.csv(bic_list, paste0(results_path, "bic_list_df.csv"))

# Use the model formula with the lowest BIC to run the main models 
# Model with the lowest BIC has 2 df so df will be set to 2 in models

##--------------------------
## Running basic spline model 
##--------------------------

#Adding age as a spline

splinemodel <- paste0("outcome_", death_type, " ~ ns(age_in_interval, df = ", 
                      min_df,
                      ", Boundary.knots = quantile(age_in_interval, c(.01, .99))) + offset(log(time_weight))")

# plot spline model using raw model output rather than datafrom output from function
model1 <- glm(formula = splinemodel,
              family = poisson(link = "log"),
              data = df_int) 

# Changed to base R from broom
summary(model1)
spline_model_output <- as.data.frame(summary(model1)$coefficients)
spline_model_output$term <- rownames(spline_model_output)
spline_model_output$irr <- exp(spline_model_output[,1])
spline_model_output$lcl <- exp(spline_model_output[,1] - 1.96 * spline_model_output[,2])
spline_model_output$ucl <- exp(spline_model_output[,1] + 1.96 * spline_model_output[,2])
spline_model_output$bic <- BIC(model1)
spline_model_output$model <- splinemodel
spline_model_output$pvalue <- spline_model_output[,4]

spline_model_output <- spline_model_output %>% 
  select(term, irr, lcl, ucl, , bic, model) 

print(spline_model_output)

#--------------------------
# Plotting spline model 
#--------------------------
      
# Plot the spline

age_list <- data.frame(age_in_interval = rep(seq(10, 17, 1)))

age_list$time_weight <- mean(df_int$time_weight)

pred <- predict(model1, age_list, type = 'link', se.fit = TRUE)

pred_ci <- data.frame(fit = pred$fit, se = pred$se.fit) %>%
  mutate(ci1 = fit + (1.96 * se),
         ci2 = fit - (1.96 * se)) %>%
  select(fit, ci1, ci2)

joinplot <- cbind(age_list, pred_ci)

# Ref for age plot
reference <- joinplot %>%
  filter(age_in_interval == 14)

rb <- reference$fit

joinplot <- joinplot %>%
  mutate(IRR = exp(fit - rb),
         ci1 = exp(ci1- rb),
         ci2 = exp(ci2 -rb))

print(joinplot)

age_irr <- joinplot %>%
  ggplot(aes(x = age_in_interval, y = IRR)) +
  geom_line(aes()) +
  geom_ribbon(aes(ymin = ci2, ymax = ci1),
              alpha = 0.30,
              show.legend = FALSE) +
  scale_x_continuous(n.breaks = 7, labels = 10:17) +
  xlab("Age in years") +
  theme_grey(base_size = 12)
      
plot(age_irr)

ggsave(paste0(results_path, "spline_plot.jpg"))

#save out qa of models 
write.csv(joinplot, paste0(results_path, "spline_model_summary.csv"))

#--------------------------
## Basic models formulas 
#--------------------------   
            
#Create formulas for both suicide and other death models

basic_model <- paste0("outcome_", death_type, " ~ sex + ns(age_in_interval, df = ", 
                              min_df,
                              ", Boundary.knots = quantile(age_in_interval, c(.01, .99))) + offset(log(time_weight))")                                                                 

exposure <- c("sex", #1
              "ethnicity_short", #2
              "main_language", #3
              "disability", #4
              "region", #5
              "nssec_3", #6
              "qualifications_hrp_short", #7
              "religion_hrp_short" #8
              )


# creates list of formula for each basic model

for (i in 1:length(exposure)){
  
  model_formulas <- paste0(basic_model, " + ", exposure, " + offset(log(time_weight))")
  
}
  
  
#--------------------------
## Running suicide models  
#--------------------------       
      
# blank list to be used for model outputs  

basic_models <- list()

# run deaths by suicide models through looped exposure list

for (i in 1:length(model_formulas)){
  for (i in 1:length(exposure)){
  
  basic_models[[i]] <- model_all(df_int, formula = model_formulas[[i]]) %>%
  mutate(exposure = exposure[[i]],
        model_type = "Basic model")
  }
  
}

model_diagnostics_basic <- bind_rows(basic_models) %>% 
  select(model_type, bic, nobs, exposure) %>%
  distinct()

basic_models <- bind_rows(basic_models) %>% 
  select(!(c(bic, nobs)))

#--------------------------
## Fully adjusted models formulas
#--------------------------
      
# Adjusted suicide models

main_adjusted_model <- "ethnicity_short + main_language + carer_binary +  disability + qualifications_hrp_short + nssec_3 + fam_type_short + tenure_modelling + region"
disability_adjusted_model <- "disability + ethnicity_short + main_language + qualifications_hrp_short + nssec_3 + fam_type_short + tenure_modelling + region"

# Fully adjusted suicide

adjusted_model_all <- paste0(basic_model, " + ", main_adjusted_model) 
adjusted_model_disability <- paste0(basic_model, " + ", disability_adjusted_model)
adjusted_model_religion <- paste0(basic_model, " + ", "religion_hrp_short + ", main_adjusted_model)

# create suicide models list

adjusted_model_formulas <- c(adjusted_model_all, 
                                   adjusted_model_disability, 
                                   adjusted_model_religion)   

exposure <- c("all",
              "disability", 
              "religion_hrp_short")  
                                             
#--------------------------
## Running fully adjusted suicides models
#--------------------------   
                                             
# blank list to be used for model outputs  

adjusted_models <- list()

for (i in 1:length(adjusted_model_formulas)){
  for (i in 1:length(exposure)){

  adjusted_models[[i]] <- model_all(df_int, formula = adjusted_model_formulas[[i]])%>%
  mutate(exposure = exposure[[i]],
            model_type = "Adjusted model")
  
   }
  
}

model_diagnostics_full <- bind_rows(adjusted_models) %>%
    select(model_type, bic, nobs, exposure) %>%
    distinct() %>%
    bind_rows(model_diagnostics_basic) %>%
    arrange(model_type)

adjusted_models <-  bind_rows(adjusted_models) %>%
  select(!(c(bic, nobs)))

             
# write.csv(model_diagnostics_full, paste0(results_path, "/models_", outcome,"_diagnostics.csv"))

#--------------------------
## Plotting models 
#-------------------------- 

sex_irrs <- bind_rows(basic_models, adjusted_models) %>%
  dplyr::filter(exposure == "sex" | exposure == "all") %>%
  dplyr::filter(grepl("sex", term)) %>%
  dplyr::mutate(term = as.factor(substr(term, 4, 999)))

ethnicity_irrs <- bind_rows(basic_models, adjusted_models) %>%
  dplyr::filter(exposure == "ethnicity_short" | exposure == "all") %>%
  dplyr::filter(grepl("ethnicity_short", term)) %>%
  dplyr::mutate(term = as.factor(substr(term, 16, 999)))

main_language_irrs <- bind_rows(basic_models, adjusted_models) %>%
  dplyr::filter(exposure == "main_language" | exposure == "all") %>%
  dplyr::filter(grepl("main_language", term)) %>%
  dplyr::mutate(term = as.factor(substr(term, 14, 999)))

disability_irrs <- bind_rows(basic_models, adjusted_models) %>%
  dplyr::filter(exposure == "disability") %>%
  dplyr::filter(grepl("disability", term)) %>%
  dplyr::mutate(term = as.factor(substr(term, 11, 999)))

nssec_3_irrs <- bind_rows(basic_models, adjusted_models) %>%
  dplyr::filter(exposure == "nssec_3" | exposure == "all") %>%
  dplyr::filter(grepl("nssec_3", term)) %>%
  dplyr::mutate(term = as.factor(substr(term, 8, 999))) 

hrp_quals_irrs <- bind_rows(basic_models, adjusted_models) %>%
  dplyr::filter(exposure == "qualifications_hrp_short" | exposure == "all") %>%
  dplyr::filter(grepl("qualifications_hrp_short", term)) %>%
  dplyr::mutate(term = as.factor(substr(term, 25, 999))) 

hrp_religion_irrs <- bind_rows(basic_models, adjusted_models) %>%
  dplyr::filter(exposure == "religion_hrp_short" | exposure == "religion_hrp_short") %>%
  dplyr::filter(grepl("religion_hrp_short", term)) %>%
  dplyr::mutate(term = as.factor(substr(term, 19, 999))) 

region_irrs <- bind_rows(basic_models, adjusted_models) %>%
  dplyr::filter(exposure == "region" | exposure == "all") %>%
  dplyr::filter(grepl("region", term)) %>%
  dplyr::mutate(term = as.factor(substr(term, 7, 999)))

write.csv(sex_irrs, paste0(results_path, "irr_", outcome, "_sex.csv"))
write.csv(ethnicity_irrs, paste0(results_path, "irr_", outcome, "_ethnicity.csv"))
write.csv(main_language_irrs, paste0(results_path, "irr_", outcome, "_main_language.csv"))
write.csv(disability_irrs, paste0(results_path, "irr_", outcome, "_disability.csv"))
write.csv(nssec_3_irrs, paste0(results_path, "irr_", outcome, "_nssec.csv"))
write.csv(hrp_quals_irrs, paste0(results_path, "irr_", outcome, "_hrp_quals.csv"))
write.csv(hrp_religion_irrs, paste0(results_path, "irr_", outcome, "_hrp_religion.csv"))
write.csv(region_irrs, paste0(results_path, "irr_", outcome, "_region.csv"))

#--------------------------
## Create plots for suicide regression models
#--------------------------


plot_function(data_frame = sex_irrs, label = "Male", title = "sex", set_x = 0.95)
plot_function(data_frame = ethnicity_irrs, label = "White", title = "ethnic group")
plot_function(data_frame = main_language_irrs, label = "Main langauge\nis English", title = "main language")
plot_function(data_frame = disability_irrs, label = "Non-disabled", title = "long term health condition or disability", set_x = 1.1)
plot_function(data_frame = nssec_3_irrs, label = "Higher managerial,\nadministrative and\nprofessional occupations", title = "NS-SEC")
plot_function(data_frame = hrp_religion_irrs, label = "No religion", title = "religion (of HRP)")
plot_function(data_frame = region_irrs, label = "London", title = "English region")

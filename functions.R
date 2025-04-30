#--------------------------
# Crude rates by demographics
#--------------------------

#' Calculate crude rates
#'
#' @description Function to calculate counts of deaths and suicide.
#' Updated to calculate rates on the rounded data.
#'
#' @param data An object of class "data.frame". A person-level data frame
#' containing "cause_death" variable.
#' @param by_var Name of demographic variable to group by.
#'
#' @import dplyr
#' @import tidyr
#' 
#' @return Table containing numbers (rounded to nearest 5) and rates of deaths and suicide rounded to 2dp
#' values under 10 are suppressed with [c] and 0's which return NA are replaced with [c]
#' 
#' @export

crude_rates_fun <- function(data, by_var) {
  
  crosstabs_table <- data %>%
    dplyr::group_by(.data[[by_var]]) %>%
    dplyr::count() %>%
    sparklyr::collect() %>%
    dplyr::rename(group = all_of(by_var)) %>%
    dplyr::mutate(group = as.character(group),
                  domain = by_var,
                  totalsample = round(n / 5)*5) %>%
    dplyr::ungroup() %>%
    dplyr::select(domain, group, totalsample)
  
  suicides_rates <- data %>% 
    dplyr::group_by(.data[[by_var]], cause_death) %>% 
    dplyr::count() %>%
    sparklyr::collect() %>%
    dplyr::rename(group = all_of(by_var)) %>%
    dplyr::filter(cause_death %in% c("Death_Suicide", "Death_Other")) %>%
    dplyr::mutate(totalevents = round(n / 5)*5)
  
  jointable <- merge(crosstabs_table, suicides_rates) %>%
    dplyr::select(domain, group, totalsample, cause_death, totalevents) %>%
    tidyr::pivot_wider(names_from = cause_death,
                       values_from = totalevents) %>%
    dplyr::mutate(
      Rate_Other = as.numeric(round(Death_Other / totalsample * 100000, 2)),
      Rate_Suicide = as.numeric(round(Death_Suicide / totalsample * 100000, 2)),
      population_percent = as.numeric(round(100 * totalsample / sum(totalsample), 2)))

  return(jointable)

}


#--------------------------
# Crude rates by sex 
#--------------------------

#' Calculate crude rates for males and females
#'
#' @description Function to calculate counts of deaths and suicide by sex.
#' Updated to calculate rates on the rounded data.
#'
#' @param data An object of class "data.frame". A person-level data frame
#' containing "cause_death" and "sex" variable.
#' @param by_var Name of demographic variable to group by.
#'
#' @import dplyr
#' @import tidyr
#' 
#' @return Table containing numbers (rounded to nearest 5) and rates of deaths and suicide rounded to 2dp
#' values under 10 are suppressed with [c] and 0's which return NA are replaced with [c]
#'
#' @export

crude_rates_sex_fun <- function(data, by_var) {
  
  crosstabs_table <- data %>%
    dplyr::group_by(.data[[by_var]], sex) %>%
    dplyr::count() %>%
    sparklyr::collect() %>%
    dplyr::rename(group = all_of(by_var)) %>%
    dplyr::mutate(group = as.character(group),
                  domain = by_var,
                  totalsample = round(n / 5)*5) %>%
    dplyr::ungroup() %>%
    dplyr::select(sex, domain, group, totalsample)
  
  suicides_rates <- data %>% 
    dplyr::group_by(.data[[by_var]], cause_death, sex) %>% 
    dplyr::count() %>%
    sparklyr::collect() %>%
    dplyr::rename(group = all_of(by_var)) %>%
    dplyr::filter(cause_death %in% c("Death_Suicide", "Death_Other")) %>%
    dplyr::mutate(totalevents = round(n / 5)* 5)
  

  jointable <- merge(crosstabs_table, suicides_rates) %>%
    dplyr::select(sex, domain, group, totalsample, cause_death, totalevents) %>%
    tidyr::pivot_wider(names_from = cause_death,
                       values_from = totalevents) %>%
    dplyr::mutate(
      Rate_Other = as.numeric(round(Death_Other / totalsample * 100000, 2)),
      Rate_Suicide = as.numeric(round(Death_Suicide / totalsample * 100000, 2)),
      population_percent = as.numeric(round(100 * totalsample / sum(totalsample), 2)))

  return(jointable)

}

#--------------------------
# Person year rates by demographics
#--------------------------

#' Calculate rates person-years
#'
#' @description Function to calculate death rates per 100,000
#' person-years for suicides.
#'
#' @param data An object of class "data.frame". A person-level data frame
#' containing "cause_death" and "total_time_at_risk" variables.
#' @param by_var Name of demographic variable to group by.
#'
#' @import dplyr
#' @import tidyr
#' 
#' @return Table containing death rates per 100,000 person-years
#' rates where deaths are under 10 are suppressed with [c] 
#' and 0's which return NA are replaced with [c]
#'
#' @export

person_year_rates_fun <- function(data, by_var) {
  
  crosstabs_table <- data %>%
    dplyr::group_by(.data[[by_var]]) %>%
    dplyr::count() %>%
    sparklyr::collect() %>%
    dplyr::rename(group = all_of(by_var)) %>%
    dplyr::mutate(group = as.character(group),
                  domain = by_var,
                  totalsample = ifelse(n <10, "[c]", round(n / 5)*5)) %>%
    dplyr::ungroup() %>%
    dplyr::select(domain, group, totalsample)
  
  total_time_at_risk_table <- data %>%
    dplyr::mutate(time_at_risk_years = (total_time_at_risk / 365.25)) %>%
    dplyr::group_by(.data[[by_var]]) %>%    
    dplyr::summarise(time_at_risk_years = sum(time_at_risk_years)) %>%
    sparklyr::collect() %>%
    dplyr::rename(group = all_of(by_var)) %>%
    dplyr::mutate(group = as.character(group),
                  domain = by_var) %>%
    dplyr::ungroup() %>%
    dplyr::select(group, domain, time_at_risk_years)
  
  outcome_death_table <- data %>%
    dplyr::filter(cause_death %in% c("Death_Suicide", "Death_Other")) %>%
    dplyr::group_by(.data[[by_var]], cause_death) %>% 
    dplyr::summarise(deaths = n()) %>%
    dplyr::mutate(deaths = round(deaths / 5)*5) %>%
    sparklyr::collect() %>%
    dplyr::rename(group = all_of(by_var))
  
  poisson <- sparklyr::spark_read_csv(
    sc, name = "poisson",
    path = poisson_distribution_lookup,
    header = TRUE, delimiter = ",", null_value = c("NA")) %>%
    sparklyr::collect()

  jointable <- merge(outcome_death_table, crosstabs_table) %>%
    merge(total_time_at_risk_table) %>%
    dplyr::left_join(poisson, by = c("deaths" = "Deaths")) %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::select(domain, group, cause_death, deaths, totalsample, time_at_risk_years, L, U) %>%
    dplyr::mutate(
      Rate_Death = round(deaths / time_at_risk_years * 100000, 2),
      lower_ci = dplyr::case_when(
        deaths <  100 ~ round(L * Rate_Death, 2),
        deaths >= 100 ~ round(Rate_Death - 1.96 * (Rate_Death / sqrt(deaths)), 2)),
      upper_ci = dplyr::case_when(
        deaths <  100 ~ round(U * Rate_Death, 2),
        deaths >= 100 ~ round(Rate_Death + 1.96 * (Rate_Death / sqrt(deaths)), 2))) %>%
    select(domain, group, cause_death, deaths, totalsample, Rate_Death, lower_ci, upper_ci)
  
  cols_to_suppress <- c("deaths", "Rate_Death", "lower_ci", "upper_ci")
  
  return(jointable)
        
         }

#--------------------------
# Person year rates by demographics and by sex
#--------------------------

#' Calculate rates person-years for males and females
#'
#' @description Function to calculate death rates per 100,000
#' person-years for suicides by sex.
#'
#' @param data An object of class "data.frame". A person-level data frame
#' containing "cause_death", "total_time_at_risk" and "sex" variables.
#' @param by_var Name of demographic variable to group by.
#'
#' @import dplyr
#' @import tidyr
#' 
#' @return Table containing death rates per 100,000 person-years by sex
#' 
#' @export

person_year_rates_sex_fun <- function(data, by_var) {
  
  crosstabs_table <- data %>%
    dplyr::group_by(.data[[by_var]], sex) %>%
    dplyr::count() %>%
    sparklyr::collect() %>%
    dplyr::rename(group = all_of(by_var)) %>%
    dplyr::mutate(group = as.character(group),
                  domain = by_var,
                  totalsample = ifelse(n <10, "[c]", round(n / 5)*5)) %>%
    dplyr::ungroup() %>%
    dplyr::select(sex, group, domain, totalsample)
  
  total_time_at_risk_table <- data %>%
    dplyr::mutate(time_at_risk_years = (total_time_at_risk / 365.25)) %>%
    dplyr::group_by(.data[[by_var]], sex) %>%    
    dplyr::summarise(time_at_risk_years = sum(time_at_risk_years)) %>%
    sparklyr::collect() %>%
    dplyr::rename(group = all_of(by_var)) %>%
    dplyr::mutate(group = as.character(group),
                  domain = by_var) %>%
    dplyr::ungroup() %>%
    dplyr::select(group, domain, sex, time_at_risk_years)
  
  outcome_death_table <- data %>%
    dplyr::filter(cause_death %in% c("Death_Suicide", "Death_Other")) %>%
    dplyr::group_by(.data[[by_var]], cause_death, sex) %>% 
    dplyr::summarise(deaths = n()) %>%
    dplyr::mutate(deaths = round(deaths / 5)*5) %>%
    sparklyr::collect() %>%
    dplyr::rename(group = all_of(by_var))
    
  poisson <- sparklyr::spark_read_csv(
    sc, name = "poisson",
    path = poisson_distribution_lookup,
    header = TRUE, delimiter = ",", null_value = c("NA")) %>%
    sparklyr::collect()
  
  jointable <- merge(outcome_death_table, crosstabs_table) %>%
    merge(total_time_at_risk_table) %>%
    dplyr::left_join(poisson, by = c("deaths" = "Deaths")) %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::select(domain, group, sex, cause_death, deaths, totalsample, time_at_risk_years, L, U) %>%
    dplyr::mutate(
      Rate_Death = round(deaths / time_at_risk_years * 100000, 2),
      lower_ci = dplyr::case_when(
        deaths <  100 ~ round(L * Rate_Death, 2),
        deaths >= 100 ~ round(Rate_Death - 1.96 * (Rate_Death / sqrt(deaths)), 2)),
      upper_ci = dplyr::case_when(
        deaths <  100 ~ round(U * Rate_Death, 2),
        deaths >= 100 ~ round(Rate_Death + 1.96 * (Rate_Death / sqrt(deaths)), 2))) %>%
    select(domain, group, sex, cause_death, deaths, totalsample, Rate_Death, lower_ci, upper_ci)
  
  return(jointable)
        
}

#--------------------------
# Person year rates by age in interval on split dataset
#--------------------------


# Sum up total time of risk for all 10 year olds whether full
# year, or partial year, then divide by 365.25
#
# number of deaths
# ----------------
# time at risk in years (above divided by 365.25)


#' Calculate rates in person-years by age in interval on split dataset
#'
#' @description Function to calculate death rates per 100,000
#' person-years for suicides.
#'
#' @param data An object of class "data.frame". An interval-level data frame
#' containing "outcome_death_interval" and "time_at_risk_new" variables.
#' @param age Integer denoting age of interest.
#'
#' @import dplyr
#' @import tidyr
#' 
#' @return Table containing death rates per 100,000 person-years
#' 
#' @export

calculate_rates_in_person_years <- function(data = data, age = age) {
    
    crosstabs_table <- data %>%
    dplyr::group_by(.data[[age]]) %>%
    dplyr::count() %>%
    sparklyr::collect() %>%
    dplyr::mutate(totalsample = round(n / 5)*5) %>%
    dplyr::ungroup() %>%
    dplyr::select(age, totalsample)
  
  total_time_at_risk_table <- data %>%
    dplyr::group_by(.data[[age]]) %>%
    dplyr::mutate(time_at_risk_years = (time_at_risk_new / 365.25) * weight) %>%
    dplyr::summarise(time_at_risk_years = sum(time_at_risk_years))
  
  pop_table <- left_join(crosstabs_table, total_time_at_risk_table, by = age)
  
  outcome_death_table <- data %>%
      dplyr::filter(outcome_death_interval != "Alive") %>%
      dplyr::group_by(.data[[age]], outcome_death_interval) %>% 
      dplyr::summarise(deaths = n())
  
  poisson <- sparklyr::spark_read_csv(
    sc, name = "poisson",
    path = poisson_distribution_lookup,
    header = TRUE, delimiter = ",", null_value = c("NA")) %>%
    sparklyr::collect()
  
  jointable <- merge(outcome_death_table, pop_table) %>%
    dplyr::left_join(poisson, by = c("deaths" = "Deaths")) %>%
    dplyr::rename(cause_death = outcome_death_interval) %>%
  dplyr::mutate(
      Rate_Death = round(deaths / time_at_risk_years * 100000, 2),
      lower_ci = dplyr::case_when(
        deaths <  100 ~ round(L * Rate_Death, 2),
        deaths >= 100 ~ round(Rate_Death - 1.96 * (Rate_Death / sqrt(deaths)), 2)),
      upper_ci = dplyr::case_when(
        deaths <  100 ~ round(U * Rate_Death, 2),
        deaths >= 100 ~ round(Rate_Death + 1.96 * (Rate_Death / sqrt(deaths)), 2))) %>%
    select(cause_death, age_in_interval, totalsample, deaths, Rate_Death, lower_ci, upper_ci)
  
  return(jointable)
        
}


#--------------------------
# Poision model function
#--------------------------

#' GLM poisson model function'
#'
#' @description runs glm models with poisson function for a
#' data.frame 
#'
#' @param data An object of class "data.frame". A data frame
#' and formula containing an outcome measue and exposure variables 
#' contained in data frmae'
#' 
#' @import 
#' 
#' @return Table containing model outputs with IRR's 
#' for each exposure and covariate 
#'
#' @export

model_all <- function(data, formula) {
  
  model <- glm(formula = formula,
               family = poisson(link = "log"),
               data = data)
  
    model_output <- as.data.frame(summary(model)$coefficients)
    model_output$term <- rownames(model_output)
    model_output$irr <- exp(model_output[,1])
    model_output$lcl <- exp(model_output[,1] - 1.96 * model_output[,2])
    model_output$ucl <- exp(model_output[,1] + 1.96 * model_output[,2])
    model_output$bic <- BIC(model) 
    model_output$nobs <- nobs(model)
    model_output$model <- formula
    model_output$pvalue <- model_output[,4]

  return(model_output %>% select(term, irr, lcl, ucl, pvalue, bic, nobs, model))
}


#--------------------------
# Model IRR's function 
#--------------------------

#' Create Plots from poisson model displaying IRRs'
#'
#' @description plots irr's and ci's against the reference group run in 
#' runs in the glm models from poisson function for a
#' data.frame 
#'
#' @param data An object of class "data.frame". A data frame
#' and formula containing an outcome measue and exposure variables 
#' contained in data frame'
#' 
#' @import ggplot2
#' 
#' @return Plot containing IRR's for each exposure
#'
#' @export

plot_function <- function(data_frame, label, title, set_x = 1) {
  
  plot <- data_frame %>%
  dplyr::filter(term != "Unknown") %>%
  dplyr::filter(term != "Not stated") %>%
    ggplot2::ggplot(
      ggplot2::aes(x = irr, y = term, color = model_type)) +
    ggplot2::scale_y_discrete(limits = rev, labels = label_wrap(15)) +
    ggplot2::geom_point(size = 2, position = ggplot2::position_dodge(-.5)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = lcl, xmax = ucl),
      width = 0.1,
      position = ggplot2::position_dodge(-.5)) +
    ggplot2::ylab("") +
    ggplot2::xlab("Incidence rate ratio (IRR)") +
    ggplot2::guides(color = guide_legend(title = "Model")) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 15)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
    ggplot2::geom_text(
      ggplot2::aes(x = set_x, y = (length(term) / 2 + 0.4), label = label), colour = "black") +
    ggplot2::ggtitle(paste(outcome_plot, "deaths by", title)) +
    ggplot2::theme(plot.title.position = "plot")
  
  ggplot2::ggsave(paste0(project_directory, "/output/plots/", outcome_plot, " deaths by ", title, ".jpg"))
  
  return(plot)
}


# Socio-demographic differences in the risk of suicides in children and young people

This is the code used for the analysis of Risk factors for suicide in children and young people in England

Authors: Emma Sharland, Rachel Mullis, Emyr John and Isobel Ward

Copyrights: Crown Copyright

## Project details

Suicide is one of the leading causes of death in children and young people (CYP) globally. Over the past decade there has been a steady increase in the number of suicide deaths in CYP in the UK. This study aims to identify socio-demographic differences in the risk of suicide in CYP, to do this we conducted a population-level analysis of the socio-demographic characteristics of children and young people who died by suicide in England between 2011 and 2022.

Link to ONS release: "[Risk factors for suicide in children and young people in England](https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/mentalhealth/articles/riskfactorsforsuicideinchildrenandyoungpeopleinengland/2025-02-27)" and associated academic paper titled "[Socio-demographic differences in the risk of suicides in children and young people: a population level linked study in England, 2011 to 2022](addlink)".

## **Data souruces**

-   Census 2011

-   ONS death registrations

## **Study population**

We linked Census 2011 and death registrations by NHS number for usual residents in England. To obtain NHS number, we linked the 2011 Census to the 2011-2013 NHS Patient registers. Individuals were included in the study if they were aged between 10 and 17 years on Census Day (27 March 2011) or from the date of their 10th birthday if they were aged under 10 on Census Day. Individuals were followed until the end of study period, their 18th birthday or death whichever occurred first. 

## **Code structure**

-   1_spark_setup.R - Loads packages, sets up spark session and configures study parameters.

-   2_data_preparation.R - Creates the person-level dataset for anaylsis. First, we import and link Census and Deaths datasets, filter to the study population and calculate the sample flow at each stage. Variables are formatted and new birthdates and ages are created from 2011 Census, death dates are created from the deaths datasets and a deaths lookup imported and linked to create a flag for the cause of death. Socio-demographic variables from Census are formatted and where neccassary for analysis aggreggated to broader groupings. To create the household reference person's (HRP) characteristics the main Census dataset is filtered to HRP only, variables required are then formatted and linked back to the cohort using the household ID to link them.

-   2a_quality_assurance.R - Creates descriptives statistics and quality assures the deaths in the study cohort, Census recodes and time at risk.
 
-   2b_data_formatting.R - Samples and splits the person-level dataset to create an interval dataset by year. Time at risk is recalculated from the total time in the cohort to the time at risk in each interval.

-   2c_suicide_rates.R - Reads in the person-level dataset (demographic characteristics) and interval level dataset (age) to calculate crude and person year rates of death by suicide and non-suicide causes.

-   3_modelling.R - Reads in the sampled interval level dataset, correctly weights the time at risk for the live population by the sampling proprortion, defines reference categories and labels for each exposure, creates and models the age spline and identifies the best fitting model using the BIC to specify the number of knots. Calculates IRRs for generalised linear models with a poisson link function to account for time at risk by causes of death and plots IRRs and CI's to compare against the reference group for each exposure.

-   functions.R - Contains all functions used in the project scripts.

-   suicide_deaths_lookup.csv - External cause of death ICD-10 codes and defintions for intentional self-harm (X60-84) and events of undetermined intent (Y10-34)

## **Methods**

Using linked 2011 Census and death registrations data, we created a cohort of several million children and young people aged 10 to 17 years in England. 
-   Descriptive Statistics

-   Rates of suicide and non-suicide deaths per 100,000 people

-   We estimated adjusted incidence rate ratios (IRRs) using generalised linear models with a Poisson link function, to identify socio-demographic characteristics associated with death by suicide in CYP.

**Time at risk - Calculating the number of days an individual is in the study cohort**

Time at risk is defined as the number of days from when an individual enters the cohort either on Census day (if they are aged 10 to 17 on Census day) or the date of their 10th birthday after census day, to the end of the study period, their 18th birthday or their date of death whichever comes first.

The following variables are derivied to calculate time at risk:

-   dob_census - date of birth (from Census; based on date of birth quarter due to security permissions to access the data)

-   dob_10bday - date of 10th birthday, created to determine when an individual enters the study cohort

-   dob_18bday - date of 18th birthday, created to flag individuals who die before they turn 18 and to identify when they age out of the cohort

-   census_year_bday - Census year birthday, used to recalculate time at risk for individuals who age in on Census day (are aged 10 to 17 on Census day) this ensures their time at risk is current in the final interval dataset

-   cohort_end - the date which an individual ages out of the cohort, this is either their 18th birthday or the 31 December 2022 (end_date) of the study cohort whichever comes first (used to determine time at risk for alive population only) 

Which time at risk variable should you use:

-   person-level dataset - use total_time_at_risk = time_at_risk - time_at_risk_relcalculate - For those who age into the cohort on Census day (are already 10 on Census day) their time at risk start should start on Census day however we have set it to start at their census year birthday (this ensures individuals age up at their birthday and not Census day when the data goes into the SurvSplit function). total_time_at_risk is recalculated so it starts on Census day for those aged over 10 on Census day. 

-   interval level dataset - use time_at_risk_new = time_at_risk - time_at_risk_relcalculate - Where the data goes through the SurvSplit function it uses the time_at_risk variables to create yearly intervals for each individual, these are set to an individuals birthday so they age up correctly each year, this means all individuals first time interval = 365.25 but for someone who enters in the cohort on Census day (is already 10 before Census day) their first age interval is recalculated to start on Census day to their next birthday rather than be set to 1 year (365.25 days). 

-   time_at_risk - *Do not use for anaylsis, used in the SurvSplit function and to derive accurate time at risk variables in the person and interval level datasets only*

## **Data availability**

The source data are not publicly available and are subject to controlled access due to their sensitive nature. Census 2011 and death registration data are available through the Integrated Data Service (IDS). Details of the application requirements and process, and the use of data, are available at [https://integrateddataservice.gov.uk/how-to-access-the-integrated-data-service](https://integrateddataservice.gov.uk/how-to-access-the-integrated-data-service.).

## **Software**

Data preparation was conducted using Sparklyr version 3.3.0, and statistical analysis was performed using R version 4.4.

## **Funding and approvals** 

This project is funded by the National Institute for Health and Care Research (NIHR) Policy Research Programme (NIHR205990). The views expressed are those of the author(s) and not necessarily those of the NIHR or the Department of Health and Social Care.

Ethical approval was obtained from the National Statistician's Data Ethics Advisory Committee (NSDEC(22(17)).



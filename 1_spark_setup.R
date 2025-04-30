#--------------------------
## Load packages
#--------------------------

library(sparklyr)
library(tidyverse)
library(magrittr)
library(stringi)
library(survival)
library(splines)
library(emmeans)
library(janitor)

options(max.print = 9999)

#--------------------------
## Set up the spark connection
#--------------------------

xl_config <- sparklyr::spark_config()
xl_config$spark.executor.memory <- "20g"
xl_config$spark.yarn.executor.memoryOverhead <- "2g"
xl_config$spark.executor.cores <- 5
xl_config$spark.dynamicAllocation.enabled <- "true"
xl_config$spark.dynamicAllocation.maxExecutors <- 12
xl_config$spark.sql.shuffle.partitions <- 240

# Add to dates correctly in latest spark?
xl_config$spark.sql.parquet.int96RebaseModeInRead = "LEGACY"
xl_config$spark.sql.legacy.timeParserPolicy = "LEGACY"
xl_config$spark.sql.session.timeZone = "UTC+1:00"


sc <- sparklyr::spark_connect(
  master = "yarn-client",
  app_name = "xl-session",
  config = xl_config)

results_path <- "project_name/output/"

source(paste0(project_directory, "/functions.R"))

poisson_distribution_lookup <- "file_name",

# Set DOD to allow a year from current date due to delays in registration
start_date <- "2011-03-28"
end_date <- "2022-12-31"
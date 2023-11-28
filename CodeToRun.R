# Please restore the renv file first ----
# if you have not installed renv, please first install: install.packages("renv")
renv::activate()
renv::restore()

# Load required packages ----
library(DBI)
library(CDMConnector)
library(dplyr)
library(dbplyr)
library(here)
library(readr)
library(IncidencePrevalence)
library(PatientProfiles)
library(zip)
library(testthat)
library(log4r)

# Connect to database ----
# please see examples how to connect to the database here:
# https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
db <- dbConnect("...")

# parameters to connect to create cdm object ----
# name of the schema where cdm tables are located
cdmSchema <- "..."

# name of a schema in the database where you have writing permission
writeSchema <- "..."

# combination of at least 5 letters + _ (eg. "abcde_") that will lead any table
# written in the write schema
writePrefix <- "..."

# name of the database, use acronym in capital letters (eg. "CPRD GOLD")
dbName <- "..."

# minimum number of counts to be reported
minCellCount <- 5

# Run the study code ----
source(here("RunStudy.R"))

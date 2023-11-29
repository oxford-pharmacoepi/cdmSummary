# Parameters ----
resultsFolder <- here("Results")
ageStrata <- list(c(0,19), c(20,44), c(45,54), c(55,64), c(65,74), c(75,84), c(85,150))
sex <- c("Both", "Female", "Male")

# start log ----
log_file <- here(resultsFolder, "log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# create cdm object ----
info(logger, 'CREATE CDM OBJECT')
cdm <- cdmFromCon(
  con = db,
  cdmSchema = c(schema = cdmSchema),
  writeSchema = c(schema = writeSchema, prefix = writePrefix),
  cdmName = dbName
)

# check that you can create temp tables
info(logger, 'CHECK TEMP TABLE PERMISSION')
cdm$person %>%
  head(1) %>%
  computeQuery() %>%
  invisible()

# cdm snapshot ----
info(logger, 'CREATE SNAPSHOT')
write.csv(
  x = snapshot(cdm),
  file = here(resultsFolder, paste0(cdmName(cdm), "_snapshot", ".csv")),
  row.names = FALSE
)

# analyses ----
info(logger, 'SOURCE FUNCTIONS')
source(here("Analyses", "functions.R"))

info(logger, '1- LARGE SCALE CHARACTERISTICS')
source(here("Analyses", "1-LargeScaleCharacteristics.R"))

info(logger, '2- MONTHLY COUNTS')
source(here("Analyses", "2-MonthlyCounts.R"))

# export results ----
info(logger, 'CREATE ZIP FILE')
zip(
  zipfile = here(
    resultsFolder, paste0("cdmSummary-", cdmName(cdm), ".zip")
  ),
  files = list.files(resultsFolder),
  root = resultsFolder
)

info(logger, 'create denominator cohorts')
cdm <- generateDenominatorCohortSet(cdm = cdm, name = "den", overwrite = TRUE)
cdm$den <- cdm$den %>%
  mutate(
    cohort_start_date = as.Date(cohort_start_date),
    cohort_end_date = as.Date(cohort_end_date)
  )

info(logger, 'summarise lsc')
lsc <- cdm$den %>%
  summariseLargeScaleCharacteristics(
    window = list(c(0, Inf)),
    eventInWindow = c("condition_occurrence", "observation", "measurement", "procedure_occurrence", "device_exposure"),
    episodeInWindow = c("drug_exposure"),
    minimumFrequency = 0.005
  )

info(logger, 'export lsc')
write.csv(
  x = lsc,
  file = here("Results", paste0(cdmName(cdm), "_lsc", ".csv")),
  row.names = FALSE
)

info(logger, 'summarise characteristics')
characteristics <- cdm$den %>%
  summariseCharacteristics(ageGroup = ageStrata)

info(logger, 'export characteristics')
write.csv(
  x = characteristics,
  file = here("Results", paste0(cdmName(cdm), "_characteristics", ".csv")),
  row.names = FALSE
)

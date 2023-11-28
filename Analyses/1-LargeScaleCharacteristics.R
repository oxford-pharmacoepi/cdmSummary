info(logger, 'create denominator cohorts')
cdm <- generateDenominatorCohortSet(cdm = cdm, name = "den")

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

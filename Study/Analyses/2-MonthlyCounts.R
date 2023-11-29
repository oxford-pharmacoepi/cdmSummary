for (tab in c(
  "observation_period", "drug_exposure", "condition_occurrence", "observation",
  "measurement", "procedure_occurrence", "device_exposure"
)) {
  info(logger, paste0('incident ', tab))
  x <- monthlyIncident(cdm, tab)
  write.csv(
    x = x,
    file = here("Results", paste0(cdmName(cdm), "_incident_", tab, ".csv")),
    row.names = FALSE
  )
}

for (tab in c(
  "observation_period", "drug_exposure", "condition_occurrence",
  "procedure_occurrence", "device_exposure"
)) {
  info(logger, paste0('incident ', tab))
  x <- monthlyOngoing(cdm, tab)
  write.csv(
    x = x,
    file = here("Results", paste0(cdmName(cdm), "_ongoing_", tab, ".csv")),
    row.names = FALSE
  )
}

info(logger, "histogram year of birth sex")
x <- cdm$person %>%
  inner_join(cdm$observation_period %>% select("person_id") %>% distinct(), by = "person_id") %>%
  group_by(gender_concept_id, year_of_birth) %>%
  summarise(n = as.numeric(n()), .groups = "drop") %>%
  collect()
x <- x %>%
  union_all(
    x %>%
      group_by(year_of_birth) %>%
      summarise(n = sum(.data$n), .groups = "drop") %>%
      mutate(gender_concept_id = as.numeric(NA))
  ) %>%
  union_all(
    x %>%
      group_by(gender_concept_id) %>%
      summarise(n = sum(.data$n), .groups = "drop") %>%
      mutate(year_of_birth = as.numeric(NA))
  ) %>%
  mutate(n = if_else(n < 5, as.numeric(NA), .data$n))
write.csv(
  x = x,
  file = here("Results", paste0(cdmName(cdm), "_year_sex.csv")),
  row.names = FALSE
)

info(logger, "follow-up summary")
x <- cdm$observation_period %>%
  inner_join(cdm$person %>% select("person_id") %>% distinct(), by = "person_id") %>%
  mutate(obs_time = !!datediff("observation_period_start_date", "observation_period_end_date"))
x %>%
  summarise(
    number_observation_periods = n(),
    number_persons = n_distinct(person_id)
  )

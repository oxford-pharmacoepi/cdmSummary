generateAgeCohortSet <- function(cdm,
                                 name,
                                 ageGroup = list(c(0, 19), c(20, Inf)),
                                 targetCohortTable = NULL,
                                 targetCohortId = NULL,
                                 overwrite = TRUE) {
  ages <- unlist(ageGroup, recursive = TRUE)
  ageBreak <- ages + rep(c(0, 1), length(ages)/2)
  ageBreak <- unique(ageBreak)

  if (is.null(targetCohortTable)) {
    x <- cdm[["observation_period"]] |>
      dplyr::select(
        "subject_id" = "person_id",
        "cohort_start_date" = "observation_period_start_date",
        "cohort_end_date" = "observation_period_end_date"
      ) |>
      dplyr::mutate("cohort_definition_id" = 1)
    set <- dplyr::tibble(
      cohort_definition_id = 1, cohort_name = "age_cohort"
    )
  } else {
    x <- cdm[[targetCohortTable]]
    set <- CDMConnector::cohortSet(x)
    if (!is.null(targetCohortId)) {
      x <- x |>
        dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
      set <- set |>
        dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
    }
  }

  ageBreaks <- ageBreak[!is.infinite(ageBreak) & ageBreak > 0]
  plus1yr <- glue::glue(
    "CDMConnector::dateadd('date_of_birth',{ageBreaks},interval = 'year')"
  ) %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue("start_{seq_along(ageBreaks) + 1}"))
  minus1d <- glue::glue(
    "CDMConnector::dateadd('start_{seq_along(ageBreaks) + 1}',-1, interval = 'day')"
  ) %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue("enddd_{seq_along(ageBreaks)}"))
  x <- x %>%
    PatientProfiles::addDateOfBirth() %>%
    dplyr::mutate(!!!plus1yr) %>%
    dplyr::mutate(!!!minus1d, !!paste0("enddd_", length(ageBreaks) + 1) := .data$cohort_end_date) %>%
    dplyr::rename("start_1" = "date_of_birth", "obs_start" = "cohort_start_date", "obs_end" = "cohort_end_date") %>%
    CDMConnector::computeQuery()

  x <- x %>%
    tidyr::pivot_longer(dplyr::starts_with(c("start", "enddd"))) %>%
    dplyr::mutate(
      date = dplyr::if_else(
        substr(.data$name, 1, 5) == "start",
        "cohort_start_date",
        "cohort_end_date"
      ),
      obs_id = substr(.data$name, 7, 7)
    ) %>%
    dplyr::select(-"name") %>%
    tidyr::pivot_wider(names_from = "date", values_from = "value") %>%
    dplyr::mutate(
      "cohort_start_date" = dplyr::if_else(
        .data$cohort_start_date > .data$obs_start,
        .data$cohort_start_date,
        .data$obs_start
      ),
      "cohort_end_date" = dplyr::if_else(
        .data$cohort_end_date > .data$obs_end,
        .data$obs_end,
        .data$cohort_end_date
      )
    ) %>%
    dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    CDMConnector::computeQuery(
      name = name, temporary = FALSE, schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )

  cdm[[name]] <- CDMConnector::newGeneratedCohortSet(
    cohortRef = x, cohortSetRef = set, overwrite = TRUE
  )

  return(cdm)
}

info(logger, 'create denominator cohorts')
cdm <- generateAgeCohortSet(
  cdm = cdm, name = "den", overwrite = TRUE, ageGroup = ageStrata
)
cdm$den <- cdm$den %>%
  mutate(
    cohort_start_date = as.Date(cohort_start_date),
    cohort_end_date = as.Date(cohort_end_date)
  ) %>%
  addSex() %>%
  addAge(ageGroup = ageStrata)

info(logger, 'summarise lsc')
lsc <- cdm$den %>%
  summariseLargeScaleCharacteristics(
    strata = list("sex", "age_group", c("sex", "age_group")),
    window = list(c(0, Inf)),
    censorDate = "cohort_end_date",
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
  summariseCharacteristics(ageGroup = ageStrata, strata = list("sex", "age_group", c("sex", "age_group")))

info(logger, 'export characteristics')
write.csv(
  x = characteristics,
  file = here("Results", paste0(cdmName(cdm), "_characteristics", ".csv")),
  row.names = FALSE
)

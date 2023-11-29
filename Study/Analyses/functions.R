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
monthlyIncident <- function(cdm, tab) {
  date <- switch(
    tab,
    "observation_period" = "observation_period_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "condition_occurrence" = "condition_start_date",
    "observation" = "observation_date",
    "measurement" = "measurement_date",
    "procedure_occurrence" = "procedure_start_date",
    "device_exposure" = "device_exposure_start_date",
  )
  if (tab != "observation_period") {
    x <- cdm[[tab]] %>%
      addInObservation(indexDate = date) %>%
      filter(in_observation == 1)
  } else {
    x <- cdm[[tab]]
  }
  x <- x %>%
    rename("incidence_date" = all_of(date)) %>%
    mutate(
      "incidence_month" = !!datepart("incidence_date", "month"),
      "incidence_year" = !!datepart("incidence_date", "year")
    ) %>%
    group_by(incidence_month, incidence_year) %>%
    summarise(n = as.numierc(n()), .groups = "drop") %>%
    collect()
  x <- x %>%
    union_all(
      x %>%
        group_by(incidence_year) %>%
        summarise(n = sum(n), .groups = "drop") %>%
        mutate(incidence_month = as.numeric(NA))
    ) %>%
    mutate(n = if_else(n < 5, as.numeric(NA), .data$n))
  return(x)
}
monthlyOngoing <- function(cdm, tab) {
  dateS <- switch(
    tab,
    "observation_period" = "observation_period_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "condition_occurrence" = "condition_start_date",
    "procedure_occurrence" = "procedure_start_date",
    "device_exposure" = "device_exposure_start_date",
  )
  dateE <- switch(
    tab,
    "observation_period" = "observation_period_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "condition_occurrence" = "condition_start_date",
    "procedure_occurrence" = "procedure_start_date",
    "device_exposure" = "device_exposure_start_date",
  )
  if (tab != "observation_period") {
    x <- cdm[[tab]] %>%
      addInObservation(indexDate = date) %>%
      filter(in_observation == 1)
  } else {
    x <- cdm[[tab]]
  }
  x <- x %>%
    rename("start_date" = all_of(dateS), "end_date" = all_of(dateE)) %>%
    mutate(
      "start_date_month" = !!datepart("start_date", "month"),
      "start_date_year" = !!datepart("start_date", "year"),
      "end_date_month" = !!datepart("end_date", "month"),
      "end_date_year" = !!datepart("end_date", "year")
    ) %>%
    group_by(start_date_month, start_date_year, end_date_month, end_date_year) %>%
    summarise(n = as.numierc(n()), .groups = "drop") %>%
    compute()
  time <- expand.grid(
    ongoing_month = 1:12,
    ongoing_year = seq(
      x %>% pull("start_date_year") %>% min(),
      x %>% pull("end_date_year") %>% max()
    ),
    id = 1
  )
  x <- x %>%
    mutate(id = 1) %>%
    inner_join(time, copy = TRUE, by = "id", relationship = "many-to-one") %>%
    filter(
      ongoing_month >= start_date_month,
      ongoing_year >= start_date_year,
      ongoing_month <= end_date_month,
      ongoing_year <= end_date_year
    ) %>%
    group_by(ongoing_month, ongoing_year) %>%
    summarise(n = sum(.data$n)) %>%
    collect() %>%
  x <- x %>%
    union_all(
      x %>%
        group_by(ongoing_year) %>%
        summarise(n = sum(n), .groups = "drop") %>%
        mutate(ongoing_month = as.numeric(NA))
    ) %>%
    mutate(n = if_else(n < 5, as.numeric(NA), .data$n))
  return(x)
}

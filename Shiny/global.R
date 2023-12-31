# load packages -----
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(PatientProfiles)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
library(ggplot2)
library(fresh)
library(plotly)
library(IncidencePrevalence)
library(snakecase)

# theme -----
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#0c0e0c" 
  ),
  adminlte_sidebar(
    # width = "400px",
    dark_bg = "#d8323c", #  "#D8DEE9",
    dark_hover_bg = "#b72818", #"#81A1C1",
    dark_color ="white"# "#2E3440"
  ), 
  adminlte_global(
    content_bg = "#eaebea" 
  ),
  adminlte_vars(
    border_color = "#112446",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446"
  )
)
# functions ----
expandStrata <- function(x, group = "strata_name", level = "strata_level") {
  group <- x[[group]] %>%
    stringr::str_split(" and ")
  level <- x[[level]] %>%
    stringr::str_split(" and ")
  groups <- unique(unlist(group))
  for (k in seq_along(groups)) {
    col <- groups[k]
    dat <- lapply(seq_along(group), function(y) {
      res <- level[[y]][group[[y]] == col]
      if (length(res) == 0) {
        return(as.character(NA))
      } else {
        return(res)
      }
    }) %>%
      unlist()
    x[[col]] <- dat
  }
  return(x)
}

# read results from data folder ----
results <- list.files(here("data"), recursive = TRUE, full.names = TRUE)
results <- results[stringr::str_detect(results, ".csv")]

# cdm snapshot ------
cdm_snapshot_files <- results[stringr::str_detect(results, "snapshot")]
cdmsnapshot <- purrr::map(cdm_snapshot_files, readr::read_csv, show_col_types = FALSE, col_types = c(cdm_version = "c")) %>%
  bind_rows() %>%
  select(
    cdm_name, cdm_source_name, vocabulary_version, cdm_release_date, person_count, observation_period_count, earliest_observation_period_start_date, latest_observation_period_end_date
  )

# lsc ----
lsc <- results[stringr::str_detect(results, "lsc")]
lsc <- purrr::map(lsc, readr::read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  select(cdm_name, strata_name, strata_level, table_name, concept, variable, estimate_type, estimate) %>%
  expandStrata() %>%
  mutate(sex = coalesce(sex, "Both")) %>%
  mutate(age_group = coalesce(age_group, "0 to 150")) %>%
  select(-Overall, -strata_name, -strata_level) %>%
  filter(concept != 0) %>%
  mutate(
    age_group = factor(age_group, c("0 to 150", "0 to 19", "20 to 44", "45 to 54", "55 to 64", "65 to 74", "75 to 84", "85 to 150")),
    sex = factor(sex, c("Both", "Female", "Male")),
    variable = paste0(variable, " (", concept, ")")
  ) %>%
  filter(estimate_type == "percentage") %>%
  mutate(estimate = as.numeric(estimate/100)) %>%
  select(cdm_name, sex, age_group, table_name, variable, estimate)

# characterisation ----
char <- results[stringr::str_detect(results, "characteristics")]
char <- purrr::map(char, readr::read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  select(cdm_name, strata_name, strata_level, variable, variable_level, variable_type, estimate_type, estimate) %>%
  expandStrata() %>%
  mutate(sex = coalesce(sex, "Both")) %>%
  mutate(age_group = coalesce(age_group, "0 to 150")) %>%
  select(-Overall, -strata_name, -strata_level) %>%
  mutate(
    age_group = factor(age_group, c("0 to 150", "0 to 19", "20 to 44", "45 to 54", "55 to 64", "65 to 74", "75 to 84", "85 to 150")),
    sex = factor(sex, c("Both", "Female", "Male"))
  )

# followup ----
follow <- results[stringr::str_detect(results, "followup")]
follow <- purrr::map(follow, readr::read_csv, show_col_types = FALSE) %>%
  bind_rows()

# year and sex ----
yearsex <- results[stringr::str_detect(results, "year_sex")]
yearsex <- purrr::map(yearsex, readr::read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  mutate(
    sex = case_when(
      gender_concept_id == 8507 ~ "Male",
      gender_concept_id == 8532 ~ "Female",
      TRUE ~ "Both"
    )
  ) %>%
  filter(!is.na(year_of_birth)) %>%
  select(-gender_concept_id)

# summary tables ----
sumTabs <- results[stringr::str_detect(results, "summary_counts")]
sumTabs <- purrr::map(sumTabs, readr::read_csv, show_col_types = FALSE) %>%
  bind_rows()
sumTabs <- sumTabs %>%
  inner_join(
    sumTabs %>%
      filter(table == "person") %>%
      select(cdm_name, number_individuals = number_records),
    by = "cdm_name"
  ) %>%
  mutate(
    records_per_person = number_records / number_individuals,
    percentage_individuals_with_record = number_persons / number_individuals,
    percentage_in_observation = number_in_observation / number_records
  ) %>%
  select(cdm_name, table, number_records, number_concepts, number_persons, percentage_in_observation, records_per_person, percentage_individuals_with_record)

# incident counts ----
incident <- results[stringr::str_detect(results, "incident_counts")]
incident <- purrr::map(incident, readr::read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  mutate(date = as.Date(paste0(incidence_year, "/", incidence_month, "/01"))) %>%
  select(-incidence_month, -incidence_year)

# ongoing counts ----
ongoing <- results[stringr::str_detect(results, "ongoing_counts")]
ongoing <- purrr::map(ongoing, readr::read_csv, show_col_types = FALSE) %>%
  bind_rows()

# server shiny ----
server <- function(input, output, session) {
  # snapshot ----
  output$snapshot <- renderDataTable({
    datatable(cdmsnapshot, options = list(scrollX = TRUE))
  })
  output$snaphot_word <- downloadHandler(
    filename = "snapshots.docx",
    content = function(file) {
      cdmsnapshot %>%
        gt() %>%
        gtsave(filename = file)
    }
  )
  # characterisation ----
  getCharTable <- reactive({
    char %>%
      filter(
        sex %in% input$char_sex,
        cdm_name %in% input$char_cdm,
        age_group %in% input$char_age_group,
        estimate_type %in% input$char_est
      )
  })
  getGtCharTable <- reactive({
    getCharTable() %>%
      gtResult(
        long = list(Variable = c(level = "variable", "clean"), Level = c(level = "variable_level"), Format = c(level = "format")), 
        wide = list(`CDM Name` = c(level = "cdm_name"), "Sex" = c(level = "sex"), "Age group" = c(level = "age_group")), 
        format = c(`N (%)` = "count (percentage%)", "median [min; q25 - q75; max]", "mean (sd)", "median [q25 - q75]", N = "count"), 
        keepNotFormatted = TRUE, 
        decimals = c(default = 0), 
        decimalMark = ".", 
        bigMark = ","
      )
  })
  output$char_table <- render_gt({
    validate(need(nrow(getCharTable()) > 0, "No results for this settings"))
    getGtCharTable()
  })
  output$char_download_table <- downloadHandler(
    filename = "characteristics.docx",
    content = function(file) {
      getGtCharTable() %>%
        gtsave(filename = file)
    }
  )
  # lsc ----
  getLscTable <- reactive({
    lsc %>%
      filter(
        sex %in% input$lsc_sex,
        cdm_name %in% input$lsc_cdm,
        age_group %in% input$lsc_age_group,
        table_name %in% input$lsc_table
      ) %>%
      mutate(name = paste0(cdm_name, "<br>", sex, "<br>", age_group)) %>%
      select(-cdm_name, -sex, -age_group) %>%
      pivot_wider(names_from = "name", values_from = estimate)
  })
  getChoices <- reactive({
    x <- getLscTable()
    colnames(x) <- gsub("<br>", "; ", colnames(x))
    cols <- colnames(x)
    cols[!cols %in% c("table_name", "variable")]
  })
  output$lsc_table <- renderDataTable({
    validate(need(nrow(getLscTable()) > 1, "No data for this settings"))
    x <- getLscTable()
    cols <- colnames(x)
    cols <- cols[!cols %in% c("table_name", "variable")]
    x %>%
      datatable(options = list(scrollX = TRUE, pageLength = -1), escape = FALSE) %>%
      formatPercentage(columns = cols, digits = 1)
  })
  output$scatter_reference <- renderUI({
    choicesScatter <- getChoices()
    pickerInput(
      inputId = "lsc_reference_scatter", 
      label = "Reference", 
      choices = choicesScatter, 
      selected = choicesScatter[1], 
      multiple = FALSE
    )
  })
  output$lsc_scatter <- renderPlotly({
    x <- getLscTable()
    colnames(x) <- gsub("<br>", "; ", colnames(x))
    cols <- colnames(x)
    cols <- cols[!cols %in% c("table_name", "variable")]
    validate(need(length(cols) > 1, "select at least two groups"))
    ref <- input$lsc_reference_scatter
    dat <- x %>%
      pivot_longer(!c("table_name", "variable", all_of(ref))) %>%
      rename(reference = all_of(ref)) %>%
      mutate(reference = 100*reference, value = 100*value)
    ggplot(dat, aes(x = reference, y = value, col = name)) +
      geom_point() +
      xlim(0, 100) +
      ylim(0, 100) +
      geom_abline(intercept = 0, slope = 1)
  })
  # output$bar_reference <- renderUI({
  #   choicesBar <- getChoices()
  #   pickerInput(
  #     inputId = "lsc_reference_bar", 
  #     label = "Reference", 
  #     choices = choicesBar, 
  #     selected = choicesBar[1], 
  #     multiple = FALSE
  #   )
  # })
  # output$lsc_bar <- renderPlotly({
  #   x <- getLscTable()
  #   colnames(x) <- gsub("<br>", "; ", colnames(x))
  #   ref <- input$lsc_reference_bar
  #   validate(need(!is.null(ref), "reference cant be null"))
  #   print(ref)
  #   print(colnames(x))
  #   dat <- x %>%
  #     arrange(desc(.data[[ref]])) %>%
  #     head(100) %>%
  #     mutate(id = row_number()) %>%
  #     pivot_longer(!c("table_name", "variable", "id")) %>%
  #     mutate(value = 100*value) %>%
  #     mutate(concept = substr(x = .data$variable, start = 1, stop = 20))
  #   labs <- dat %>%
  #     select(id, concept) %>%
  #     arrange(-id) %>%
  #     distinct() %>%
  #     pull(concept)
  #   ggplot(dat, aes(x = -id, y = value, col = name)) +
  #     geom_bar(stat = "identity") + 
  #     scale_x_discrete(breaks=-(1:100), labels=labs) +
  #     coord_flip() +
  #     facet_grid(. ~ name)
  # })
  # summary tables ----
  # followup ----
  output$follow_plot <- renderPlotly({
    follow %>%
      filter(cdm_name %in% input$followup_cdm) %>%
      rename("Weeks of observation" = obs_weeks) %>%
      ggplot(aes(x = `Weeks of observation`, y = n)) +
      geom_bar(stat="identity") + 
      facet_wrap(vars(cdm_name), ncol = 2)
  })
  # year and sex ----
  output$year_sex_plot <- renderPlotly({
    yearsex %>%
      filter(cdm_name %in% input$year_sex_cdm) %>%
      ggplot(aes(x = year_of_birth, y = n, col = sex)) +
      geom_bar(stat="identity") + 
      facet_wrap(cdm_name ~ sex)
  })
  # incident counts ----
  output$plot_incident <- renderPlotly({
    x <- incident %>%
      filter(
        cdm_name %in% input$incident_cdm & tab %in% input$incident_table
      )
    if (input$incident_y == "Normalised") {
      x <- x %>%
        group_by(tab, cdm_name) %>%
        mutate(n = n / sum(n, na.rm = TRUE)) %>% 
        ungroup()
    }
    if (length(input$incident_facet) == 0) {
      x <- x %>% mutate(name = paste0(cdm_name, "; ", tab))
      p <- ggplot(x, aes(x = date, y = n, col = name)) +
        geom_line() +
        geom_point()
    } else if (length(input$incident_facet) == 2) {
      x <- x %>% mutate(name = paste0(cdm_name, "; ", tab))
      p <- ggplot(x, aes(x = date, y = n, col = name)) +
        geom_line() +
        geom_point() +
        facet_wrap(vars(name)) + theme(legend.position="none")
    } else if (length(input$incident_facet) == 1 && input$incident_facet == "cdm_name") {
      p <- ggplot(x, aes(x = date, y = n, col = tab)) +
        geom_line() +
        geom_point() +
        facet_wrap(vars(cdm_name))
    } else if (length(input$incident_facet) == 1 && input$incident_facet == "table") {
      p <- ggplot(x, aes(x = date, y = n, col = cdm_name)) +
        geom_line() +
        geom_point() +
        facet_wrap(vars(tab))
    }
    p
  })
  # ongoing counts ----
  output$plot_ongoing <- renderPlotly({
    x <- ongoing %>%
      filter(
        cdm_name %in% input$ongoing_cdm & table %in% input$ongoing_table
      )
    if (input$ongoing_y == "Normalised") {
      x <- x %>%
        group_by(table, cdm_name) %>%
        left_join(
          ongoing %>%
            filter(table == "observation_period") %>%
            rename(den = n) %>%
            select(-table),
          by = c("ongoing_date", "cdm_name")
        ) %>%
        mutate(n = n / den) %>%
        select(-den) %>%
        ungroup()
    }
    if (length(input$ongoing_facet) == 0) {
      x <- x %>% mutate(name = paste0(cdm_name, "; ", table))
      p <- ggplot(x, aes(x = ongoing_date, y = n, col = name)) +
        geom_line() +
        geom_point()
    } else if (length(input$ongoing_facet) == 2) {
      x <- x %>% mutate(name = paste0(cdm_name, "; ", table))
      p <- ggplot(x, aes(x = ongoing_date, y = n, col = name)) +
        geom_line() +
        geom_point() +
        facet_wrap(vars(name)) + theme(legend.position="none")
    } else if (length(input$ongoing_facet) == 1 && input$ongoing_facet == "cdm_name") {
      p <- ggplot(x, aes(x = ongoing_date, y = n, col = table)) +
        geom_line() +
        geom_point() +
        facet_wrap(vars(cdm_name))
    } else if (length(input$ongoing_facet) == 1 && input$ongoing_facet == "table") {
      p <- ggplot(x, aes(x = ongoing_date, y = n, col = cdm_name)) +
        geom_line() +
        geom_point() +
        facet_wrap(vars(table))
    }
    p
  })
  # end ----
}


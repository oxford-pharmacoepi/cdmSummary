# ui shiny ----
ui <- dashboardPage(
  dashboardHeader(title = "CDM characterisation"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background"
      ),
      menuItem(
        text = "Databases",
        tabName = "snap"
      ),
      menuItem(
        text = "Demographics",
        tabName = "demo",
        menuSubItem(
          text = "Follow-up", 
          tabName = "followup"
        ),
        menuSubItem(
          text = "Year of birth and sex", 
          tabName = "year_sex"
        )
      ),
      menuItem(
        text = "Tables summary",
        tabName = "tables"
      ),
      menuItem(
        text = "Counts",
        tabName = "counts", 
        menuSubItem(
          text = "Incident counts", 
          tabName = "incident"
        ), 
        menuSubItem(
          text = "Ongoing counts", 
          tabName = "ongoing"
        )
      ),
      menuItem(
        text = "Characteristics",
        tabName = "char"
      ),
      menuItem(
        text = "Large scale characterisation",
        tabName = "lsc"
      )
    )
  ),
  
  ## body ----
  dashboardBody(
    use_theme(mytheme),
    tabItems(
      # background  ------
      tabItem(
        tabName = "background",
        h3("Characteristing databases"),
        tags$hr(),
        a(
          img(src="logo.png", align = "right", height="2%", width="20%"), 
          href="https://www.ohdsi-europe.org/index.php/national-nodes/uk",
          target="_blank"
        )
      ),
      # cdm snapshot ------
      tabItem(
        tabName = "snap",
        DTOutput('snapshot'),
        tags$hr(),
        div(
          style="display:inline-block; float:right",
          downloadButton(
            outputId = "snaphot_word", label = "Download table as word"
          )
        )
      ),
      # large scale characterisation ----
      tabItem(
        tabName = "lsc",
        h3("Large scale characterisation"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "lsc_cdm", label = "Database", choices = unique(lsc$cdm_name), selected = unique(lsc$cdm_name), multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "lsc_age_group", label = "Age group", choices = levels(lsc$age_group), selected = "0 to 150", multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "lsc_sex", label = "Sex", choices = levels(lsc$sex), selected = "Both", multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "lsc_table", label = "Table", choices = unique(lsc$table_name), selected = unique(lsc$table_name), multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table of estimates",
            DTOutput('lsc_table') %>% withSpinner()
          ),
          tabPanel(
            "Scatter plot",
            uiOutput("scatter_reference"),
            plotlyOutput('lsc_scatter') %>% withSpinner()
          ),
          tabPanel(
            "Bar plot",
            uiOutput("bar_reference"),
            plotlyOutput('lsc_bar') %>% withSpinner()
          )
        )
      ),
      # characterisation ----
      tabItem(
        tabName = "char",
        h3("Characterisation"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "char_cdm", label = "Database", choices = unique(char$cdm_name), selected = unique(char$cdm_name), multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "char_age_group", label = "Age group", choices = levels(char$age_group), selected = "0 to 150", multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "char_sex", label = "Sex", choices = levels(char$sex), selected = "Both", multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "char_est", label = "Estimates", choices = unique(char$estimate_type), selected = c("count", "percentage", "median", "q25", "q75"), multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        tags$hr(),
        downloadButton("char_download_table", "Download word table"),
        gt_output("char_table")
      ),
      # followup ----
      tabItem(
        tabName = "followup",
        h3("Follow up distribution"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "followup_cdm", label = "Database", choices = unique(follow$cdm_name), selected = unique(follow$cdm_name), multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        plotlyOutput("follow_plot")
      ),
      # year of birth ----
      tabItem(
        tabName = "year_sex",
        h3("Year of birth and sex summary"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "year_sex_cdm", label = "Database", choices = unique(yearsex$cdm_name), selected = unique(yearsex$cdm_name), multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        plotlyOutput("year_sex_plot")
      ),
      # tables ----
      tabItem(
        tabName = "tables",
        h3("Summary of data in each table"),
        datatable(sumTabs, options = list(scrollX = TRUE, pageLength = -1)) %>%
          formatPercentage(c("percentage_in_observation", "percentage_individuals_with_record"), 1)
      ),
      # incidence counts ----
      tabItem(
        tabName = "incident",
        h3("Incident counts per table"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "incident_cdm", label = "cdm name", choices = unique(incident$cdm_name), selected = unique(incident$cdm_name), multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "incident_table", label = "Table", choices = unique(incident$tab), selected = unique(incident$tab), multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "incident_y", label = "Y axis", choices = c("Normalised", "Counts"), selected = "Counts", multiple = FALSE)
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "incident_facet", label = "Facet by", choices = c("cdm_name", "table"), selected = c("cdm_name", "table"), multiple = TRUE)
        ),
        plotlyOutput("plot_incident")
      ),
      # ongoing counts ----
      tabItem(
        tabName = "ongoing",
        h3("Ongoing counts per table"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "ongoing_cdm", label = "cdm name", choices = unique(ongoing$cdm_name), selected = unique(ongoing$cdm_name), multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "ongoing_table", label = "Table", choices = unique(ongoing$table), selected = unique(ongoing$table), multiple = TRUE, options = list(`actions-box` = TRUE))
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "ongoing_y", label = "Y axis", choices = c("Normalised", "Counts"), selected = "Counts", multiple = FALSE)
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "ongoing_facet", label = "Facet by", choices = c("cdm_name", "table"), selected = c("cdm_name", "table"), multiple = TRUE)
        ),
        plotlyOutput("plot_ongoing")
      )
      # end -----
    )
  )
)



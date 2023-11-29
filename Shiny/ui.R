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
        text = "Large scale characterisation",
        tabName = "lsc"
      ),
      menuItem(
        text = "Characteristics",
        tabName = "char"
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
      )
      # end -----
    )
  )
)



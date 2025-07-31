ui <- fluidPage(
  titlePanel("Epidemiological Dashboard"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("refresh", "Force refresh from DHIS2", value = FALSE),
      selectInput("morbidity_filter", "Select Morbidity Category:", choices = NULL)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", tableOutput("summary_table")),
        tabPanel("Visualizations", plotOutput("morbidity_plot"))
      )
    )
  )
)
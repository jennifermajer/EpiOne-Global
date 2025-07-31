server <- function(input, output, session) {
  data_reactive <- reactive({
    load_data(force_refresh = input$refresh)
  })
  
  observe({
    updateSelectInput(
      session,
      "morbidity_filter",
      choices = unique(data_reactive()$morbidity)
    )
  })
  
  output$summary_table <- renderTable({
    head(data_reactive())  # Display the first few rows of the processed data
  })
  
  output$morbidity_plot <- renderPlot({
    filtered_data <- data_reactive() %>%
      filter(morbidity == input$morbidity_filter)
    
    ggplot(filtered_data, aes(x = datevisit)) +
      geom_histogram(binwidth = 30, fill = "blue", color = "white") +
      labs(
        title = paste("Consultations for", input$morbidity_filter),
        x = "Date of Visit",
        y = "Number of Consultations"
      ) +
      theme_minimal()
  })
}
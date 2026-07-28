results_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      valueBox(
        value = textOutput(ns("rsq")),
        subtitle = "R-squared",
        icon = icon("chart-line"),
        color = "info"
      ),
      valueBox(
        value = textOutput(ns("n")),
        subtitle = "Observations",
        icon = icon("database"),
        color = "primary"
      )
    ),
    
    bs4Card(
      width = 12,
      title = "Predicted vs Actual",
      status = "warning",
      solidHeader = TRUE,
      plotOutput(ns("plot"))
    )
  )
}

results_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    output$rsq <- renderText({
      req(r$model)
      summary(r$model)$r.squared
    })
    
    output$n <- renderText({
      req(r$data)
      nrow(r$data)
    })
    
    output$plot <- renderPlot({
      req(r$model)
      preds <- predict(r$model)
      plot(preds, r$data[[all.vars(formula(r$model))[1]]],
           xlab = "Predicted",
           ylab = "Actual")
      abline(0, 1, col = "red", lwd = 2)
    })
    
  })
}

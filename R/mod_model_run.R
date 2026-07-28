model_run_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    bs4Card(
      title = "Run Model",
      status = "success",
      solidHeader = TRUE,
      
      uiOutput(ns("controls")),
      actionButton(ns("run_model"), "Run Model", class = "btn btn-primary"),
      verbatimTextOutput(ns("model_summary"))
    )
  )
}

model_run_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    # Dynamically build UI based on data
    output$controls <- renderUI({
      req(r$data)
      ns <- session$ns
      
      tagList(
        selectInput(ns("target"), "Select Target Variable", names(r$data)),
        selectInput(ns("features"), "Select Features", names(r$data), multiple = TRUE)
      )
    })
    
    observeEvent(input$run_model, {
      req(r$data, input$target, input$features)
      
      formula <- as.formula(
        paste(input$target, "~", paste(input$features, collapse = "+"))
      )
      
      r$model <- lm(formula, data = r$data)
    })
    
    output$model_summary <- renderPrint({
      req(r$model)
      summary(r$model)
    })
    
  })
}

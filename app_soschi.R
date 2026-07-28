library(shiny)
library(bs4Dash)
library(reactable)
library(plotly)
source("./R/helpers/load_packages.R")
source("./R/Functions/diseases_shared.R")
# source("./R/Functions/malaria.R")
# source("./R/Functions/diarrhea.R")

# Load modules
source("R/mod_load_data.R")
source("R/mod_thanks.R")

ui <- bs4DashPage(
  title = "Modeling Platform",
  
  bs4DashNavbar(),
  
  sidebar = bs4DashSidebar(
    collapsed = TRUE, minified = FALSE,
    # skin = "dark",
    status = "primary",
    
    bs4SidebarMenu(
      tags$div(
        style = "text-align:center; padding:15px;", # 
        # tags$a(href = "http://www.statistics.gov.rw/",
        #        tags$img(src = "NISR_logo.png", style = "width:140px; height:auto;")),
        tags$h5("SOSCHI - Climate & Health Statistics", style = "margin-top:10px;")
      ),
      bs4SidebarMenuItem("Load Data", tabName = "load", icon = icon("upload")),
      bs4SidebarMenuItem("Acknowledgments", tabName = "acknowledgements", icon = icon("file"))
    )
  ),
  
  body = bs4DashBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem("load", load_data_ui("load_page")),
      tabItem("acknowledgements", thanks_ui("thanks"))
    ),
    
    tags$footer(
      class = "main-footer-custom",
      
      # ── Partner logos row ──────────────────────────────────────
      div(class = "footer-partners",
          span(class = "footer-partners-label", "In partnership with:"),
          div(class = "footer-logos",
              tags$a(href = "http://www.aimsric.org/", 
                     tags$img(src = "AIMSRIC_logo.png", class = "partner-logo")),
              tags$a(href = "https://climate-health.officialstatistics.org/",
                     tags$img(src = "SOSCHI_logo.svg", class = "partner-logo"))
              # ,
              # tags$a(href = "https://www.moh.gov.rw/",
              #        tags$img(src = "MoH_logo.svg", class = "partner-logo"))
          )
      ),
      
      # ── Divider ────────────────────────────────────────────────
      tags$hr(class = "footer-divider")
      # ,
      # 
      # # ── Copyright centered ─────────────────────────────────────
      # div(class = "footer-copyright",
      #     HTML("&copy; 2026 National Institute of Statistics of Rwanda (NISR) &mdash; All rights reserved")
      # )
    )
  )
)

server <- function(input, output, session) {

  r <- reactiveValues(data = NULL, model = NULL)
  
  load_data_server("load_page", r)
  thanks_server("thanks", r)
}

shinyApp(ui, server)
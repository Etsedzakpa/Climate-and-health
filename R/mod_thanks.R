thanks_ui <- function(id) {
  ns <- NS(id)
  
  bs4Card(
      width = 12,
      title = "SOSCHI Acknowledgement",
      uiOutput(ns("thanks_text"))   # uiOutput, not htmlOutput
  )
}

thanks_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    output$thanks_text <- renderUI({
      HTML('
    <p>We would like to acknowledge that the statistical modelling used for producing
    the indicators in this release has been adapted from the methods and code included in
    the <a href="https://www.ons.gov.uk/aboutus/whatwedo/programmesandprojects/standardsforofficialstatisticsonclimatehealthinteractions"
       target="_blank">Standards for Official Statistics on Climate-Health Interactions (SOSCHI)</a> framework.
    The SOSCHI project aims to develop a standardised and globally generalisable statistical
    framework and <a href="https://climate-health.officialstatistics.org/">online platform</a> to facilitate the production of official statistics on
    climate and health, through an international collaboration led by ONS and funded by
    Wellcome (Grant number 224682/Z/21/Z).</p>
  ')
    })
  })
}
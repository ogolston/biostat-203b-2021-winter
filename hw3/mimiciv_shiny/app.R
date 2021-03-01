library(shiny)
library(tidyverse)

#icu_cohort = readRDS("hw3/mimiciv_shiny/icu_cohort.rds")

# Define UI for miles per gallon app ----
ui <- navbarPage("MIMIC-IV Data Dashboard",
  tabPanel("Admissions",
     sidebarLayout(
         sidebarPanel(
         selectInput("admit_var", "Choose variable of interest", 
                     choices = list("First Care Unit",
                                    "Last Care Unit",
                                    "Admission Type",
                                    "Admission Location"),
                     selected = "First Care Unit")
       ),
       mainPanel(
         plotOutput(outputId = "distPlot")
       )
    )   
  ),
  tabPanel("Patients",
    titlePanel("Patients Data"),
    sidebarLayout(
      sidebarPanel(
      ),
    mainPanel()
    )
  )
  # tabPanel("Lab Events",
  #   titlePanel("Lab Event Data"),
  #   sidebarLayout(
  #     sidebarPanel(
  #       #sliderInput()
  #     ),
  #    
  #     mainPanel(
  #       plotOutput(outputId = "distPlot")
  #     )
  #   )        
  # ),
  # tabPanel("Chart Events",
  #   titlePanel("Chart Event Data"),
  #   sidebarLayout(
  #     sidebarPanel(
  #       #sliderInput()
  #     ),
  #     
  #     mainPanel(
  #       plotOutput(outputId = "distPlot")
  #     )
  #   )         
  #)
)
  
  
 


server <- function(input, output) {

  output$distPlot <- renderPlot({
    # data <- switch(input$admit_var,
    #                "First Care Unit" = icu_cohort$first_careunit,
    #                "Last Care Unit" = icu_cohort$last_careunit,
    #                "Admission Type" = icu_cohort$admission_type,
    #                "Admission Location" = icu_cohort$admission_location)
    
    data <- switch(input$admit_var,
                   "First Care Unit" = "first_careunit",
                   "Last Care Unit" = "last_careunit",
                   "Admission Type" = "admission_type",
                   "Admission Location" = "admission_location")
    
    ggplot(icu_cohort) +
      geom_bar(aes_string(data))
    })
  
   
}


shinyApp(ui, server)


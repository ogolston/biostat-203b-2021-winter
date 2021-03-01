library(shiny)
library(tidyverse)

#icu_cohort = readRDS("hw3/mimiciv_shiny/icu_cohort.rds")


#Ideas: add option for pie chart? 
#Bivariate Distributions?
#Missing Data? 


ui <- navbarPage("MIMIC-IV Data Dashboard",
  tabPanel("Admissions",
     titlePanel("Admissions Data"),
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
         plotOutput(outputId = "admitPlot")
       )
    )   
  ),
  
  tabPanel("Patients",
    titlePanel("Patients Data"),
    sidebarLayout(
      sidebarPanel( ),
      mainPanel(
        #plotOutput(outputId = "patientPlot")
      )
    )
  ),
  
  tabPanel("Lab Events",
    titlePanel("Lab Event Data"),
    sidebarLayout(
      sidebarPanel( 
        selectInput("lab_var", "Choose lab measurement of interest", 
                    choices = list("Bicarbonate",
                                   "Calcium",
                                   "Chloride",
                                   "Creatinine",
                                   "Glucose",
                                   "Magnesium",
                                   "Potassium",
                                   "Sodium",
                                   "Hemacrit",
                                   "White Blood Cells",
                                   "Lactate"),
                    selected = "Bicarbonate")
      ),
      mainPanel(
        plotOutput(outputId = "labPlot")
      )
    )
  ),
  
  tabPanel("Chart Events",
    titlePanel("Chart Event Data"),
    sidebarLayout(
      sidebarPanel(
      ),

      mainPanel(
        #plotOutput(outputId = "chartPlot")
      )
    )
  ),
  
  tabPanel("Bivariate Distributions",
    titlePanel("Bivariate Distributions"),
    sidebarLayout(
      sidebarPanel(
      ),
      
      mainPanel(
        #plotOutput(outputId = "bivariatePlot")
      )
    )         
  )
)
  
  
 


server <- function(input, output) {

  output$admitPlot <- renderPlot({
    data <- switch(input$admit_var,
                   "First Care Unit" = "first_careunit",
                   "Last Care Unit" = "last_careunit",
                   "Admission Type" = "admission_type",
                   "Admission Location" = "admission_location")
    
    ggplot(icu_cohort) +
      geom_bar(aes_string(data))
    })
  
  
  output$labPlot <- renderPlot({
    data <- switch(input$lab_var,
                   "Bicarbonate" = "bicarbonate",
                   "Calcium" = "calcium",
                   "Chloride" = "chloride",
                   "Creatinine" = "creatinine",
                   "Glucose" = "glucose",
                   "Magnesium" = "magnesium",
                   "Potassium" = "potassium",
                   "Sodium" = "sodium",
                   "Hemacrit" = "hemacrit",
                   "White Blood Cells" = "wbc",
                   "Lactate" = "lactate")
    
    ggplot(icu_cohort) +
      geom_histogram(aes_string(data))
  })
   
}


shinyApp(ui, server)


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
      sidebarPanel( 
        selectInput("patient_var", "Choose variable of interest",
                    choices = list("Insurance",
                                   "Language",
                                   "Marital Status",
                                   "Ethnicity",
                                   "Gender",
                                   "Age",
                                   "Death in 30 Days?")         
        ) 
        
      ),
      mainPanel(
        plotOutput(outputId = "patientPlot")
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
        selectInput("chart_var", "Choose chart value of interest", 
                    choices = list("Heart Rate",
                                   "Systolic BP (non-invasive)",
                                   "Mean BP (non-invasive)",
                                   "Respiratory Rate",
                                   "Temperature (F)",
                                   "Systolic BP (arterial)",
                                   "Mean BP (arterial)"),
                    selected = "Heart Rate")
      ),

      mainPanel(
        plotOutput(outputId = "chartPlot")
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
  
  
  output$patientPlot <- renderPlot({
    data <- switch(input$patient_var,
                   "Insurance" = "insurance",
                    "Language" = "language",
                    "Marital Status" = "marital_status",
                    "Ethnicity" = "ethnicity",
                    "Gender" = "gender",
                    "Age" = "age_at_adm",
                    "Death in 30 Days?" = "death_in_month")
                   
                   
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
   
  output$chartPlot <- renderPlot({
    data <- switch(input$chart_var,
                   "Heart Rate" = "heart_rate",
                    "Systolic BP (non-invasive)" = 
                             "non_invasive_blood_pressure_systolic",
                    "Mean BP (non-invasive)" = 
                              "non_invasive_blood_pressure_mean",
                    "Respiratory Rate" = "respiratory_rate",
                    "Temperature (F)" = "temperature_fahrenheit",
                    "Systolic BP (arterial)" =
                              "arterial_blood_pressure_systolic",
                    "Mean BP (arterial)" =
                              "arterial_blood_pressure_mean")
    
    ggplot(icu_cohort) +
      geom_histogram(aes_string(data))
    
  })
}


shinyApp(ui, server)


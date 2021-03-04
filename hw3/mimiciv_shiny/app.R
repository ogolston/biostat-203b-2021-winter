library(shiny)
library(tidyverse)

#icu_cohort = readRDS("hw3/mimiciv_shiny/icu_cohort.rds")

#Ideas: add option for pie chart? 
#Missing Data? 

#Create list of variable names and meanings for each category, for use in 
#dropdown menus 

admissions_list <- list("First Care Unit" = "first_careunit",
                        "Last Care Unit" = "last_careunit",
                        "Admission Type" = "admission_type",
                        "Admission Location" = "admission_location")



patient_list <-  list("Insurance" = "insurance",
                       "Language" = "langauge",
                       "Marital Status" = "marital_status",
                       "Ethnicity" = "ethnicity",
                       "Gender" ="gender",
                       "Age" = "age_at_adm",
                       "Death in 30 Days?" = "death_in_month")


lab_list <-  list("Bicarbonate" = "bicarbonate",
                  "Calcium" = "calcium",
                  "Chloride" = "chloride",
                  "Creatinine" = "creatinine",
                  "Glucose" = "glucose",
                  "Magnesium" = "magnesium",
                  "Potassium" = "potassium",
                  "Sodium" = "sodium",
                  "Hematocrit" = "hematocrit",
                  "White Blood Cells" = "wbc",
                  "Lactate" = "lactate")


chart_list <- list("Heart Rate" = "heart_rate",
                   "Systolic BP (non-invasive)" 
                   = "non_invasive_blood_pressure_systolic",
                   "Mean BP (non-invasive)"= 
                     "non_invasive_blood_pressure_mean",
                   "Respiratory Rate" = "respiratory_rate",
                   "Temperature (F)" = "temperature_fahrenheit",
                   "Systolic BP (arterial)" =
                     "arterial_blood_pressure_systolic",
                   "Mean BP (arterial)" =
                     "arterial_blood_pressure_mean")




ui <- navbarPage("MIMIC-IV Data Dashboard",
  tabPanel("Admissions",
     titlePanel("Admissions Data"),
     sidebarLayout(
         sidebarPanel(
         selectInput("admit_var", "Choose variable of interest", 
                     choices = admissions_list,
                     selected = "first_careunit")
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
                    choices = patient_list,
                    selected = "insurance"),
        
        radioButtons("plot_type", "Choose type of visualization",
                     choices = c("Bar Plot", "Pie Chart"), 
                     selected = "Bar Plot")
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
                    choices = lab_list,
                    selected = "bicarbonate")
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
                    choices = chart_list,
                    selected = "heart_rate")
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
        selectInput("var1", "Choose variable for x-axis", 
                    choices = c(lab_list, chart_list),
                    selected = "heart_rate"),
        
        selectInput("var2", "Choose variable for y-axis", 
                    choices = c(lab_list, chart_list),
                    selected = "respiratory_rate"),
        
        selectInput("var3", "Optional: Choose variable for color", 
                    choices = c(patient_list, admissions_list),
                    selected = "gender")
      ),
      
      mainPanel(
        plotOutput(outputId = "bivariatePlot")
      )
    )         
  )
)
  
  
 


server <- function(input, output) {

  output$admitPlot <- renderPlot({
    data <- input$admit_var
    
    ggplot(icu_cohort) +
      geom_bar(aes_string(data))
    })
  
  
  output$patientPlot <- renderPlot({
    data <- input$patient_var
    viz <- input$plot_type
    if(viz == "Bar Plot"){
      ggplot(icu_cohort) +
        geom_bar(aes_string(data))
    } else {
      ggplot(icu_cohort, aes_string(x = factor(1), fill = data)) +
        geom_bar(width=1) +
        coord_polar("y") +
        labs(title = data, x = NULL)
    }              
    
    
  })
  
  output$labPlot <- renderPlot({
    data <- input$lab_var

    
    ggplot(icu_cohort) +
      geom_histogram(aes_string(data))
  })
   
  output$chartPlot <- renderPlot({
    data <- input$chart_var
    
    ggplot(icu_cohort) +
      geom_histogram(aes_string(data))
    
  })
  
  
  output$bivariatePlot <- renderPlot({
    var1 <- input$var1
    var2 <- input$var2
    var3 <- input$var3
    
    ggplot(icu_cohort) +
      geom_point(aes_string(var1, var2, color=var3))
    
  })
  
}


shinyApp(ui, server)


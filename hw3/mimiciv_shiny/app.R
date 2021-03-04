library(shiny)
library(tidyverse)
library(bslib)

#icu_cohort = readRDS("hw3/mimiciv_shiny/icu_cohort.rds")

#Missing Data? 

# Predefine variable lists for use in dropdown menus ---------------------------
admissions_list <- list("First Care Unit" = "first_careunit",
                        "Last Care Unit" = "last_careunit",
                        "Admission Type" = "admission_type",
                        "Admission Location" = "admission_location")

patient_list <-  list("Insurance" = "insurance",
                       "Language" = "language",
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


color_key_labs <- list("Red" = "tomato", 
                  "Yellow" = "darkgoldenrod1", 
                  "Blue" = "dodgerblue", 
                  "Violet" = "violet",
                  "Pink" = "palevioletred1",
                  "Grey" = "lightgrey")


color_key_charts <- list("Orange" = "sienna1",
                         "Green" = "lightgreen", 
                         "Cyan" = "cyan1",
                        "Purple" = "orchid",
                       "Brown" = "burlywood1",
                       "Grey" = "lightgrey")



ui <- navbarPage("MIMIC-IV Data Dashboard",
  theme = bs_theme(version = 4, bootswatch = "spacelab"),
  
  tabPanel("Admissions",          
     tabsetPanel(
     type = "tabs",
        tabPanel("Plots",
          sidebarLayout(
               sidebarPanel(
               selectInput("admit_var", "Choose variable of interest", 
                           choices = admissions_list,
                           selected = "first_careunit"),
               
               radioButtons("admit_plot_type", "Choose type of visualization",
                            choices = list("Bar Plot", "Pie Chart"), 
                            selected = "Bar Plot")
             ),
             
             mainPanel(
               plotOutput(outputId = "admitPlot"),
             )
           ),
        ),
        tabPanel("Table",
          sidebarLayout(
            sidebarPanel(
              selectInput("admit_var_sum", "Choose variable of interest", 
                           choices = admissions_list,
                           selected = "first_careunit"),
               
             ),
             mainPanel(
               tableOutput(outputId = "summary")
             )        
          )
        )
     )
  ),  
  tabPanel("Patients", 
    tabsetPanel(
    type = "tabs",
        tabPanel("Plots",
          sidebarLayout(
            sidebarPanel( 
               selectInput("patient_var", "Choose variable of interest",
                           choices = patient_list,
                           selected = "insurance"),
               
               radioButtons("plot_type", "Choose type of visualization",
                            choices = list("Bar Plot", "Pie Chart"), 
                            selected = "Bar Plot")
             ),
               
             mainPanel(
               plotOutput(outputId = "patientPlot")
             )
           )
         ),
         tabPanel("Table",
            sidebarLayout(
              sidebarPanel(
                selectInput("patient_var_sum", "Choose variable of interest", 
                            choices = patient_list,
                            selected = "insurance"),
                
              ),
              mainPanel(
                tableOutput(outputId = "patient_summary")
              )        
            )
         )
       )
    ),
  
  
  tabPanel("Lab Events",
    titlePanel("Lab Event Data"),
    sidebarLayout(
      sidebarPanel( 
        selectInput("lab_var", "Choose lab measurement of interest", 
                    choices = lab_list,
                    selected = "bicarbonate"),
        
        selectInput("lab_color", "Choose color for plot", 
                    choices = color_key_labs,
                    selected = "lightgrey"),
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
                    selected = "heart_rate"),
        
        selectInput("chart_color", "Optional: Choose color for plot", 
                    choices = color_key_charts,
                    selected = "lightgrey")
        
        #textInput("Optional: x_min", "Choose x-min", ""),
        #textInput("Optional: x_max", "Choose x-max", ""),
        
      ),

      mainPanel(
        plotOutput(outputId = "chartPlot")
      )
    )
  ),
  
  
  navbarMenu("Bivariate Distributions",
       tabPanel("Scatterplots",
          titlePanel("Scatterplots"),
          sidebarLayout(
            sidebarPanel(
              selectInput("var1", "Choose variable for x-axis", 
                          choices = c(lab_list, 
                                      chart_list, 
                                      "Age" = "age_at_adm"),
                          selected = "heart_rate"),
              
              selectInput("var2", "Choose variable for y-axis", 
                          choices = c(lab_list, 
                                      chart_list,
                                      "Age" = "age_at_adm"),
                          selected = "respiratory_rate")
              
            ),
                  
          mainPanel(
            plotOutput(outputId = "bivariatePlot")
          )
          )
        ),
             
             
         tabPanel("Boxplots",
            titlePanel("Boxplots"),
            sidebarLayout(
              sidebarPanel(
                selectInput("boxplot_var1", "Choose variable for x-axis", 
                            choices = c(patient_list),
                            selected = "insurance"),
                
                selectInput("boxplot_var2", "Choose variable for y-axis", 
                            choices = c(lab_list, chart_list),
                            selected = "bicarbonate"),
                
              ),
              
              mainPanel(
                plotOutput(outputId = "boxPlot")
              )
            )              
         )
  
    )
)
  
  
 


server <- function(input, output) {

  output$admitPlot <- renderPlot({
    data <- input$admit_var
    viz <- input$admit_plot_type

    if(viz == "Bar Plot"){
      ggplot(icu_cohort) +
        geom_bar(aes_string(data)) + 
        coord_flip()
    } else {
      ggplot(icu_cohort, aes_string(x = factor(1), fill = data)) +
        geom_bar(width = 1) +
        coord_polar("y") +
        theme_void()
    }
    
  })
  
  output$summary <- renderTable({
    data <- input$admit_var_sum
    
    icu_cohort %>%
      group_by(get(data)) %>%
      arrange() %>%
      count()
  })
  
  
  output$patientPlot <- renderPlot({
    data <- input$patient_var
    viz <- input$plot_type
    
    if(viz == "Bar Plot"){
      ggplot(icu_cohort) +
        geom_bar(aes_string(data))
    } else {
      ggplot(icu_cohort, aes_string(x = factor(1), fill = data)) +
        geom_bar(width = 1) +
        coord_polar("y") +
        theme_void()
    }              

  })
  
  output$patient_summary <- renderTable({
    data <- input$patient_var_sum

    icu_cohort %>%
      group_by(get(data)) %>%
      arrange() %>%
      count()
  })
  
  output$labPlot <- renderPlot({
    data <- input$lab_var

    
    ggplot(icu_cohort) +
      geom_histogram(aes_string(data), color = "black", 
                     fill = input$lab_color)
  })
   
  output$chartPlot <- renderPlot({
    data <- input$chart_var
    
    icu_cohort %>%
      ggplot() +
      geom_histogram(aes_string(data), color = "black", 
                     fill = input$chart_color)
    
  })
  
  
  output$bivariatePlot <- renderPlot({
    var1 <- input$var1
    var2 <- input$var2

    icu_cohort %>%
      filter(!is.na(get(var1)) & !is.na(get(var2))) %>%
      ggplot() +
      geom_jitter(aes_string(var1, var2))
    
  })
  
  output$boxPlot <- renderPlot({
    var1 <- input$boxplot_var1
    var2 <- input$boxplot_var2
    
    icu_cohort %>%
      ggplot() +
      geom_boxplot(aes_string(var1, var2))
  })
  
}


shinyApp(ui, server)


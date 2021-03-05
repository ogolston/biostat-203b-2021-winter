library(shiny)
library(tidyverse)
library(bslib)

#icu_cohort = readRDS("hw3/mimiciv_shiny/icu_cohort.rds")

#Missing Data? 

# Predefine variable lists for use in dropdown menus ---------------------------
categorical_list <- list("First Care Unit" = "first_careunit",
                        "Last Care Unit" = "last_careunit",
                        "Admission Type" = "admission_type",
                        "Admission Location" = "admission_location",
                        "Insurance" = "insurance",
                        "Language" = "language",
                        "Marital Status" = "marital_status",
                        "Ethnicity" = "ethnicity",
                        "Gender" ="gender",
                        "Death in 30 Days?" = "death_in_month")


numeric_list <-  list("Age" = "age_at_adm",
                  "Bicarbonate" = "bicarbonate",
                  "Calcium" = "calcium",
                  "Chloride" = "chloride",
                  "Creatinine" = "creatinine",
                  "Glucose" = "glucose",
                  "Magnesium" = "magnesium",
                  "Potassium" = "potassium",
                  "Sodium" = "sodium",
                  "Hematocrit" = "hematocrit",
                  "White Blood Cells" = "wbc",
                  "Lactate" = "lactate",
                  "Heart Rate" = "heart_rate",
                  "Systolic BP (non-invasive)" 
                   = "non_invasive_blood_pressure_systolic",
                  "Mean BP (non-invasive)"
                   = "non_invasive_blood_pressure_mean",
                  "Respiratory Rate" = "respiratory_rate",
                  "Temperature (F)" = "temperature_fahrenheit",
                  "Systolic BP (arterial)" =
                     "arterial_blood_pressure_systolic",
                  "Mean BP (arterial)" =
                     "arterial_blood_pressure_mean",
                  "Admission Time" = "intime",
                  "Discharge Time" = "outtime")


color_key <- list("Red" = "tomato", 
                  "Yellow" = "darkgoldenrod1", 
                  "Orange" = "sienna1",
                  "Green" = "lightgreen", 
                  "Blue" = "dodgerblue", 
                  "Violet" = "violet",
                  "Pink" = "palevioletred1",
                  "Brown" = "burlywood1",
                  "Grey" = "lightgrey")


ui <- navbarPage("MIMIC-IV Data Dashboard",
  theme = bs_theme(version = 4, bootswatch = "spacelab"),
  
  tabPanel("Categorical Variables",  
    titlePanel("Patient and Admission Characteristics"),
           
    tabsetPanel(
    type = "tabs",
      tabPanel("Plots",
        sidebarLayout(
          sidebarPanel(
            selectInput("cat_var", "Choose variable of interest", 
                         choices = categorical_list,
                         selected = "first_careunit"),
               
            radioButtons("cat_plot_type", "Choose type of visualization",
                          choices = list("Bar Plot", "Pie Chart"), 
                          selected = "Bar Plot")
            ),
             
            mainPanel(
              plotOutput(outputId = "catPlot")
            )
          )
        ),
     
     tabPanel("Table",
        sidebarLayout(
          sidebarPanel(
            selectInput("cat_var_sum", "Choose variable of interest", 
                         choices = categorical_list,
                         selected = "first_careunit")
           ),
          
           mainPanel(
             tableOutput(outputId = "catSummary")
           )        
         )
       )
     )
  ),  
  
  tabPanel("Quantitative Variables",  
    titlePanel("Quantitative Variables"),
           
    tabsetPanel(
      type = "tabs",
      tabPanel("Plots",
        sidebarLayout(
          sidebarPanel( 
            selectInput("num_var", "Choose lab measurement of interest", 
                        choices = numeric_list,
                        selected = "bicarbonate"),
            
            selectInput("plot_color", "Choose color for plot", 
                        choices = color_key,
                        selected = "tomato")
          ),
          
          mainPanel(
            plotOutput(outputId = "numPlot")
          )
        )
      ),
      
      tabPanel("Table",
         sidebarLayout(
           sidebarPanel(
             selectInput("num_var_sum", "Choose variable of interest", 
                         choices = numeric_list,
                         selected = "age_at_adm")
             
           ),
           
           mainPanel(
             tableOutput(outputId = "numSummary")
           )        
         )
       )
    )
  ),
  
  tabPanel("Bivariate Distributions",  
    titlePanel("Bivariate Distributions"),
    tabsetPanel(
      type = "tabs",
      tabPanel("Scatterplots",
          sidebarLayout(
            sidebarPanel(
              selectInput("var1", "Choose variable for x-axis", 
                          choices = numeric_list,
                          selected = "heart_rate"),
              
              selectInput("var2", "Choose variable for y-axis", 
                          choices = numeric_list,
                          selected = "age_at_adm")
              
              ),
              
              mainPanel(
                plotOutput(outputId = "bivariatePlot")
              )    
          )
       ),
     tabPanel("Boxplots",
        sidebarLayout(
          sidebarPanel(
            selectInput("boxplot_var1", "Choose variable for x-axis", 
                        choices = categorical_list,
                        selected = "insurance"),
            
            selectInput("boxplot_var2", "Choose variable for y-axis", 
                        choices = numeric_list,
                        selected = "bicarbonate")
          ),
          
          mainPanel(
            plotOutput(outputId = "boxPlot")
          )
        )              
      )
    )
   )
  
)

server <- function(input, output) {

  output$catPlot <- renderPlot({
    data <- input$cat_var
    viz <- input$cat_plot_type

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
  
  output$catSummary <- renderTable({
    data <- input$cat_var_sum
    
    icu_cohort %>%
      group_by(get(data)) %>%
      arrange() %>%
      count()
  })
  
  output$numPlot <- renderPlot({
    data <- input$num_var

    ggplot(icu_cohort) +
      geom_histogram(aes_string(data), color = "black", 
                     fill = input$plot_color)
  })
  
  
  output$numSummary <- renderTable({
    data <- icu_cohort[[input$num_var_sum]]

    tibble(
      Statistic = c("Min", "1st Quartile", "Median", "Mean", 
                    "3rd Quartile", "Max", "# of NAs"),
      Value = c(min(data, na.rm=T),
                 quantile(data, .25, na.rm=T),
                 median(data, na.rm=T),
                 mean(data, na.rm=T),
                 quantile(data, .75, na.rm=T),
                 max(data, na.rm=T),
                 sum(is.na(data)))
      )
  }) 


  output$bivariatePlot <- renderPlot({
    var1 <- input$var1
    var2 <- input$var2

    icu_cohort %>%
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


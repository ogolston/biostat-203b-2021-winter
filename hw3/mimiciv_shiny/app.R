library(shiny)
library(tidyverse)
library(bslib)

#icu_cohort = readRDS("hw3/mimiciv_shiny/icu_cohort.rds")

#Missing Data? 

# Predefine variable lists for use in dropdown menus ------------------------
categorical_list <- list("First Care Unit" = "first_careunit",
                        "Last Care Unit" = "last_careunit",
                        "Admission Type" = "admission_type",
                        "Admission Location" = "admission_location",
                        "Insurance" = "insurance",
                        "Language" = "language",
                        "Marital Status" = "marital_status",
                        "Ethnicity" = "ethnicity",
                        "Gender" ="gender",
                        "Death in 30 Days" = "death_in_month")


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
                     "arterial_blood_pressure_mean")


color_key <- list("Red" = "tomato", 
                  "Orange" = "sienna1",
                  "Yellow" = "darkgoldenrod1", 
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
            textOutput(outputId = "catSummaryTitle"),
            tableOutput(outputId = "catSummary")
          )        
        ) 
      )
    )
  ),  
  
  tabPanel("Quantitative Variables",  
    titlePanel("Age, Labs, and Chart Measurements"),
           
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
                        selected = "tomato"),
            
            radioButtons("provide_axis", "Provide custom axis?", 
                         c("Use default axis", "Create custom axis"),
                         "Use default axis"),
            
            conditionalPanel(
              condition = "input.provide_axis == 'Create custom axis'",
              
              helpText("Use slider below to adjust x-axis limits. This
                       can help in cases of extreme outliers. You can return
                       to default by clicking 'Use default axis' above."),
              
              sliderInput("xvals", "Set x-min and max", 0, 500,
                          c(0, 500))
            )
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
            textOutput(outputId = "numSummaryTitle"),
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
                          selected = "age_at_adm"),
              
              selectInput("var3", "Optional: Choose variable for color", 
                          choices = list("None" = "NULL",
                                      "Death in 30 days?" = "death_in_month",
                                      "Gender" = "gender"),
                          selected = "NULL"),
              
              radioButtons("scatter_provide_axis", "Provide custom axes?", 
                           c("Use default axes", "Create custom axes"),
                           "Use default axes"),
            
              
              conditionalPanel(
                condition = "input.scatter_provide_axis == 'Create custom axes'",
                
                helpText("Use slider below to adjust x and y axis limits. This
                       can help in cases of extreme outliers. You can return
                       to default by clicking 'Use default axis' above."),
                
                sliderInput("scatter_xvals", "Set x-min and max", 0, 500,
                            c(0, 500)),
                
                sliderInput("scatter_yvals", "Set y-min and max", 0, 500,
                            c(0, 500))
              )
              
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
    name <- names(which(categorical_list == data))
    viz <- input$cat_plot_type

    if(viz == "Bar Plot"){
      ggplot(icu_cohort) +
        geom_bar(aes_string(data), color = "black", fill = "deepskyblue") + 
        coord_flip() +
        labs(title = str_c("Distribution of ", name), x = name)
    } else {
      ggplot(icu_cohort, aes_string(x = factor(1), fill = data)) +
        geom_bar(width = 1) +
        coord_polar("y") +
        theme_void() +
        labs(title = str_c("Distribution of ", name), x = name)
    }
    
  })
  
  
  output$catSummaryTitle <- renderText({
    data <- input$cat_var_sum
    name <- names(which(categorical_list == data))
    
    str_c("Frequency table for ", name)
    
  })
  
  
  output$catSummary <- renderTable({
    data <- input$cat_var_sum

    table <- icu_cohort %>%
      group_by(get(data)) %>%
      count() %>%
      arrange(desc(n)) 
    
    colnames(table) <- c("Value", "Count")
    
    table
  })
  
  output$numPlot <- renderPlot({
    data <- input$num_var
    name <- names(which(numeric_list == data))
    
    choice <- input$provide_axis
    
    x_min <- input$xvals[1]
    x_max <- input$xvals[2]
    
    if(choice == "Use default axis"){
      ggplot(icu_cohort) +
        geom_histogram(aes_string(data), color = "black", 
                       fill = input$plot_color) +
        labs(title = str_c("Distribution of ", name), x = name)
    } else {
      ggplot(icu_cohort) +
        geom_histogram(aes_string(data), color = "black", 
                       fill = input$plot_color) +
        xlim(x_min, x_max) +
        labs(title = str_c("Distribution of ", name), x = name)
    }
  })
  
  output$numSummaryTitle <- renderText({
    name <- names(which(numeric_list == input$num_var_sum))
    str_c("Summary statistics for ", name)
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
    name1 <- names(which(numeric_list == var1))
    
    var2 <- input$var2
    name2 <- names(which(numeric_list == var2))
    
    var3 <- input$var3
    
    choice <- input$scatter_provide_axis
    xmin <- input$scatter_xvals[1]
    xmax <- input$scatter_xvals[2]
    ymin <- input$scatter_yvals[1]
    ymax <- input$scatter_yvals[2]
    
    
    if(choice == "Use default axes"){
      icu_cohort %>%
        ggplot() +
        geom_jitter(aes_string(var1, var2, color = var3)) +
        labs(x = name1, y = name2, 
             title = str_c(name2, " vs. ", name1))
      
    } else{
      icu_cohort %>%
        ggplot() +
        geom_jitter(aes_string(var1, var2, color = var3)) +
        labs(x = name1, y = name2, 
             title = str_c(name2, " vs. ", name1)) +
        xlim(xmin, xmax) +
        ylim(ymin, ymax)
    }
  })
  
  output$boxPlot <- renderPlot({
    var1 <- input$boxplot_var1
    var2 <- input$boxplot_var2
    
    name1 <- names(which(categorical_list == var1))
    name2 <- names(which(numeric_list == var2))
    
    icu_cohort %>%
      ggplot() +
      geom_boxplot(aes_string(var1, var2)) +
      labs(x = name1, y = name2, 
           title = str_c(name2, " grouped by ", name1))
    
  })
  
}


shinyApp(ui, server)


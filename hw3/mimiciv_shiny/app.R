library(shiny)
library(tidyverse)
library(bslib)

icu_cohort = readRDS("icu_cohort.rds")

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
                  "Systolic Blood Pressure (non-invasive)" 
                   = "non_invasive_blood_pressure_systolic",
                  "Mean Blood Pressure (non-invasive)"
                   = "non_invasive_blood_pressure_mean",
                  "Respiratory Rate" = "respiratory_rate",
                  "Temperature (F)" = "temperature_fahrenheit",
                  "Systolic Blood Pressure (arterial)" =
                     "arterial_blood_pressure_systolic",
                  "Mean Blood Pressure (arterial)" =
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
            htmlOutput(outputId = "catSummaryTitle"),
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
            selectInput("num_var", "Choose variable of interest", 
                        choices = numeric_list,
                        selected = "age_at_adm"),
            
            selectInput("plot_color", "Choose color for plot", 
                        choices = color_key,
                        selected = "tomato"),
            
            numericInput("bins", "Adjust number of bins", 
                        value = 30,
                        min = 10, max = 75),
            
            radioButtons("provide_axis", "Provide custom axis?", 
                         c("Use default axis", "Create custom axis"),
                         "Use default axis"),
            
            conditionalPanel(
              condition = "input.provide_axis == 'Create custom axis'",
              
              helpText("Use slider below to adjust x-axis limits. This
                       can help when there are extreme outliers. You can return
                       to default by clicking 'Use default axis' above."),
              
              sliderInput("xvals", "", 0, 600, c(0, 150))
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
            htmlOutput(outputId = "numSummaryTitle"),
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
                        selected = "age_at_adm"),
            
            selectInput("var2", "Choose variable for y-axis", 
                        choices = numeric_list,
                        selected = "heart_rate"),
            
            checkboxInput("var3", "Check box to color by death status", 
                          value = FALSE),
            
            radioButtons("scatter_axis", NULL, 
                         c("Use default axes", "Create custom axes"),
                         "Use default axes"),
          
              
            conditionalPanel(
              condition = "input.scatter_axis == 'Create custom axes'",
              
              helpText("Use sliders below to adjust x and y axis limits. You 
                       can return to default by clicking 'Use default axis' 
                       above."),
              
              sliderInput("scatter_xvals", "Set x-min and max:", 
                          min = 0, max = 600, value = c(0, 150)),
              
              sliderInput("scatter_yvals", "Set y-min and max:", 
                          min = 0, max = 600, value = c(0, 150))
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
            selectInput("boxplot_cat", "Choose grouping variable", 
                         choices = categorical_list,
                         selected = "death_in_month"),
            
            selectInput("boxplot_num", "Choose numeric outcome variable", 
                         choices = numeric_list,
                         selected = "bicarbonate"),
            
            radioButtons("provide_axis_box", "Provide custom axis?", 
                          c("Use default axis", "Create custom axis"),
                          "Use default axis"),
            
            conditionalPanel(
              condition = "input.provide_axis_box == 'Create custom axis'",
            
              helpText("Use slider below to adjust axis limits. This
                       can help visualization when there are outliers, but 
                       will not remove those values. You can return
                       to default by clicking 'Use default axis' above."),
            
              sliderInput("xvals_box", "Set x-min and max", 
                          min = 0, max = 600, value = c(0, 150))
            )
          ),
          
          mainPanel(
            plotOutput(outputId = "boxPlot")
          )
        )              
      )
    )
  ),
  
  tabPanel("About",
    titlePanel("About MIMIC-IV and this App"),
    h6("Data in this app comes from the MIMIC-IV dataset, which is derived from 
    several years of health records for a medical center in Boston, 
    Massachusetts.", br(), "This dashboard allows exploration of admissions, 
    demographic, lab, and chart data for the ICU stays of 50,048 
    unique adult patients.", br(), br(), "All data is deidentified and use 
    was authorized by MIT. Documentation details are available", 
    a("here", href = "http://mimic-iv.mit.edu/docs/", target = "_blank")), 

    p("This app was created by Olivia Golston for Biostatistics 203B at UCLA, 
    using RStudio and Shiny.")      
  )
  
)

server <- function(input, output) {

  output$catPlot <- renderPlot({
    data <- input$cat_var
    name <- names(which(categorical_list == data))
    viz <- input$cat_plot_type

    if (viz == "Bar Plot") {
      ggplot(icu_cohort) +
        geom_bar(aes_string(data), color = "black", fill = "deepskyblue") + 
        coord_flip() +
        labs(title = str_c(name), x = "") +
        theme(axis.text = element_text(size = 14),
              plot.title = element_text(size = 16, face = "bold")) 
    } else {
      ggplot(icu_cohort, aes_string(x = factor(1), fill = data)) +
        geom_bar(width = 1) +
        coord_polar("y") +
        theme_void() +
        labs(title = str_c("Distribution of ", name), x = "") +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = .5),
              legend.title = element_blank(),
              legend.text = element_text(size = 12))
    }
    
  })
  
  
  output$catSummaryTitle <- renderUI({
    data <- input$cat_var_sum
    name <- names(which(categorical_list == data))
    HTML(paste("<h3>", "Frequency Table for ", name, "</h3>"))
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
    
    base_plot <-  ggplot(icu_cohort) +
      geom_histogram(aes_string(data), color = "black", 
                     fill = input$plot_color, bins = input$bins) +
      labs(title = str_c("Distribution of ", name), x = name) +
      theme(axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
    
    if (choice == "Use default axis") {
      base_plot
    } else {
      base_plot +         
        xlim(x_min, x_max)
    }
  })
  
  output$numSummaryTitle <- renderUI({
    name <- names(which(numeric_list == input$num_var_sum))
    HTML(str_c("<h4>", "Summary Statistics for ", name, "</h4>"))
  })
  
  output$numSummary <- renderTable({
    data <- icu_cohort[[input$num_var_sum]]

    tibble(
      Statistic = c("Min", "1st Quartile", "Median", "Mean", 
                    "3rd Quartile", "Max", "# of Measurements", "# of NAs"),
      
      Value = c(min(data, na.rm = T),
                 quantile(data, .25, na.rm = T),
                 median(data, na.rm = T),
                 mean(data, na.rm = T),
                 quantile(data, .75, na.rm = T),
                 max(data, na.rm = T),
                 sum(!is.na(data)),
                 sum(is.na(data)))
    )
  }) 

  output$bivariatePlot <- renderPlot({
    var1 <- input$var1
    name1 <- names(which(numeric_list == var1))
    
    var2 <- input$var2
    name2 <- names(which(numeric_list == var2))
    
    colorvar <- ifelse(input$var3, "death_in_month", "NULL")
    
    
    choice <- input$scatter_axis
    xmin <- input$scatter_xvals[1]
    xmax <- input$scatter_xvals[2]
    ymin <- input$scatter_yvals[1]
    ymax <- input$scatter_yvals[2]
    
    
    base_plot <- icu_cohort %>%
      ggplot() +
      geom_jitter(aes_string(var1, var2, color = colorvar)) +
      labs(x = name1, y = name2, 
           title = str_c("Scatterplot of ", name2, " vs. ", name1)) +
      theme(axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
    
    
    if (choice == "Use default axes") {
      base_plot
    } else {
      base_plot +
        xlim(xmin, xmax) +
        ylim(ymin, ymax)
    }
  })
  
  output$boxPlot <- renderPlot({
    cat <- input$boxplot_cat
    num <- input$boxplot_num
    
    name_cat <- names(which(categorical_list == cat))
    name_num <- names(which(numeric_list == num))
    
    choice <- input$provide_axis_box
    
    x_min <- input$xvals_box[1]
    x_max <- input$xvals_box[2]
    
    base_plot <- icu_cohort %>%
      ggplot() +
      geom_boxplot(aes_string(num, cat)) +
      labs(x = name_num, y = name_cat, 
           title = str_c("Distribution of ", name_num, " by ", name_cat)) +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold")) 
    
    if (choice == "Use default axis") {
      base_plot
    } else {
      base_plot +
        coord_cartesian(xlim = input$xvals_box) 
    }
    
  })
  
}


shinyApp(ui, server)

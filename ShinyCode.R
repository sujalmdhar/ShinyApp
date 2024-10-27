# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(lubridate)
library(rsconnect)
library(shinythemes)
library(DT)       # For interactive data tables
library(plotly)   # For interactive plotting

# Load the dataset using a relative path
data <- read.csv("AirQuality.csv")

# Define the UI with a modern look
ui <- fluidPage(
  theme = shinytheme("flatly"),  # A clean theme for the dashboard
  titlePanel("Air Quality Dashboard", windowTitle = "Air Quality Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", 
                  choices = sort(unique(data$Country)), 
                  selected = sort(unique(data$Country))[1],
                  width = '100%'),
      hr(),
      h4("Additional Info"),
      helpText("Select a country to view air quality data and statistics."),
      helpText("Explore AQI categories and trends in the selected region."),
      width = 3  # Set width for sidebar
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Dataset Introduction",
                           fluidRow(
                             column(12,
                                    h4("Dataset Introduction"),
                                    p("This dashboard provides an interactive platform to explore air quality data across various global regions. The dataset integrates air quality indices (AQI) collected from multiple cities, enabling a detailed analysis of pollution levels."),
                                    p("Air quality is a critical aspect of public health, affecting millions worldwide. Understanding the factors contributing to poor air quality, such as specific pollutants like PM2.5, carbon monoxide, and ozone, is essential for effective policy-making and health interventions."),
                                    p("Each record in the dataset includes detailed information on various pollutants categorized by severity based on AQI values. This categorization helps in assessing the health impacts of different pollution levels, making the data particularly useful for researchers and policymakers."),
                                    p("The dataset also features a 'Date' column, allowing for the creation of Monthly Trend Plots to observe temporal variations in air quality. By tracking these trends, stakeholders can identify patterns and implement targeted strategies to combat air pollution."),
                                    p("Ultimately, this dashboard aims to promote public awareness and engagement in addressing air quality issues, fostering healthier environments for communities worldwide. Users are encouraged to explore the data and utilize the insights gained to advocate for better air quality standards and practices.")
                             )
                           )
                  ),
                  tabPanel("Dataset Overview",
                           fluidRow(
                             column(12,
                                    h4("Dataset Overview for All Countries"),
                                    dataTableOutput("fullDataset")  # Display full dataset as an interactive table
                             )
                           )
                  ),
                  tabPanel("Dataset Summary",
                           fluidRow(
                             column(12,
                                    h4("Detailed Dataset Summary"),
                                    verbatimTextOutput("dataSummary"),
                                    p("This dataset contains a wealth of information on air quality across various countries and cities. Below are key highlights from the summary:"),
                                    tags$ul(
                                      tags$li("Total Records: The dataset includes a comprehensive collection of air quality measurements, with each entry representing a unique observation."),
                                      tags$li("Pollutants Analyzed: Major pollutants include PM2.5, PM10, carbon monoxide (CO), sulfur dioxide (SO2), nitrogen dioxide (NO2), and ozone (O3). Each pollutant has varying effects on health, with PM2.5 being particularly harmful as it can penetrate deep into the lungs."),
                                      tags$li("Health Impacts: Poor air quality has been linked to respiratory diseases, cardiovascular issues, and other health complications. Understanding the distribution of these pollutants is crucial for public health initiatives."),
                                      tags$li("Temporal Analysis: The dataset's date range allows for the examination of air quality trends over time, enabling researchers to identify seasonal variations and the effectiveness of pollution control measures."),
                                      tags$li("Geographical Insights: By analyzing AQI values across different countries and cities, we can observe geographical disparities in air quality, helping to pinpoint areas in need of intervention.")
                                    ),
                                    p("By exploring these summary statistics, users can gain insights into global air quality trends and their implications for public health and policy.")
                             )
                           )
                  ),
                  tabPanel("AQI Categories",
                           fluidRow(
                             column(12,
                                    h4("Distribution of AQI Categories"),
                                    plotlyOutput("aqiCategoryPlot", height = "400px")  # Set height for plot
                             )
                           )
                  ),
                  tabPanel("Top 20 Cities by Average AQI",
                           fluidRow(
                             column(12,
                                    h4("Top 20 Cities with Highest Average AQI"),
                                    plotlyOutput("averageAQIPlot", height = "400px")  # Set height for plot
                             )
                           )
                  ),
                  tabPanel("Pollutant Relationships",
                           fluidRow(
                             column(12,
                                    h4("Scatter Plot Matrix of Key Pollutants"),
                                    plotlyOutput("scatterPlotMatrix", height = "600px")  # Set height for plot
                             )
                           )
                  )
      )
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Display the full dataset as an interactive data table
  output$fullDataset <- renderDataTable({
    datatable(data, options = list(pageLength = 25, autoWidth = TRUE))  # Adjust options as needed
  })
  
  # Enhanced dataset summary
  output$dataSummary <- renderPrint({
    overview <- data.frame(
      Total_Records = nrow(data),
      Missing_AQI_Values = sum(is.na(data$`AQI.Value`)),
      Unique_Countries = n_distinct(data$Country),
      Unique_Cities = n_distinct(data$City),
      Unique_AQI_Categories = n_distinct(data$`AQI.Category`),
      Earliest_Record = min(data$Date, na.rm = TRUE),
      Latest_Record = max(data$Date, na.rm = TRUE),
      Average_AQI_Value = round(mean(data$`AQI.Value`, na.rm = TRUE), 2),
      Median_AQI_Value = round(median(data$`AQI.Value`, na.rm = TRUE), 2),
      Standard_Deviation_AQI = round(sd(data$`AQI.Value`, na.rm = TRUE), 2)
    )
    print(overview)
  })
  
  # Reactive dataset based on selected country
  filtered_data <- reactive({
    req(input$country)  # Ensure country is selected
    data %>% filter(Country == input$country)
  })
  
  # Plot for AQI Categories using plotly
  output$aqiCategoryPlot <- renderPlotly({
    req(filtered_data())  # Ensure data is available
    aqi_counts <- filtered_data() %>%
      group_by(`AQI.Category`) %>%
      summarise(Count = n())
    
    p <- ggplot(aqi_counts, aes(x = `AQI.Category`, y = Count, fill = `AQI.Category`)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      ggtitle("Distribution of AQI Categories in Selected Country") +
      xlab("AQI Category") +
      ylab("Count") +
      scale_fill_manual(values = viridis(length(unique(aqi_counts$`AQI.Category`)))) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(hjust = 0.5))  # Center the title
    
    ggplotly(p)  # Convert ggplot to an interactive plotly plot
  })
  
  # Plot for Average AQI by Top 20 Cities using plotly
  output$averageAQIPlot <- renderPlotly({
    req(filtered_data())  # Ensure data is available
    city_aqi <- filtered_data() %>%
      group_by(City) %>%
      summarise(Average_AQI = mean(`AQI.Value`, na.rm = TRUE)) %>%
      arrange(desc(Average_AQI)) %>%
      head(20)  # Show top 20 cities
    
    p <- ggplot(city_aqi, aes(x = reorder(City, Average_AQI), y = Average_AQI, fill = Average_AQI)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis_c() +
      theme_minimal() +
      ggtitle("Top 20 Cities with Highest Average AQI") +
      xlab("City") +
      ylab("Average AQI Value") +
      theme(axis.text.y = element_text(size = 8)) +  # Adjust text size if necessary
      theme(plot.title = element_text(hjust = 0.5))  # Center the title
    
    ggplotly(p)  # Convert ggplot to an interactive plotly plot
  })
  
  # Create a scatter plot matrix of pollutants
  output$scatterPlotMatrix <- renderPlotly({
    req(filtered_data())  # Ensure data is available
    
    # Select relevant pollutants
    pollutants_data <- filtered_data() %>%
      select(`CO.AQI.Value`, `Ozone.AQI.Value`, `NO2.AQI.Value`, `PM2.5.AQI.Value`)
    
    # Create a scatter plot matrix using plotly
    plotly::plot_ly() %>%
      add_trace(type = 'scatter', mode = 'markers', 
                x = pollutants_data$`CO.AQI.Value`, 
                y = pollutants_data$`Ozone.AQI.Value`, 
                name = 'CO vs Ozone', 
                marker = list(color = 'red')) %>%
      add_trace(x = pollutants_data$`CO.AQI.Value`, 
                y = pollutants_data$`NO2.AQI.Value`, 
                name = 'CO vs NO2', 
                marker = list(color = 'blue')) %>%
      add_trace(x = pollutants_data$`CO.AQI.Value`, 
                y = pollutants_data$`PM2.5.AQI.Value`, 
                name = 'CO vs PM2.5', 
                marker = list(color = 'green')) %>%
      add_trace(x = pollutants_data$`Ozone.AQI.Value`, 
                y = pollutants_data$`NO2.AQI.Value`, 
                name = 'Ozone vs NO2', 
                marker = list(color = 'purple')) %>%
      add_trace(x = pollutants_data$`Ozone.AQI.Value`, 
                y = pollutants_data$`PM2.5.AQI.Value`, 
                name = 'Ozone vs PM2.5', 
                marker = list(color = 'orange')) %>%
      add_trace(x = pollutants_data$`NO2.AQI.Value`, 
                y = pollutants_data$`PM2.5.AQI.Value`, 
                name = 'NO2 vs PM2.5', 
                marker = list(color = 'cyan')) %>%
      layout(title = 'Scatter Plot Matrix of Pollutants',
             xaxis = list(title = 'Pollutants'),
             yaxis = list(title = 'Pollutants'))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# app.R
library(leaflet)
library(DT) # For displaying data tables
library(sf) # For handling spatial data
library(leaflet.extras) # For more mapping options
library(shiny)
library(tidyverse)
library(ggplot2)
library(usmap)



final_sfdc_lead <- read_csv("final_sfdc_lead.csv")

# final_sfdc_lead$MQL <- as.logical(final_sfdc_lead$MQL)
# final_sfdc_lead$SQL <- as.logical(final_sfdc_lead$SQL)
# final_sfdc_lead$CW <- as.logical(final_sfdc_lead$CW)
# final_sfdc_lead$Onboarded <- as.logical(final_sfdc_lead$Onboarded)

# UI
ui <- fluidPage(
  titlePanel("US State Heatmap based on Marketing Metrics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Choose a metric:",
        choices = c("MQL", "SQL", "CW", "Onboarded")
      ),
      hr(),
      helpText("Select a metric to display its heatmap across the US states.")
    ),
    mainPanel(
      plotOutput("stateHeatmap")
    )
  )
)

# Server logic
server <- function(input, output) {
  # Define reactive expression for filtered data
  filtered_data <- reactive({
    # Filter based on selected metric
    final_sfdc_lead %>%
      filter(!!sym(input$metric) == TRUE) %>%
      count(state) %>%
      rename(count = n)
  })
  
  # Render the heatmap plot
  output$stateHeatmap <- renderPlot({
    # Check if there is data to plot
    if (!is.null(filtered_data())) {
      plot_data <- plot_usmap(data = filtered_data(), values = "count") +
        scale_fill_continuous(low = "white", high = "red", name = "Number of Leads") +
        theme(legend.position = "right")
      print(plot_data)
    }
  })
}



# Run the application
shinyApp(ui = ui, server = server)

# Load necessary libraries
library(shiny)
library(tmap)
library(sf)
library(dplyr)
library(readr)
library(tigris)

# Load and prepare data
final_sfdc_lead <- read_csv("../data/final_sfdc_lead.csv")  # Update path as necessary
states_sf <- states(cb = TRUE, resolution = "20m") |> 
  st_as_sf()

# Define UI for application
ui <- fluidPage(
  titlePanel("US State Heatmap based on Marketing Metrics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Choose a metric:",
                  choices = c("MQL", "SQL", "CW", "Onboarded")),
      hr(),
      helpText("Select a metric to display its heatmap across the US states."),
      
      selectInput("channel", "Choose Channel:",
                  choices = c("All", "DM", "Wechat", "SMS", "Email", "Google", "SEO")),
      hr(),
      helpText("Pick a channel")
    ), #sidebar panel ends
    mainPanel(
      tmapOutput("stateHeatmap"),  # Changed from plotOutput to tmapOutput for interactive map
      #verbatimTextOutput("debugData")  # Only for debugging purposes
    ) # mainpanel ends
    
  ) #sidebarlayout ends

  
) # ui fluid page ends

# Define server logic required to draw a heatmap
server <- function(input, output) {
  
  filtered_data <- reactive({
    # Check if 'All' channels are selected or a specific channel; adjust if there's no 'All' option
    if (input$channel == "All") {
      df <- final_sfdc_lead %>% 
        filter(!!sym(input$metric) == TRUE) %>% 
        count(State) %>% 
        rename(count = n)
    } else {
      df <- final_sfdc_lead %>% 
        filter(!!sym(input$metric) == TRUE,
               str_detect(Lead_Channel_SFDC, input$channel)) %>% 
        count(State) %>% 
        rename(count = n)
    }
    return(df)
  })

  output$stateHeatmap <- renderTmap({
    # Check if data is available to avoid errors during initialization
    req(filtered_data())
    
    # Prepare the color palette based on the count data
    pal <- colorNumeric(palette = "-Reds", domain = filtered_data()$count, na.color = "transparent")
    
    # Merge filtered data with state geometries
    data_merged <- merge(states_sf, filtered_data(), by.x = 'STUSPS', by.y = 'State', all.x = TRUE)
    
    tmap_mode("view")
    
    tm <- tm_shape(data_merged) +
      tm_polygons(
        col = "count",
        palette = "-Reds",
        border.col = "black",
        border.alpha = 0.5,
        popup.vars = c("Numbers of Lead: " = "count")
      ) +
      tm_layout(
        legend.title.size = 1,
        legend.text.size = 0.8,
        legend.position = c("left", "bottom")
      ) +
      # Set default view to focus on the continental US
      tm_view(set.view = c(lon = -96.9, lat = 37.8, zoom = 4))  # Correct parameter names
    
    tm  # Render the map
    })
  
 
  
  # Debugging: print the first few rows of the filtered data
  # output$debugData <- renderPrint({
  #   if (!is.null(filtered_data())) {
  #     head(filtered_data())
  #   }
  # })
}

# Run the application
shinyApp(ui = ui, server = server)

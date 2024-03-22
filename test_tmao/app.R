# Load necessary libraries
library(shiny)
library(tmap)
library(sf)
library(dplyr)
library(readr)
library(tigris)
library(leaflet)
library(bslib)

# Load and prepare data
final_sfdc_lead <- read_csv("../data/final_sfdc_lead.csv")  # Update path as necessary
states_sf <- states(cb = TRUE, resolution = "20m") |> 
  st_as_sf()

# Define UI for application
ui <- navbarPage(  # Changed from fluidPage to navbarPage
  title = "State Penetration Dashboard",
  theme = bslib::bs_theme(bootswatch = "cerulean"),  # Optional: add a theme from bootstrap
  tabPanel("Map",
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
             ),
             mainPanel(
               tmapOutput("stateHeatmap")  # Your tmap output
             )
           )
  ),
  tabPanel("Trends",
           p("Second page content.")  # Place content for Trends here
  ),
  tabPanel("TBD",
           p("Third page content.")  # Place content for TBD here
  ),
  navbarMenu("Links",
             tabPanel("Posit", tags$iframe(style="height:600px;width:100%",src="https://posit.co")),
             tabPanel("Shiny", tags$iframe(style="height:600px;width:100%",src="https://shiny.posit.co"))
  )
)  # ui navbarPage ends

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
               Lead_Channel_SFDC == input$channel) %>% 
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
        popup.vars = c("Numbers of Lead: " = "count"),
        popup.format=list(count=list(digits=0))
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

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(tidyverse)

final_sfdc_lead <- read_csv("data/final_sfdc_lead.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$stateHeatmap <- renderPlot({
        # Dynamically create the filtering expression based on input
        metric <- sym(input$metric)  # Convert input to a symbol
        filter_expr <- !!metric == TRUE  # Turn the symbol into a filter expression
        
        # Filter and count by state
        filtered_data <- final_sfdc_lead %>%
            filter(!!metric) %>%  # Use the dynamic filter expression
            count(StateProvince_text_only_SFDC) %>%
            rename(state = StateProvince_text_only_SFDC, count = n)
        
        # Ensure filtered_data has 'state' column as factor for plotting
        filtered_data$state <- factor(filtered_data$state, levels = state.abb)
        
        # Plotting the heatmap
        plot_data <- ggplot(filtered_data, aes(fill = count, map_id = state)) +
            geom_map(map = fifty_states, colour = "white", size = 0.5) +
            expand_limits(x = fifty_states$long, y = fifty_states$lat) +
            scale_fill_continuous(low = "white", high = "red", name = paste(input$metric, "Count")) +
            labs(title = paste("Heatmap of", input$metric, "by State"), x = "", y = "") +
            theme_minimal() +
            theme(legend.position = "right")
        
        print(plot_data)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

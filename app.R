################################### loading packages ############################
# Load necessary libraries
library(shiny)
library(tmap)
library(sf)
library(dplyr)
library(readr)
library(tigris)
library(leaflet)
library(bslib)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(shinymanager)
library(keyring)


################################################################################

#source("data/salesforce_data_via_api.R")

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

#######
# data.frame with credentials info
credentials <- read_csv("pw/credentials.csv")






#################################################################################
sfdc_lead <- read_csv("data/sfdc_leads_us_state.csv")

lead_channel_list <- unique(sfdc_lead$Lead_Channel)

sfdc_opportunity <- read_csv("data/sfdc_opportunity.csv")


state <- unique(sfdc_lead$State)

states_sf <- states(cb = TRUE, resolution = "20m") |>
  st_as_sf()

restaurants <- read_csv("data/restaurants.csv")

################################# ui ##############################################

# Define UI for application
main_ui <- secure_app(
  head_auth = tags$script(inactivity),
  navbarPage(
    title = "State Penetration Dashboard",
    theme = bslib::bs_theme(bootswatch = "flatly"),

    ################################# map ##########################################
    tabPanel(
      "Map",
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            ############### sfdc leads map ##################
            tabPanel(
              "SFDC Leads",
              dateRangeInput("month_range_map",
                label = "Select Month Range",
                start = Sys.Date() - 30, # Default start date
                end = Sys.Date(), # Default end date
                min = "2022-01-01", # The earliest selectable date
                max = "2024-12-31", # The latest selectable date
                format = "yyyy-mm",
                startview = "year",
                language = "en",
                separator = " - "
              ), # date range
              selectInput("metric_map", "Metric:",
                choices = c("MEL", "MQL", "SQL", "CW", "Onboarded")
              ),
              helpText("Select a metric to display its heatmap across the US states."),
              hr(),
              pickerInput("channel_map", "Channel:",
                choices = lead_channel_list,
                multiple = TRUE, options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                ),
                selected = lead_channel_list
              )
            ),
            ############### existing clients ##################
            tabPanel(
              "Existing Client",
              helpText("Click the group to view"),
              radioButtons("clientMetrics", "Select Metric:",
                choices = list(
                  "Active Clients" = "activeclients",
                  "Suspended&Canceled Clients", "canceledclients",
                  "DE Clients" = "declients",
                  "SMS Clients" = "smsclients",
                  "Greendot Clients" = "greendotclients"
                )
              )
            )
          ),
          tags$br(),
          tableOutput("funnelsummary")
        ),
        mainPanel(
          fluidRow(
            column(6, tmapOutput("stateHeatmap")), # First map on the left
            column(6, tmapOutput("existingClientmap")) # Second map on the right
          ),
          tags$br(),
          dataTableOutput("stateConversionTable")
        ) # main panel
      )
    ),
    ################################### trend ###########################################
    tabPanel(
      "Trends",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput("monthRange",
            label = "Select Month Range",
            start = Sys.Date() - 30, # default start date
            end = Sys.Date(), # default end date
            min = "2022-01-01", # the earliest selectable date
            max = "2024-12-31", # the latest selectable date
            format = "yyyy-mm",
            startview = "year",
            language = "en",
            separator = " - "
          ), # date range
          pickerInput("state_trend", "States:",
            choices = state,
            multiple = TRUE, options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Deselect all",
              `select-all-text` = "Select all"
            ),
            selected = state
          ),
          pickerInput("metric_trend", "Metrics:",
            choices = c("MEL", "MQL", "SQL", "CW", "Onboarded"),
            multiple = TRUE, options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Deselect all",
              `select-all-text` = "Select all"
            ),
            selected = c("MEL", "MQL", "SQL", "CW", "Onboarded")
          ),
          pickerInput("channel_trend", "Channels:",
            choices = lead_channel_list,
            multiple = TRUE, options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Deselect all",
              `select-all-text` = "Select all"
            ),
            selected = lead_channel_list
          )
        ), # side bar panel ends
        mainPanel(
          plotOutput("trend_plot"), # Your tmap output
          tags$br(),
          dataTableOutput("conversionTable")
        ) # main
      ) # side bar layout
    ), # tab panel ends

    #################################### tbd ############################################
    tabPanel(
      "TBD",
      p("Third page content.") # not sure what to put in here
    ),


    #################################### links ########################################
    navbarMenu(
      "Links",
      tabPanel("Marketing Funnel Performance Tracker", tags$iframe(style = "height:600px;width:100%", src = "https://docs.google.com/spreadsheets/d/1YdoQ3ffIxyMW066WrISdyknUD11unx917G-JLZjkT44/edit?usp=sharing")),
      tabPanel("Shiny", tags$iframe(style = "height:600px;width:100%", src = "https://shiny.posit.co"))
    )
  )
) # ui navbarPage ends


###################################### server ######################################
main_server <- function(input, output, session) {
  result_auth <- secure_server(check_credentials = check_credentials(credentials))

  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })

  ############################## heat map ###########################

  filtered_data <- reactive({
    df <- sfdc_lead |>
      filter(
        Created_Date >= input$month_range_map[1] & Created_Date <= input$month_range_map[2],
        Lead_Channel %in% input$channel_map
      )
    return(df)
  })


  state_heat_map_count <- reactive({
    req(filtered_data())
    df <- filtered_data() |>
      filter(if (input$metric_map == "MEL") TRUE else !!sym(input$metric_map)) |>
      group_by(State) |>
      summarise(Total_Lead_Credit = sum(Lead_Credit, na.rm = TRUE))
  })

  

  output$stateHeatmap <- renderTmap({
    req(state_heat_map_count())

    pal <- colorNumeric(palette = "-Reds", domain = state_heat_map_count()$Total_Lead_Credit, na.color = "transparent")

    data_merged <- merge(states_sf, state_heat_map_count(), by.x = "STUSPS", by.y = "State", all.x = TRUE)

    tmap_mode("view")
    tm <- tm_shape(data_merged) +
      tm_polygons(
        col = "Total_Lead_Credit",
        title = "SFDC Leads",
        textNA = "No Leads",
        palette = "Blues",
        border.col = "black",
        border.alpha = 0.5,
        popup.vars = c("Count" = "Total_Lead_Credit"),
        popup.format = list(Total_Lead_Credit = list(digits = 0))
      ) +
      tm_layout(
        legend.title.size = 1,
        legend.text.size = 0.8,
        legend.position = c("left", "bottom"),
      ) +
      tm_view(  set.view = c(lon = -98.35, lat = 41, zoom = 2.7), 
                view.legend.position = c("left", "bottom"))

    tm
  })



  ######################## existingClientmap #####################

  existing_clients <- reactive({
    filters_map <- list(
      activeclients = function(data) data |> filter(`Restaurants Status` == "active"),
      canceledclients = function(data) data |> filter(`Restaurants Status` %in% c("suspended", "canceled")),
      declients = function(data) data |> filter(`Restaurants Is Delivery Platform Service Enabled` == TRUE),
      smsclients = function(data) data |> filter(`Restaurants Is SMS Opted In` == TRUE),
      greendotclients = function(data) data |> filter(`Restaurants Is GreenDot Enabled` == TRUE)
    )
    filter_func <- filters_map[[input$clientMetrics]]
    if (!is.null(filter_func)) {
      data <- filter_func(restaurants)
    } else {
      data <- restaurants
    }
    return(data)
  })
  
  existingclient_map_count <- reactive({
    req(existing_clients())
    df <- existing_clients() |>
      group_by(`Restaurants State`) |>
      summarise(Number_of_Clients = n(), .groups = 'drop')
  })
  
  output$existingClientmap <- renderTmap({
    req(existingclient_map_count())
    pal <- colorNumeric(palette = "-Reds", domain = existingclient_map_count()$Number_of_Clients, na.color = "transparent")
    data_merged_2 <- merge(states_sf, existingclient_map_count(), by.x = "STUSPS", by.y = "Restaurants State", all.x = TRUE)
    tmap_mode("view")
    tm <- tm_shape(data_merged_2) +
      tm_polygons(
        col = "Number_of_Clients",
        title = "Existing Clients",
        textNA = "No Leads",
        palette = "Reds",
        border.col = "black",
        border.alpha = 0.5,
        popup.vars = c("Count" = "Number_of_Clients"),
        popup.format = list(Number_of_Clients = list(digits = 0))
      ) +
      tm_layout(
        legend.title.size = 1,
        legend.text.size = 0.8,
        legend.position = c("left", "bottom"),
      ) +
      tm_view(  set.view = c(lon = -98.35, lat = 41, zoom = 2.7),
                view.legend.position = c("left", "bottom"))
    tm
  })
  

  ################################# conversation rate ########################################

  state_conversion_rates <- reactive({
    req(filtered_data())

    state_filtered <- filtered_data() %>%
      group_by(State) %>%
      summarise(
        MEL = n(),
        MQL = sum(MQL, na.rm = TRUE),
        SQL = sum(SQL, na.rm = TRUE),
        CW = sum(CW, na.rm = TRUE),
        Onboarded = sum(Onboarded, na.rm = TRUE)
      ) %>%
      mutate(
        "MEL→MQL" = (MQL / MEL) * 100,
        "MQL→SQL" = (SQL / MQL) * 100,
        "SQL→CW" = (CW / SQL) * 100,
        "SQL→Onboarded" = (Onboarded / SQL) * 100
      ) %>%
      mutate(across(
        c("MEL→MQL", "MQL→SQL", "SQL→CW", "SQL→Onboarded"),
        ~ ifelse(is.nan(.) | is.infinite(.), "0%", paste0(round(., 0), "%"))
      )) %>%
      select(State, MEL, MQL, "MEL→MQL", SQL, "MQL→SQL", CW, Onboarded, "SQL→CW", "SQL→Onboarded")



    return(state_filtered)
  })

  output$stateConversionTable <- DT::renderDT(
    {
      req(state_conversion_rates())
      state_conversion_rates()
    },
    options = list(pageLength = 10)
  ) # number of row shows


  summary_table <- reactive({
    req(filtered_data())
    
    state_filtered <- filtered_data() %>%
      summarise(
        MEL = n(),
        MQL = sum(MQL, na.rm = TRUE),
        SQL = sum(SQL, na.rm = TRUE),
        CW = sum(CW, na.rm = TRUE),
        Onboarded = sum(Onboarded, na.rm = TRUE))
    
    state_filtered_long <- pivot_longer(state_filtered, cols = everything(), names_to = "Metric", values_to = "Total")
    
      return(state_filtered_long)
    
  })
  
  output$funnelsummary <- renderTable({
    req(summary_table())
    summary_table()
  })


  ################################# plot - trend  ##########################################

  filtered_data_trend <- reactive({
    df <- sfdc_lead |>
      filter(
        Lead_Channel %in% input$channel_trend,
        Created_Date >= input$monthRange[1] & Created_Date <= input$monthRange[2],
        State %in% input$state_trend
      )
    print(df)
  })


  output$trend_plot <- renderPlot({
    if (is.null(input$metric_trend) || length(input$metric_trend) == 0) {
      # Display a custom error message
      return(ggplot() +
        annotate("text",
          x = 0.5, y = 0.5, label = "Please select at least one metric to display the trend.",
          size = 6, hjust = 0.5, vjust = 0.5
        ) +
        theme_void())
    }

    req(filtered_data_trend())


    metric_expressions <- lapply(input$metric_trend, function(metric) {
      # Check if the current metric is "MEL". If so, sum all Lead_Credit values.
      # Otherwise, proceed with the original logic.
      if (metric == "MEL") {
        expr(sum(Lead_Credit, na.rm = TRUE))
      } else {
        expr(sum(ifelse(!!sym(metric) == TRUE, Lead_Credit, 0), na.rm = TRUE))
      }
    })
    names(metric_expressions) <- paste0(input$metric_trend, "_Credit")



    if (!is.null(metric_expressions) && length(metric_expressions) > 0) {
      monthly_summary <- filtered_data_trend() %>%
        group_by(Month = floor_date(Created_Date, "week")) %>%
        summarise(!!!metric_expressions) %>%
        pivot_longer(
          cols = ends_with("_Credit"),
          names_to = "Stage",
          values_to = "Credit",
          names_prefix = "_Credit"
        )

      ######################### plot #################################
      trend_plot <- ggplot(monthly_summary, aes(x = Month, y = Credit, color = Stage)) +
        geom_line(size = 1) +
        labs(
          title = "Weekly Progression of Selected Metrics",
          x = "Week",
          y = "Count",
          color = "Metric"
        ) +
        theme_minimal() +
        theme(legend.position = "right")

      trend_plot
    }
  })

  #################################### conversion rate ######################################


  conversion_rates <- reactive({
    req(filtered_data_trend())
    # Ensure the main data is ready
    conversion_data <- filtered_data_trend() %>%
      group_by(State) %>%
      summarise(
        MEL = n(),
        MQL = sum(MQL, na.rm = TRUE),
        SQL = sum(SQL, na.rm = TRUE),
        CW = sum(CW, na.rm = TRUE),
        Onboarded = sum(Onboarded, na.rm = TRUE)
      ) %>%
      mutate(
        "MEL→MQL" = (MQL / MEL) * 100,
        "MQL→SQL" = (SQL / MQL) * 100,
        "SQL→CW" = (CW / SQL) * 100,
        "SQL→Onboarded" = (Onboarded / SQL) * 100
      ) %>%
      mutate(across(
        c("MEL→MQL", "MQL→SQL", "SQL→CW", "SQL→Onboarded"),
        ~ ifelse(is.nan(.) | is.infinite(.), "0%", paste0(round(., 0), "%"))
      )) %>%
      select(State, MEL, MQL, "MEL→MQL", SQL, "MQL→SQL", CW, Onboarded, "SQL→CW", "SQL→Onboarded")


    print(conversion_data)
  })

  output$conversionTable <- DT::renderDT(
    {
      req(conversion_rates()) # Ensure the conversion data is ready
      conversion_rates() # Render this data as a DataTable
    },
    options = list(pageLength = 10, searching = FALSE)
  )


  # Debugging: print the first few rows of the filtered data
  # output$debugData <- renderPrint({
  #   if (!is.null(filtered_data())) {
  #     head(filtered_data())
  #   }
  # })
}


shinyApp(ui = main_ui, server = main_server)

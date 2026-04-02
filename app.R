#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(dataRetrieval)
library(dplyr)
library(leaflet)
library(plotly)
library(blastula)
library(glue)

# 1. STATION & THRESHOLD METADATA
# Flood stages based on NWS/USGS data for Bogachiel and Calawah rivers
stations_df <- data.frame(
  name = c("Bogachiel River near La Push", "Bogachiel River near Forks", "Calawah River near Forks"),
  id = c("12043015", "12042800", "12043000"),
  lat = c(47.9029, 47.8944, 47.9601),
  lng = c(-124.5455, -124.3571, -124.3930),
  action = c(35.0, 35.0, 14.6), # Action stages (ft)
  flood = c(37.0, 37.0, 16.5),  # Flood stages (ft)
  stringsAsFactors = FALSE
)

# Weather station IDs for Forks and Quillayute Airports
weather_stations <- data.frame(
  name = c("Forks Airport (Precip)", "Quillayute Airport (Precip)"),
  id = c("475610124233001", "475647124330401"),
  stringsAsFactors = FALSE
)

# 2. USER INTERFACE
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Olympic River Monitor"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "main", icon = icon("dashboard")),
      h4("Station Selection", style = "margin-left: 15px;"),
      leafletOutput("stationMap", height = "200px"),
      selectInput("stations", "Rivers:", choices = stations_df$name, 
                  selected = stations_df$name, multiple = TRUE),
      checkboxGroupInput("weather", "Show Precipitation:", choices = weather_stations$name),
      dateRangeInput("dates", "Date Range:", start = Sys.Date() - 7, end = Sys.Date())
    )
  ),
  dashboardBody(
    fluidRow(
      # Status Boxes that update based on flood levels
      uiOutput("status_boxes")
    ),
    fluidRow(
      box(width = 12, plotlyOutput("combinedPlot", height = "500px"))
    )
  )
)

# 3. SERVER LOGIC
server <- function(input, output, session) {
  
  # Reactive flag to prevent email spamming
  alert_sent <- reactiveVal(FALSE)
  
  # Map Display
  output$stationMap <- renderLeaflet({
    leaflet(stations_df) %>%
      addTiles() %>%
      addCircleMarkers(~lng, ~lat, layerId = ~name, label = ~name, 
                       color = "#2c7fb8", fillOpacity = 0.8, radius = 8)
  })
  
  # Sync Map Click to Dropdown
  observeEvent(input$stationMap_marker_click, {
    new_selection <- unique(c(input$stations, input$stationMap_marker_click$id))
    updateSelectInput(session, "stations", selected = new_selection)
  })
  
  # Fetch River Data (Stage Height)
  river_data <- reactive({
    req(input$stations, input$dates)
    ids <- stations_df$id[stations_df$name %in% input$stations]
    
    # Corrected indexing for dates to fix 'length=2' error
    df <- readNWISuv(siteNumbers = ids, parameterCd = "00065",
                     startDate = input$dates[1], endDate = input$dates[2])
    if(nrow(df) == 0) return(NULL)
    
    renameNWISColumns(df) %>% left_join(stations_df, by = c("site_no" = "id"))
  })
  
  # Fetch Precip Data
  precip_data <- reactive({
    req(input$weather)
    ids <- weather_stations$id[weather_stations$name %in% input$weather]
    df <- readNWISuv(siteNumbers = ids, parameterCd = "00045",
                     startDate = input$dates[1], endDate = input$dates[2])
    if(nrow(df) == 0) return(NULL)
    renameNWISColumns(df) %>% left_join(weather_stations, by = c("site_no" = "id"))
  })
  
  # Status Boxes logic
  output$status_boxes <- renderUI({
    df <- river_data()
    req(df)
    
    latest <- df %>% group_by(name) %>% slice_max(dateTime, n = 1)
    
    # Create a box for each selected station
    lapply(1:nrow(latest), function(i) {
      val <- latest$GH_Inst[i]
      flood_lvl <- latest$flood[i]
      action_lvl <- latest$action[i]
      
      # Determine Color
      box_color <- if(val >= flood_lvl) "red" else if(val >= action_lvl) "yellow" else "aqua"
      status_text <- if(val >= flood_lvl) "FLOOD" else if(val >= action_lvl) "ACTION" else "NORMAL"
      
      valueBox(
        value = paste(val, "ft"),
        subtitle = paste(latest$name[i], "|", status_text),
        icon = icon("water"),
        color = box_color,
        width = 4
      )
    })
  })
  
  # Combined Plotly Chart
  output$combinedPlot <- renderPlotly({
    df_r <- river_data()
    df_p <- precip_data()
    req(df_r)
    
    p <- plot_ly() %>%
      add_lines(data = df_r, x = ~dateTime, y = ~GH_Inst, color = ~name, name = ~name)
    
    # Add Flood Stage Lines
    # 1. Define styling vectors (Add more if adding more stations)
    red_shades <- c("#FF0000", "#B22222", "#8B0000") # Bright Red, Firebrick, Dark Red
    dash_styles <- c("dash", "dot", "dashdot")
    
    # 2. Add Station-Specific Threshold Lines (Grouped with Unique Styles)
    selected_thresh <- stations_df %>% filter(name %in% input$stations)
    
    if(nrow(selected_thresh) > 0) {
      for(i in 1:nrow(selected_thresh)) {
        p <- p %>% 
          add_segments(
            x = min(df_r$dateTime), xend = max(df_r$dateTime),
            y = selected_thresh$flood[i], yend = selected_thresh$flood[i],
            # Legend Label
            name = paste("-", selected_thresh$name[i]),
            # Grouping
            legendgroup = "Flood Thresholds",
            legendgrouptitle = list(text = "<b>Flood Thresholds</b>"),
            # UNIQUE STYLING: Apply specific shade and dash style based on index
            line = list(
              color = red_shades[i], 
              dash = dash_styles[i], 
              width = 2
            ),
            showlegend = TRUE, 
            inherit = FALSE
          )
      }
    }
    
    # Add Precip on secondary axis
    if(!is.null(df_p)) {
      p <- p %>% add_bars(data = df_p, x = ~dateTime, y = ~Precip_Inst, 
                          name = "Precip", yaxis = "y2", marker = list(color = 'rgba(0,0,255,0.2)'))
    }
    
    p %>% layout(
      yaxis = list(title = "Stage (ft)"),
      yaxis2 = list(title = "Rain (in)", overlaying = "y", side = "right", autorange = "reversed"),
      hovermode = "x unified"
    )
  })
  
  # Email Alert Logic
  observe({
    df <- river_data()
    req(df)
    latest <- df %>% group_by(name) %>% slice_max(dateTime, n = 1) %>% ungroup()
    
    for(i in 1:nrow(latest)) {
      if(latest$GH_Inst[i] >= latest$flood[i] && !alert_sent()) {
        # SMTP Alert (Requires blastula credentials key 'river_gmail')
        # smtp_send(email = compose_email(body = "Flood Alert!"), 
        #           to = "user@example.com", from = "alert@river.com", credentials = creds_key("river_gmail"))
        alert_sent(TRUE)
        showNotification(paste("EMERGENCY: Flood Stage at", latest$name[i]), type = "error", duration = NULL)
      }
    }
  })
}

shinyApp(ui, server)


# To export for local development and testing, run the following command in the R console:
## shinylive::export("./", "shinylive_export", wasm_packages = FALSE)
# Note: The 'wasm_packages = FALSE' argument is used to exclude the large WebAssembly files from the export
# The actual deployment of this app is handled with GitHub Actions, which runs a similar export command
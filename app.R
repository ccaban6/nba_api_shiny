# app.R

library(shiny)
library(plotly)
library(htmlwidgets)

source("loaddata.R")
# Source the CourtPlotter class
source("CourtPlotter.R")


# UI
ui <- fluidPage(
  tags$style(
    HTML(".sidebar {
            position: absolute;
            top: 0;
            bottom: 0;
            left: 0;
            width: 400px;
            padding: 20px;
            overflow-y: auto;
            border-right: 1px solid #ddd;
            z-index: 1000; /* Ensure sidebar stays on top */
          }
          .navbar {
            z-index: 1000; /* Ensure navbar stays on top */
          }")
  ),
  navbarPage("NBA Shot Tracker", 
             tabPanel("Shot Visualizer", fluid = TRUE),
             sidebarLayout(
               sidebarPanel(
                 titlePanel("NBA Player Shot Chart"),
                 fluidRow(
                   column(12, 
                          selectInput(inputId = "TeamFinder",
                                      label = "Select Team",
                                      choices = teams,
                                      selected = "Lakers"
                          )
                   ),
                   column(12,
                          selectInput(inputId = "PlayerFinder",
                                      label = "Select Player",
                                      choices = distinct(filter(player_box, team_name == "Lakers"), athlete_display_name),
                                      selected = "LeBron James"
                          )
                   )
                 )
               ),
               mainPanel(
                plotlyOutput("basketball_court_plot")
                 
               )
             )
  )
)



# Server
server <- function(input, output, session) {
  if (!exists("nba_php")) {
    initiateTables()
  }
  
  player_chosen <- reactive({
    req(input$PlayerFinder)
    player_chosen <- nba_pbp %>%
      filter(grepl(paste0(input$PlayerFinder, "\\s+makes|", input$PlayerFinder, "\\s+misses"), text))
    return(player_chosen)
  })
  

  output$basketball_court_plot <- renderPlotly({
    # Generate basketball court plot
    basketball_court_plot <- generate_basketball_court()
    player_data <- player_chosen()
    
    View(player_data)
    
    # Add NBA player's markers to the plot
    basketball_court_plot <- basketball_court_plot %>%
      add_markers(data = player_data, 
                  x = ~coordinate_x, 
                  y = ~coordinate_y, 
                  z = 0.75,
                  size = 2, 
                  color = I("red"))  %>%
      onRender("
        function(el) { 
          el.on('plotly_hover', function(d) { 
            if(d.points[0].curveNumber == 7){
              // Change the color of the hovered marker
              Plotly.restyle(el, {marker: {color: 'blue'}}, [d.points[0].pointNumber]);
            }
          });
        }
      ")
    
    return (basketball_court_plot)
  })
}

  

#  
#  hover_event <- reactive({
#    event_data(event = "plotly_hover", source = "hoverplotsource")
#  })
#  
#  unhover_event <- reactive({
#    event_data(event = "plotly_unhover", source = "hoverplotsource")
#  })
#  
#  hoverplotlyProxy <- plotlyProxy("hoverplot", session)
#  
#  observeEvent(hover_event(), {
#    # Get the coordinates of the hovered point
#    x_hover <- hover_event()$x
#    y_hover <- hover_event()$y
#    z_hover <- hover_event()$z  # Assuming you have the z-coordinate of the hovered point
#    
#    # Get the coordinates of the rim
#    rim_x <- rim_center_x  # Replace with actual x-coordinate of the rim
#    rim_y <- rim_center_y  # Replace with actual y-coordinate of the rim
#    rim_z <- rim_center_z  # Replace with actual z-coordinate of the rim
#    
#    # Create data frame for the line trace
#    line_data <- data.frame(
#      x = c(x_hover, rim_x),
#      y = c(y_hover, rim_y),
#      z = c(z_hover, rim_z)
#    )
#    
#    # Add the line trace to the plot
#    hoverplotlyProxy %>%
#      plotlyProxyInvoke("addTraces", list(line_data))
#  })



# Run the application
shinyApp(ui = ui, server = server)

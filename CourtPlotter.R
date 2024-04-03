# CourtPlotter.R
library(shiny)
library(shinyjs)

library(plotly)

  generate_circle <- function(radius, center_x = 0, center_y = 0, center_z = 0, num_points = 200) {
    theta <- seq(0, 2 * pi, length.out = num_points)
    x_circle <- center_x + radius * cos(theta)
    y_circle <- center_y + radius * sin(theta)
    z_circle <- rep(center_z, length(x_circle))
    return(list(x = x_circle, y = y_circle, z = z_circle))
  }
  
  generate_trail <- function(shot_x, shot_y, basket_center_x, basket_center_y) {
    trail_x <- c(shot_x, basket_center_x)
    trail_y <- c(shot_y, basket_center_y)
    return(list(x = trail_x, y = trail_y))
  }
  
  initiateTables <- function() {
    tictoc::tic()
    progressr::with_progress({
      nba_pbp <- hoopR::load_nba_pbp()
    })
    tictoc::toc()
    lebron <- nba_pbp |>
      filter(grepl("LeBron James makes|LeBron James misses", text))
  }
  
  generate_basketball_court <- function() {
    
  # Define court dimensions
  length_court <- 94
  width_court <- 50
  
  # Create court lines
  x_lines <- c(-length_court/2, -length_court/2, length_court/2, length_court/2, -length_court/2)
  y_lines <- c(-width_court/2, width_court/2, width_court/2, -width_court/2,    
               -width_court/2)
  z_lines <- rep(0, 5)
  
  # Define 3-point lines
  theta_3pt <- seq(0, 2 * pi, length.out = 200)
  x_3pt_line <- length_court/2 + (23.75) * cos(theta_3pt)
  y_3pt_line <- (23.75) * sin(theta_3pt)
  z_3pt_line <- rep(0, length(x_3pt_line))
  
  # Backboard dimensions
  x_backboard_right <- rep(length_court/2 - 4, 5)
  y_backboard_right <- c(3,-3,-3, 3, 3)
  z_backboard_right <- c(9.5, 9.5, 13, 13, 9.5)
  
  # Define full-court circle
  half_court_outer <- generate_circle(radius =  6)
  half_court_inner <- generate_circle(radius = 2)
  
  # Define rim
  rim <- generate_circle(radius = .75, center_x = length_court/2 - 4.75, center_z = 10)
  
  basketball_court <- plot_ly() %>%
    
    # Creates court borders
    add_trace(type = 'scatter3d', x = x_lines, y = y_lines, z = z_lines, 
              mode = 'lines', line = list(color = 'black'), surfaceaxis = 2, surfacecolor = "#dfbb85", hoverinfo = 'skip') %>%
    
    # Creates back boards
    add_trace(type = 'scatter3d', x = x_backboard_right, y = y_backboard_right, z = z_backboard_right, 
              mode = 'lines', line = list(color = 'black', hoverinfo = 'none')) %>%
    add_trace(type = 'scatter3d', x = x_backboard_right * -1, y = y_backboard_right, z = z_backboard_right, 
              mode = 'lines', line = list(color = 'black', hoverinfo = 'none')) %>%
    # Creates rims
    add_trace(type = 'scatter3d', x = rim$x, y = rim$y, z = rim$z, 
              mode = 'lines', line = list(color = 'orange', hoverinfo = 'none')) %>%
    add_trace(type = 'scatter3d', x = rim$x * -1, y = rim$y, z = rim$z, 
              mode = 'lines', line = list(color = 'orange', hoverinfo = 'none')) %>%
    
    
    # Creates half court circles
    add_trace(type = 'scatter3d', x = half_court_outer$x, y = half_court_outer$y, z = half_court_outer$z,
              mode = 'lines', line = list(color = 'black', hoverinfo = 'none')) %>%
    add_trace(type = 'scatter3d', x = half_court_inner$x, y = half_court_inner$y, z = half_court_inner$z,
              mode = 'lines', line = list(color = 'black', hoverinfo = 'none'))
  
  basketball_court <- basketball_court %>%
    layout(scene = list(
      aspectmode = "manual",
      aspectratio = list(x = 1.88, y = 1, z = 0.5),
      xaxis = list(range = c(-length_court/2 - 8, length_court/2 + 8), 
                   showgrid = FALSE, showline = FALSE, zerolinewidth = 0),
      yaxis = list(range = c(-width_court/2 - 5, width_court/2 + 5), 
                   showgrid = FALSE, showline = FALSE, zerolinewidth = 0),
      zaxis = list(range = c(0, 30), showgrid = FALSE, showline = FALSE, zerolinecolor = '#ffff')
    )
    )
  
  # Disable hover for each trace
  return(basketball_court)
  }
  
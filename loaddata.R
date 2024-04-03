library(tidyverse)
library(nbastatR)
library(tictoc)

tictoc::tic()
progressr::with_progress({
  nba_pbp <- hoopR::load_nba_pbp()
  player_box <- hoopR::load_nba_player_box()
})
tictoc::toc()

teams <- player_box |> 
  distinct(team_name) |> filter(team_name != "All-Stars") |>
  arrange(team_name)

players <- player_box |>
  distinct(athlete_display_name)
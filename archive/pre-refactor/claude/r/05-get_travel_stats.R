library(tidyverse)
library(arrow)

source('claude/r/helpers_optimize.R') # get_travel_stats()

incident_travel_matrix_df <- read_parquet(
  'datasets/outputs/incident_travel_matrix_fdz.parquet'
)

travel_stats_df <- get_travel_stats(incident_travel_matrix_df)

write_parquet(travel_stats_df, 'datasets/outputs/travel_stats.parquet')

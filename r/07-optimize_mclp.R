library(tidyverse)
library(arrow)

source('r/helpers_optimize.R') # solve_mclp()

# source data for the coverage matrix and demand weights
travel_stats_df <- read_parquet('datasets/outputs/travel_stats.parquet')

mclp_result <- solve_mclp(travel_stats_df, p = 10, threshold_minutes = 6)

mclp_result$status
mclp_result$covered_demand
mclp_result$pct_covered
mclp_result$open_stations
mclp_result$coverage

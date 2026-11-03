# LEGACY — quarantined. Not owner-vetted as a whole. Do not build new code on this; copy vetted pieces into the new structure.
library(tidyverse)
library(arrow)

source('legacy/R/helpers_optimize.R') # solve_p_median()

# source data for the cost matrix and demand weights
travel_stats_df <- read_parquet('datasets/outputs/travel_stats.parquet')

p_median_result <- solve_p_median(travel_stats_df, p = 10)

p_median_result$status
p_median_result$objective
p_median_result$open_stations
p_median_result$assignments

library(tidyverse)
library(arrow)

incident_travel_matrix_df <- read_parquet(
  'datasets/outputs/incident_travel_matrix_fdz.parquet'
)



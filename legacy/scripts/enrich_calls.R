# LEGACY — quarantined. Not owner-vetted as a whole. Do not build new code on this; copy vetted pieces into the new structure.
library(tidyverse)
library(arrow)

incident_travel_matrix_df <- read_parquet(
  'datasets/outputs/incident_travel_matrix_fdz.parquet'
)



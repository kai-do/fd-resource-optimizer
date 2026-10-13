library(tidyverse)

# load in runcard in the format output by cad_run_card.sql
runcard_df <- read_csv(
  'datasets/2026-10-13_cad_run_card.csv',
  na = 'NULL',
  col_types = cols(
    location_id = col_integer(),
    location_type = col_factor(levels = c('Beat', 'Location', 'Area')),
    agency = col_factor(levels = c('CF', 'MF', 'AF')),
    response_sequence = col_integer(),
    responding_station_id = col_integer(),
    responding_battalion_id = col_integer()
  )
)

# filter runcard and adjust the response sequences based on selected stations, agencies, and location types
filter_runcard <- function(
  runcard_df,
  selected_stations = NULL,
  included_agencies = c('CF'),
  included_response_sequences = 1,
  included_location_types = c('Beat')
) {
  runcard_df %>%
    filter(
      is.null(selected_stations) | responding_station_id %in% selected_stations,
      agency %in% included_agencies,
      location_type %in% included_location_types
    ) %>%
    group_by(location_id) %>%
    arrange(location_id, response_sequence) %>%
    mutate(adjusted_response_sequence = as.integer(row_number())) %>%
    ungroup() %>%
    filter(adjusted_response_sequence %in% included_response_sequences)
}

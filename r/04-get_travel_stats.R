library(data.table)

incident_travel_matrix_df <- fread('datasets/outputs/incident_travel_matrix.csv', data.table = FALSE)



get_travel_stats <- function(travel_matrix_df, incident_address) {
  groups <- list()

}
# Helpers for the deployment analysis transition tables.

# Dispatch codes repeat across case types, so prefix the case type to make
# them unique: F for Fire, M for Medical.
make_dispatch_key <- function(case_type, code) {
  case_when(
    code == "None" ~ "None",
    case_type == "Fire" ~ paste0("F", code),
    case_type == "Medical" ~ paste0("M", code),
    TRUE ~ code
  )
}

# A few codes carry more than one description over the years (for example
# EAMBU is spelled two ways), so keep the most common row for each state.
most_common <- function(df) {
  df %>%
    count(across(everything())) %>%
    group_by(code) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-n)
}

# Count incidents moving from one stage's state to the next stage's state.
count_transition <- function(df, from, to, phase_id, from_stage, to_stage) {
  df %>%
    count(from = {{ from }}, to = {{ to }}) %>%
    mutate(phase_id = phase_id, from_stage = from_stage, to_stage = to_stage)
}

add_percent <- function(df) {
  df %>%
    group_by(phase_id) %>%
    mutate(percent = n / sum(n)) %>%
    ungroup() %>%
    select(phase_id, from_stage, from, to_stage, to, n, percent) %>%
    arrange(phase_id, desc(percent))
}

# Stack a copy of df with a combined level ("Both" / "All") so a single
# group_by yields the individual groups and the overall group together.
add_overall <- function(df, col, label) {
  bind_rows(
    df %>% mutate("{col}" := as.character(.data[[col]])),
    df %>% mutate("{col}" := label)
  )
}

# Counts and time stats by unit type, split by whether the unit arrived.
# Returns a list: `counts` (units, by unit_type x arrived) and `time_stats`
# (mean, median, sd, p90 for each time measure, same grouping). unit_type gets
# an "All" row and unit_arrived_flag gets a "Both" row.
get_apparatus_stats <- function(unit_response_df,
                                time_cols = c("turnout_time_sec", "travel_time_sec",
                                              "on_scene_time_sec", "response_time_sec",
                                              "commit_time_sec")) {
  df <- unit_response_df %>%
    mutate(unit_arrived_flag = as.character(unit_arrived_flag)) %>%
    add_overall("unit_arrived_flag", "Both") %>%
    add_overall("unit_type", "All")

  counts <- df %>%
    count(unit_type, unit_arrived_flag, name = "n_units") %>%
    arrange(unit_type != "All", unit_type, unit_arrived_flag)

  time_stats <- df %>%
    pivot_longer(all_of(time_cols), names_to = "measure", values_to = "seconds") %>%
    drop_na(seconds) %>%
    group_by(unit_type, unit_arrived_flag, measure) %>%
    summarise(
      n = n(),
      mean = mean(seconds),
      median = median(seconds),
      sd = sd(seconds),
      p90 = quantile(seconds, 0.9, names = FALSE),
      .groups = "drop"
    ) %>%
    arrange(unit_type != "All", unit_type, unit_arrived_flag, measure)

  list(counts = counts, time_stats = time_stats)
}

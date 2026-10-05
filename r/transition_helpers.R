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
    group_by(state) %>%
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

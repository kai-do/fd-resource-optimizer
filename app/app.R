# Install anything missing, including what r/helpers_optimize.R loads
required_packages <- c(
  'shiny', 'bslib', 'leaflet', 'DT', 'tidyverse', 'arrow', 'sf', 'htmltools', 'scales',
  'ompr', 'ompr.roi', 'ROI.plugin.glpk'
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) install.packages(missing_packages)

library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(tidyverse)
library(arrow)
library(sf)

# Run from the repo root with shiny::runApp('app'); paths resolve either way
root <- if (dir.exists('datasets')) '.' else '..'
source(file.path(root, 'r/helpers_optimize.R'))

# One multi-select per filter. Columns are on the travel matrix, selections are combined (AND).
# Descriptions for category and subcategory already start with the code, so they are not prefixed again.
filter_defs <- tribble(
  ~key, ~label, ~col, ~prefix_code,
  'rms_category', 'Incident Category', 'incident_category_code', FALSE,
  'rms_subcategory', 'Incident Subcategory', 'incident_subcategory_code', FALSE,
  'rms_type', 'RMS Incident Type', 'rms_incident_type_code', TRUE,
  'cad_type', 'CAD Incident Type', 'cad_incident_type_code', TRUE,
  'proqa_case_type', 'ProQA Case Type', 'dispatch_case_type_description', FALSE,
  'ems_capability', 'EMS Unit Capability', 'ems_unit_capability', FALSE,
  'dispatch_category', 'Dispatch Category', 'dispatch_category_code', TRUE,
  'dispatch_code', 'Dispatch Code', 'dispatch_code', TRUE
)
cost_choices <- c(
  'Mean Minutes' = 'mean_minutes',
  'Median Minutes' = 'median_minutes',
  '90th Percentile Minutes' = 'p90_minutes'
)

# everything the app needs is read here, before it starts, and shared across sessions
# factors elsewhere, fdz is joined to polygons and run cards as text
travel_matrix_df <- read_parquet(file.path(root, 'app/data/travel_matrix_app.parquet')) |>
  mutate(fdz = as.character(fdz))

# values for the filter dropdowns, written by r/08-prep_app_data.R so the matrix is not needed to build them
filter_values <- readRDS(file.path(root, 'app/data/filter_values.rds'))
dispatch_capability_df <- filter_values$dispatch_capability
fdz_polygons_sf <- readRDS(file.path(root, 'app/data/fdz_polygons.rds'))
stations_df <- read_parquet(file.path(root, 'app/data/stations.parquet'))
runcard_df <- read_parquet(file.path(root, 'app/data/runcard_beats.parquet'))
fdz_agency_df <- runcard_df |> distinct(fdz, agency)
agency_choices <- sort(unique(fdz_agency_df$agency))

# date limits and clock times for the date and time of day filters, on local time like filter_incidents()
alarm_date_limits <- as_date(range(with_tz(travel_matrix_df$incident_alarm_dt, 'America/New_York'), na.rm = TRUE))
time_minutes <- seq(0, 1440, by = 30)
time_choices <- set_names(time_minutes, sprintf('%02d:%02d', time_minutes %/% 60, time_minutes %% 60))

source_system_choices <- filter_values$source_system
station_choices <- set_names(stations_df$source_id, paste('Station', stations_df$source_id))

# choices are named with their description so the dropdown shows and searches it
code_lookup_df <- read_parquet(file.path(root, 'app/data/code_lookup.parquet')) |>
  bind_rows(tibble(
    filter_by = 'ems_capability',
    code = c('PARAMEDIC', 'AEMT', 'EMT-I / BLS', 'AMBULANCE ONLY', 'OTHER'),
    description = c('Paramedic', 'AEMT', 'EMT-I / BLS', 'Ambulance Only', 'Other (No EMS Capability Listed)')
  ))
code_choices <- pmap(filter_defs, \(key, label, col, prefix_code) {
  codes <- filter_values$columns[[col]]
  desc <- code_lookup_df |>
    filter(filter_by == key) |>
    with(description[match(codes, code)])
  shown <- if (prefix_code) paste(codes, desc, sep = ' - ') else desc
  set_names(codes, if_else(is.na(desc), codes, shown))
}) |>
  set_names(filter_defs$key)

# fills in a smaller summary row so the map and table get most of the page
value_box_css <- '
  .bslib-value-box .value-box-title { font-size: 0.8rem; }
  .bslib-value-box .value-box-value { font-size: 1.25rem; }
  .bslib-value-box .value-box-area { padding: 0.35rem 0.75rem; }
'

# hover text for a zone: who responds and all of the travel stats for that station and zone
hover_label <- function(zones_df, headline) {
  lines <- paste0(
    '<b>FDZ ', zones_df$fdz, ': ', headline, '</b><br>',
    'Incidents: ', format(zones_df$n_incidents, big.mark = ','), '<br>',
    'Mean Minutes: ', round(zones_df$mean_minutes, 2), '<br>',
    'Median Minutes: ', round(zones_df$median_minutes, 2), '<br>',
    '90th Percentile Minutes: ', round(zones_df$p90_minutes, 2), '<br>',
    'Mean Distance (Miles): ', round(zones_df$mean_distance_m / 1609.344, 2)
  )
  lapply(lines, htmltools::HTML)
}

ui <- page_sidebar(
  title = 'Fire Station Optimizer',
  fillable = TRUE,
  tags$head(tags$style(value_box_css)),
  sidebar = sidebar(
    width = 340,
    h5('1. Travel Stats'),
    selectizeInput(
      'source_system', 'Source System',
      choices = source_system_choices, selected = source_system_choices, multiple = TRUE
    ),
    map2(filter_defs$key, filter_defs$label, \(key, label) {
      selectizeInput(
        paste0('f_', key), label, choices = NULL, multiple = TRUE,
        options = list(placeholder = 'All')
      )
    }) |> tagList(),
    selectizeInput(
      'agency', 'FDZ Agency', choices = agency_choices, selected = 'CF', multiple = TRUE,
      options = list(placeholder = 'All')
    ),
    dateRangeInput(
      'dates', 'Incident Dates',
      start = alarm_date_limits[1], end = alarm_date_limits[2],
      min = alarm_date_limits[1], max = alarm_date_limits[2]
    ),
    # a start later than the end wraps past midnight, e.g. 22:00 to 06:00
    layout_columns(
      col_widths = c(6, 6), gap = '0.5rem',
      selectInput('time_start', 'Start Time', time_choices, selected = 0),
      selectInput('time_end', 'End Time', time_choices, selected = 1440)
    ),
    actionButton('build', 'Build Travel Stats', class = 'btn-secondary w-100'),
    textOutput('stats_note'),
    hr(),
    h5('2. Optimization'),
    radioButtons(
      'method', 'Method',
      c('P-Median (Minimize Travel Time)' = 'p_median', 'MCLP (Maximize Coverage)' = 'mclp')
    ),
    numericInput('p', 'Number of Stations (p)', value = 10, min = 1, step = 1),
    selectInput('cost_col', 'Travel Time Statistic', cost_choices),
    conditionalPanel(
      "input.method == 'mclp'",
      numericInput('threshold', 'Coverage Threshold (Minutes)', value = 6, min = 0, step = 0.5)
    ),
    selectizeInput(
      'include', 'Required Stations', choices = station_choices, multiple = TRUE,
      options = list(placeholder = 'None')
    ),
    checkboxInput('include_baseline', 'Include Baseline (Required Stations Only)', value = FALSE),
    selectizeInput(
      'exclude', 'Excluded Stations', choices = station_choices, multiple = TRUE,
      options = list(placeholder = 'None')
    ),
    checkboxInput('use_runcard', 'Use Run Card Order', value = TRUE),
    numericInput('n_alts', 'Number of Alternatives', value = 1, min = 1, max = 10, step = 1),
    actionButton('run', 'Run', class = 'btn-primary w-100'),
    conditionalPanel(
      'output.has_result',
      selectInput('alt', 'Viewing Alternative', choices = NULL)
    )
  ),
  div(style = 'flex: 0 0 auto;', uiOutput('metrics')),
  navset_card_tab(
    full_screen = TRUE,
    nav_panel('Map', leafletOutput('map', height = '100%')),
    nav_panel('Alternatives', DTOutput('alts_table')),
    nav_panel(
      'Assignments',
      downloadButton('download', 'Download CSV', class = 'btn-sm mb-2'),
      DTOutput('table')
    ),
    nav_panel(
      'Run Card',
      downloadButton('download_runcard', 'Download CSV', class = 'btn-sm mb-2'),
      DTOutput('runcard_table')
    ),
    nav_panel(
      'Response Stats',
      div(
        style = 'overflow-y: auto;',
        p(
          class = 'text-muted small mb-2',
          "Responding stations follow the CAD run card order for each scenario's open stations.",
          "They do not use the optimizer's assignments."
        ),
        layout_columns(
          col_widths = c(3, 3, 3, 3), fill = FALSE,
          selectInput('rs_left', 'Left Scenario', choices = NULL),
          selectInput('rs_right', 'Right Scenario', choices = NULL),
          selectInput('rs_level', 'Response Level', choices = NULL),
          div(class = 'pt-4', actionButton('rs_ci', 'Compute 95% CIs', class = 'btn-sm btn-secondary'))
        ),
        uiOutput('rs_note'),
        DTOutput('rs_summary'),
        uiOutput('rs_unserved'),
        downloadButton('download_rs', 'Download CSV', class = 'btn-sm my-2'),
        DTOutput('rs_fdz_table')
      )
    )
  )
)

server <- function(input, output, session) {
  # long code lists are served from the server side
  for (key in filter_defs$key) {
    updateSelectizeInput(session, paste0('f_', key), choices = code_choices[[key]], server = TRUE)
  }

  # dispatch codes follow the selected EMS unit capabilities
  observeEvent(input$f_ems_capability, ignoreNULL = FALSE, {
    codes <- dispatch_capability_df$dispatch_code[dispatch_capability_df$ems_unit_capability %in% input$f_ems_capability]
    choices <- code_choices$dispatch_code
    if (length(input$f_ems_capability) > 0) choices <- choices[choices %in% codes]
    updateSelectizeInput(
      session, 'f_dispatch_code', choices = choices,
      selected = intersect(input$f_dispatch_code, choices), server = TRUE
    )
  })

  # selections that apply right now, empty selections mean no filter
  current_filters <- function() {
    filters <- map(filter_defs$key, \(key) input[[paste0('f_', key)]]) |>
      set_names(filter_defs$key)
    # selecting every source system is the same as not filtering
    if (!setequal(input$source_system, source_system_choices)) {
      filters$source_system <- input$source_system
    }
    filters
  }

  # matching travel matrix rows (every station for each incident), rebuilt only when the button is pressed (all incidents on startup).
  # Kept at incident level so Response Stats can take medians and percentiles over calls.
  filtered_incidents <- eventReactive(
    input$build,
    {
      withProgress(message = 'Filtering Incidents', value = 0.3, {
        incidents <- filter_incidents(
          travel_matrix_df,
          filters = current_filters(),
          date_range = input$dates,
          time_range = as.numeric(c(input$time_start, input$time_end))
        )
        # zones are limited to the selected agencies, so runs with and without the run card use the same zones
        if (length(input$agency) > 0) {
          agency_fdzs <- fdz_agency_df$fdz[fdz_agency_df$agency %in% input$agency]
          incidents <- incidents |> filter(is.na(fdz) | fdz %in% agency_fdzs)
        }
        incidents
      })
    },
    ignoreNULL = FALSE
  )

  travel_stats <- reactive({
    withProgress(message = 'Building Travel Stats', value = 0.7, summarise_travel_stats(filtered_incidents()))
  })

  output$stats_note <- renderText({
    s <- travel_stats()
    if (nrow(s) == 0) return('No incidents match these filters')
    n_st <- n_distinct(s$source_id)
    n_inc <- s |> filter(!is.na(fdz)) |> distinct(fdz, n_incidents) |> pull(n_incidents) |> sum()
    overnight <- isolate(as.numeric(input$time_start) > as.numeric(input$time_end))
    paste0(format(n_inc, big.mark = ','), ' Incidents, ', n_st, ' Stations', if (overnight) ' (Overnight Time Window)' else '')
  })

  # keep p within the number of candidate stations
  observeEvent(travel_stats(), {
    n_st <- n_distinct(travel_stats()$source_id)
    req(n_st > 0)
    updateNumericInput(session, 'p', max = n_st, value = min(input$p, n_st, na.rm = TRUE))
  })

  # per-fdz table for one solution, with the chosen statistic from the serving station
  build_table <- function(res, stats, method, cost_col) {
    demand <- stats |> filter(!is.na(fdz)) |> distinct(fdz, n_incidents)
    if (method == 'p_median') {
      res$assignments |>
        left_join(demand, by = 'fdz') |>
        left_join(
          stats |> select(station = source_id, fdz, minutes = all_of(cost_col)),
          by = c('station', 'fdz')
        )
    } else if ('station' %in% names(res$coverage)) {
      # run card: the station that responds first is already on the coverage table
      res$coverage |>
        left_join(
          stats |> select(station = source_id, fdz, minutes = all_of(cost_col)),
          by = c('station', 'fdz')
        )
    } else {
      # nearest open station by the chosen statistic, covered if within threshold
      best <- stats |>
        filter(source_id %in% res$open_stations, !is.na(fdz)) |>
        group_by(fdz) |>
        slice_min(.data[[cost_col]], n = 1, with_ties = FALSE) |>
        ungroup() |>
        select(fdz, station = source_id, minutes = all_of(cost_col))
      res$coverage |> left_join(best, by = 'fdz')
    }
  }

  # all travel stats for each zone's serving station, used in the map hover
  add_zone_stats <- function(table, stats) {
    table |>
      left_join(
        stats |> select(
          station = source_id, fdz, mean_minutes, median_minutes, p90_minutes, mean_distance_m
        ),
        by = c('station', 'fdz')
      )
  }

  # run the chosen solver on the current stats, keeping every alternative.
  # A failed run leaves the previous result in place so the view is not blanked.
  result <- reactiveVal(NULL)
  observeEvent(input$run, {
    stats <- travel_stats()
    incidents <- filtered_incidents()
    method <- input$method
    cost_col <- input$cost_col
    runcard <- if (isTRUE(input$use_runcard)) runcard_df else NULL
    n_alts <- min(max(as.integer(input$n_alts), 1L, na.rm = TRUE), 10L)
    include <- as.integer(input$include)
    exclude <- as.integer(input$exclude)

    problem <- if (nrow(stats |> filter(!is.na(fdz))) == 0) {
      'No incidents match the travel stats filters'
    } else if (!isTRUE(is.finite(input$p) && input$p >= 1)) {
      'Number of stations (p) must be at least 1'
    } else if (method == 'mclp' && !isTRUE(is.finite(input$threshold))) {
      'Coverage threshold must be a number'
    }
    if (!is.null(problem)) {
      showNotification(problem, type = 'error', duration = 10)
      return()
    }

    with_baseline <- isTRUE(input$include_baseline)
    if (with_baseline && length(include) == 0) {
      showNotification('Include Baseline needs at least one required station, running without it', type = 'warning', duration = 10)
      with_baseline <- FALSE
    }

    solve <- function(p, n_solutions) {
      if (method == 'p_median') {
        solve_p_median(
          stats, p = p, cost_col = cost_col,
          include = include, exclude = exclude, n_solutions = n_solutions, runcard_df = runcard
        )
      } else {
        solve_mclp(
          stats, p = p, threshold_minutes = input$threshold, cost_col = cost_col,
          include = include, exclude = exclude, n_solutions = n_solutions, runcard_df = runcard
        )
      }
    }
    scenario <- function(a, label) {
      list(res = a, label = label, table = build_table(a, stats, method, cost_col) |> add_zone_stats(stats))
    }

    res <- tryCatch(
      withProgress(message = 'Solving', value = 0.5, {
        res <- solve(input$p, n_alts)
        alts <- imap(res$alternatives, \(a, k) scenario(a, paste0('Alternative ', k, if (k == 1) ' (Best)' else '')))
        # the required stations on their own: p equal to their count leaves no other station to open
        if (with_baseline) {
          alts <- c(list(scenario(solve(length(include), 1), 'Baseline')), alts)
        }
        # incidents are kept so Response Stats uses the same calls the optimizer did
        list(method = method, alts = alts, best = if (with_baseline) 2L else 1L, incidents = incidents)
      }),
      error = function(e) {
        showNotification(conditionMessage(e), type = 'error', duration = 10)
        NULL
      }
    )
    if (!is.null(res)) result(res)
  })

  output$has_result <- reactive(!is.null(result()))
  outputOptions(output, 'has_result', suspendWhenHidden = FALSE)

  # alternative selector follows each run, and clicking a row in the Alternatives tab picks it
  observeEvent(result(), {
    r <- result()
    labels <- map_chr(r$alts, 'label')
    updateSelectInput(session, 'alt', choices = set_names(seq_along(labels), labels), selected = r$best)
  })
  observeEvent(input$alts_table_rows_selected, {
    updateSelectInput(session, 'alt', selected = input$alts_table_rows_selected)
  })
  # and the dropdown moves the highlighted row
  observeEvent(input$alt, {
    req(result())
    selectRows(dataTableProxy('alts_table'), as.integer(input$alt))
  })

  # the alternative being viewed, in the same shape the outputs below expect
  current <- reactive({
    r <- req(result())
    idx <- min(max(as.integer(input$alt), 1L, na.rm = TRUE), length(r$alts))
    c(list(method = r$method), r$alts[[idx]])
  })

  # one row per scenario, the baseline (if run) and then the alternatives best first
  alts_summary <- reactive({
    r <- req(result())
    labels <- map_chr(r$alts, 'label')
    stations_txt <- map_chr(r$alts, \(a) paste(a$res$open_stations, collapse = ', '))
    if (r$method == 'p_median') {
      objective <- map_dbl(r$alts, \(a) a$res$objective)
      tibble(
        Scenario = labels,
        `Total Weighted Minutes` = round(objective),
        `Difference vs Best` = round(objective - objective[r$best]),
        `Open Stations` = stations_txt
      )
    } else {
      covered <- map_dbl(r$alts, \(a) a$res$covered_demand)
      tibble(
        Scenario = labels,
        `Demand Covered` = scales::percent(map_dbl(r$alts, \(a) a$res$pct_covered), accuracy = 0.1),
        `Incidents Covered` = covered,
        `Difference vs Best` = covered - covered[r$best],
        `Open Stations` = stations_txt
      )
    }
  })

  # file name piece for the scenario being viewed, e.g. alternative_1_best or baseline
  scenario_slug <- function(label) str_replace_all(str_to_lower(label), '[^a-z0-9]+', '_') |> str_remove_all('^_|_$')

  # table with display column names, shared by the Assignments tab and the CSV download
  display_table <- reactive({
    table <- current()$table |>
      select(any_of(c('fdz', 'station', 'n_incidents', 'covered', 'minutes'))) |>
      mutate(minutes = round(minutes, 2))
    names(table) <- c(
      fdz = 'FDZ', station = 'Station', n_incidents = 'Incidents',
      covered = 'Covered', minutes = 'Minutes'
    )[names(table)]
    table
  })

  # run card order for the open stations of the alternative being viewed, all candidate stations before a run
  runcard_table <- reactive({
    selected <- if (isTruthy(tryCatch(result(), error = \(e) NULL))) current()$res$open_stations else stations_df$source_id
    runcard_response_table(runcard_df, selected)
  })

  output$metrics <- renderUI({
    r <- current()
    res <- r$res
    weighted_minutes <- weighted.mean(r$table$minutes, r$table$n_incidents, na.rm = TRUE)
    boxes <- if (r$method == 'p_median') {
      list(
        value_box('Status', str_to_title(res$status)),
        value_box('Total Weighted Minutes', format(round(res$objective), big.mark = ',')),
        value_box('Average Minutes per Incident', round(weighted_minutes, 2))
      )
    } else {
      list(
        value_box('Status', str_to_title(res$status)),
        value_box('Demand Covered', scales::percent(res$pct_covered, accuracy = 0.1)),
        value_box('Incidents Covered', format(res$covered_demand, big.mark = ','))
      )
    }
    boxes <- map(boxes, \(b) tagAppendAttributes(b, style = 'min-height: 0;'))
    do.call(layout_columns, c(boxes, list(fill = FALSE, fillable = FALSE)))
  })

  output$map <- renderLeaflet({
    r <- current()
    open_df <- stations_df |> filter(source_id %in% r$res$open_stations)
    polys <- fdz_polygons_sf |> inner_join(r$table, by = 'fdz')

    if (r$method == 'p_median') {
      pal <- colorFactor('Set3', domain = sort(unique(polys$station)))
      polys <- polys |>
        mutate(fill = pal(station), label = hover_label(pick(everything()), paste0('Served by Station ', station)))
      legend_args <- list(pal = pal, values = polys$station, title = 'Serving Station')
    } else {
      pal <- colorFactor(c('#d95f02', '#1b9e77'), domain = c(FALSE, TRUE))
      polys <- polys |>
        mutate(
          fill = pal(covered),
          label = hover_label(
            pick(everything()),
            paste0(if_else(covered, 'Covered', 'Not Covered'), ' by Station ', station)
          )
        )
      legend_args <- list(pal = pal, values = c(FALSE, TRUE), title = 'Covered', labFormat = \(type, x) c('No', 'Yes'))
    }

    m <- leaflet() |>
      addProviderTiles(providers$Esri.WorldStreetMap) |>
      addPolygons(
        data = polys, fillColor = ~fill, fillOpacity = 0.6,
        color = '#555', weight = 1, label = ~label,
        labelOptions = labelOptions(style = list('font-size' = '13px'))
      ) |>
      addPolygons(
        data = fdz_polygons_sf |> filter(!fdz %in% r$table$fdz),
        fillColor = '#999', fillOpacity = 0.5, color = '#555', weight = 1,
        label = ~paste0('FDZ ', fdz, ': Not Included')
      ) |>
      addCircleMarkers(
        data = open_df, lng = ~lon, lat = ~lat, radius = 8,
        color = '#000', fillColor = '#d62728', fillOpacity = 1, weight = 2,
        label = ~paste('Station', source_id)
      )
    do.call(addLegend, c(list(m, position = 'bottomright'), legend_args))
  })

  output$alts_table <- renderDT({
    datatable(
      alts_summary(), rownames = FALSE, fillContainer = TRUE,
      selection = list(mode = 'single', selected = result()$best)
    )
  })

  output$table <- renderDT({
    datatable(display_table(), rownames = FALSE, fillContainer = TRUE)
  })

  output$runcard_table <- renderDT({
    table <- runcard_table()
    datatable(
      table, rownames = FALSE, fillContainer = TRUE,
      options = list(scrollX = TRUE, scrollY = '60vh', paging = FALSE)
    ) |>
      formatStyle('1st Due', fontWeight = 'bold', backgroundColor = '#d9ead3')
  })

  output$download_runcard <- downloadHandler(
    filename = function() {
      r <- tryCatch(result(), error = \(e) NULL)
      label <- if (is.null(r)) 'all_stations' else scenario_slug(current()$label)
      paste0('run_card_', label, '_', Sys.Date(), '.csv')
    },
    content = function(file) write_csv(runcard_table(), file, na = '')
  )

  output$download <- downloadHandler(
    filename = function() paste0('assignments_', scenario_slug(current()$label), '_', Sys.Date(), '.csv'),
    content = function(file) write_csv(display_table(), file)
  )

  # Response Stats: two scenarios compared at one run card response level.
  # Stations come from runcard_responders() on each scenario's open stations, never from the optimizer's assignments.
  observeEvent(result(), {
    r <- result()
    choices <- set_names(seq_along(r$alts), map_chr(r$alts, 'label'))
    has_baseline <- r$alts[[1]]$label == 'Baseline'
    updateSelectInput(session, 'rs_left', choices = choices, selected = if (has_baseline) 1L else r$best)
    updateSelectInput(session, 'rs_right', choices = choices, selected = r$best)
  })

  rs_scenarios <- reactive({
    r <- req(result())
    idx <- as.integer(c(req(input$rs_left), req(input$rs_right)))
    req(all(idx %in% seq_along(r$alts)))
    r$alts[idx]
  })

  # levels go up to the larger station count, the side with fewer stations shows its zones as having no responder
  observeEvent(rs_scenarios(), {
    n <- max(map_int(rs_scenarios(), \(a) length(a$res$open_stations)))
    choices <- set_names(seq_len(n), due_label(seq_len(n)))
    selected <- if (isTruthy(input$rs_level) && as.integer(input$rs_level) <= n) input$rs_level else 1L
    updateSelectInput(session, 'rs_level', choices = choices, selected = selected)
  })

  rs_compare <- reactive({
    sides <- rs_scenarios()
    k <- as.integer(req(input$rs_level))
    withProgress(message = 'Computing Response Stats', value = 0.5, {
      compare_runcard_response(
        req(result())$incidents, runcard_df,
        sides[[1]]$res$open_stations, sides[[2]]$res$open_stations, k
      )
    })
  })

  # confidence intervals are slow, so they wait for the button and are cleared when the comparison changes
  rs_ci <- reactiveVal(NULL)
  observeEvent(rs_compare(), rs_ci(NULL))
  observeEvent(input$rs_ci, {
    paired <- rs_compare()$paired
    if (nrow(paired) == 0) {
      showNotification('No paired calls to resample', type = 'error', duration = 10)
      return()
    }
    rs_ci(withProgress(message = 'Bootstrapping 95% CIs', value = 0.5, bootstrap_response_diff(paired)))
  })

  output$rs_note <- renderUI({
    if (is.null(result())) p(class = 'text-muted', 'Run the optimizer to compare scenarios.')
  })

  output$rs_summary <- renderDT({
    cmp <- rs_compare()
    sides <- rs_scenarios()
    ci <- rs_ci()
    stat_labels <- c(calls = 'Calls', mean = 'Mean Minutes', median = 'Median Minutes', p90 = '90th Percentile Minutes')
    table <- cmp$summary |>
      left_join(ci %||% tibble(stat = character(), lower = numeric(), upper = numeric()), by = 'stat') |>
      transmute(
        Statistic = stat_labels[stat],
        left = round(left, 2),
        right = round(right, 2),
        Difference = round(difference, 2),
        `Difference 95% CI` = if_else(is.na(lower), '', sprintf('[%.2f, %.2f]', lower, upper))
      )
    names(table)[2:3] <- paste0(c('Left: ', 'Right: '), map_chr(sides, 'label'))
    datatable(table, rownames = FALSE, options = list(dom = 't', ordering = FALSE))
  })

  output$rs_unserved <- renderUI({
    cmp <- rs_compare()
    sides <- rs_scenarios()
    level <- due_label(as.integer(input$rs_level))
    lines <- map2(c('Left', 'Right'), seq_len(2), \(side, i) {
      u <- cmp$unserved[i, ]
      paste0(
        side, ' (', sides[[i]]$label, '): ', u$zones, ' zones / ', format(u$calls, big.mark = ','),
        ' calls have no ', level, ' station on the run card.'
      )
    })
    div(
      class = 'text-muted small mt-2',
      map(lines, p, class = 'mb-1'),
      p(class = 'mb-1', paste0('Differences are right minus left over ', format(nrow(cmp$paired), big.mark = ','), ' calls served on both sides.')),
      if (!is.null(rs_ci())) p(class = 'mb-1', 'CIs resample whole FDZs (1,000 draws). Travel times are model estimates, so they reflect which calls occurred, not travel time variation.')
    )
  })

  rs_fdz_display <- reactive({
    cmp <- rs_compare()
    other_first <- runcard_df |> distinct(fdz, other_agency_first) |> group_by(fdz) |> summarise(other_agency_first = any(other_agency_first))
    cmp$by_fdz |>
      left_join(other_first, by = 'fdz') |>
      transmute(
        FDZ = fdz,
        `Left Station` = left_station,
        `Right Station` = right_station,
        `Other Agency First Due` = if_else(other_agency_first %in% TRUE, 'Yes', ''),
        Calls = calls,
        `Left Mean` = left_mean, `Right Mean` = right_mean, `Diff Mean` = diff_mean,
        `Left Median` = left_median, `Right Median` = right_median, `Diff Median` = diff_median,
        `Left P90` = left_p90, `Right P90` = right_p90, `Diff P90` = diff_p90
      ) |>
      mutate(across(c(`Left Mean`:`Diff P90`), \(x) round(x, 2)))
  })

  output$rs_fdz_table <- renderDT({
    datatable(
      rs_fdz_display(), rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 25)
    )
  })

  output$download_rs <- downloadHandler(
    filename = function() {
      sides <- rs_scenarios()
      paste0(
        'response_stats_', scenario_slug(sides[[1]]$label), '_vs_', scenario_slug(sides[[2]]$label),
        '_', str_remove(str_to_lower(due_label(as.integer(input$rs_level))), ' due'), '_due_', Sys.Date(), '.csv'
      )
    },
    content = function(file) write_csv(rs_fdz_display(), file, na = '')
  )
}

shinyApp(ui, server)

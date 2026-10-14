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

# loaded once at startup, shared across sessions
incident_category_df <- read_parquet(file.path(root, 'app/data/incident_categories.parquet'))
travel_matrix_df <- read_parquet(
  file.path(root, 'datasets/outputs/incident_travel_matrix_fdz.parquet'),
  col_select = c(
    'source_id', 'dest_id', 'fdz', 'duration_min', 'distance_m', 'rms_source_system',
    'rms_incident_type_code', 'cad_incident_type_code', 'dispatch_code'
  )
) |>
  left_join(incident_category_df, by = c('dest_id' = 'incident_number'))

# EMS unit capability needed by each dispatch code. Coded incidents the file does not list are 'OTHER'
# (all fire today), incidents with no dispatch code stay NA and drop out when the filter is used.
ems_capabilities_df <- read_parquet(file.path(root, 'app/data/ems_capabilities.parquet'))
travel_matrix_df <- travel_matrix_df |>
  left_join(ems_capabilities_df, by = 'dispatch_code') |>
  mutate(ems_unit_capability = if_else(is.na(ems_unit_capability) & !is.na(dispatch_code), 'OTHER', ems_unit_capability))
dispatch_capability_df <- travel_matrix_df |>
  distinct(dispatch_code, ems_unit_capability) |>
  filter(!is.na(dispatch_code))
fdz_polygons_sf <- readRDS(file.path(root, 'app/data/fdz_polygons.rds'))
stations_df <- read_parquet(file.path(root, 'app/data/stations.parquet'))
runcard_df <- read_parquet(file.path(root, 'app/data/runcard_beats.parquet'))
fdz_agency_df <- runcard_df |> distinct(fdz, agency)
agency_choices <- sort(unique(fdz_agency_df$agency))

source_system_choices <- sort(unique(na.omit(travel_matrix_df$rms_source_system)))
station_choices <- set_names(stations_df$source_id, paste('Station', stations_df$source_id))

# choices are named with their description so the dropdown shows and searches it
code_lookup_df <- read_parquet(file.path(root, 'app/data/code_lookup.parquet')) |>
  bind_rows(tibble(
    filter_by = 'ems_capability',
    code = c('PARAMEDIC', 'AEMT', 'EMT-I / BLS', 'AMBULANCE ONLY', 'OTHER'),
    description = c('Paramedic', 'AEMT', 'EMT-I / BLS', 'Ambulance Only', 'Other (No EMS Capability Listed)')
  ))
code_choices <- pmap(filter_defs, \(key, label, col, prefix_code) {
  codes <- sort(unique(na.omit(travel_matrix_df[[col]])))
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

  # travel stats, rebuilt only when the button is pressed (all incidents on startup)
  travel_stats <- eventReactive(
    input$build,
    {
      withProgress(message = 'Building Travel Stats', value = 0.5, {
        stats <- get_travel_stats(travel_matrix_df, filters = current_filters())
        # zones are limited to the selected agencies, so runs with and without the run card use the same zones
        if (length(input$agency) > 0) {
          agency_fdzs <- fdz_agency_df$fdz[fdz_agency_df$agency %in% input$agency]
          stats <- stats |> filter(is.na(fdz) | fdz %in% agency_fdzs)
        }
        stats
      })
    },
    ignoreNULL = FALSE
  )

  output$stats_note <- renderText({
    s <- travel_stats()
    n_st <- n_distinct(s$source_id)
    n_inc <- s |> filter(!is.na(fdz)) |> distinct(fdz, n_incidents) |> pull(n_incidents) |> sum()
    paste0(format(n_inc, big.mark = ','), ' Incidents, ', n_st, ' Stations')
  })

  # keep p within the number of candidate stations
  observeEvent(travel_stats(), {
    n_st <- n_distinct(travel_stats()$source_id)
    updateNumericInput(session, 'p', max = n_st, value = min(input$p, n_st))
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

  # run the chosen solver on the current stats, keeping every alternative
  result <- eventReactive(input$run, {
    stats <- travel_stats()
    method <- input$method
    cost_col <- input$cost_col
    runcard <- if (isTRUE(input$use_runcard)) runcard_df else NULL
    n_alts <- min(max(as.integer(input$n_alts), 1L, na.rm = TRUE), 10L)
    include <- as.integer(input$include)
    exclude <- as.integer(input$exclude)
    tryCatch(
      withProgress(message = 'Solving', value = 0.5, {
        res <- if (method == 'p_median') {
          solve_p_median(
            stats, p = input$p, cost_col = cost_col,
            include = include, exclude = exclude, n_solutions = n_alts, runcard_df = runcard
          )
        } else {
          solve_mclp(
            stats, p = input$p, threshold_minutes = input$threshold, cost_col = cost_col,
            include = include, exclude = exclude, n_solutions = n_alts, runcard_df = runcard
          )
        }
        alts <- map(res$alternatives, \(a) list(res = a, table = build_table(a, stats, method, cost_col) |> add_zone_stats(stats)))
        list(method = method, alts = alts)
      }),
      error = function(e) {
        showNotification(conditionMessage(e), type = 'error', duration = 10)
        NULL
      }
    )
  })

  output$has_result <- reactive(!is.null(result()))
  outputOptions(output, 'has_result', suspendWhenHidden = FALSE)

  # alternative selector follows each run, and clicking a row in the Alternatives tab picks it
  observeEvent(result(), {
    n <- length(result()$alts)
    labels <- paste('Alternative', seq_len(n), c('(Best)', rep('', n - 1)))
    updateSelectInput(session, 'alt', choices = set_names(seq_len(n), trimws(labels)), selected = 1)
  })
  observeEvent(input$alts_table_rows_selected, {
    updateSelectInput(session, 'alt', selected = input$alts_table_rows_selected)
  })

  # the alternative being viewed, in the same shape the outputs below expect
  current <- reactive({
    r <- req(result())
    idx <- min(max(as.integer(input$alt), 1L, na.rm = TRUE), length(r$alts))
    c(list(method = r$method), r$alts[[idx]])
  })

  # one row per alternative, best first
  alts_summary <- reactive({
    r <- req(result())
    stations_txt <- map_chr(r$alts, \(a) paste(a$res$open_stations, collapse = ', '))
    if (r$method == 'p_median') {
      objective <- map_dbl(r$alts, \(a) a$res$objective)
      tibble(
        Rank = seq_along(objective),
        `Total Weighted Minutes` = round(objective),
        `Difference vs Best` = round(objective - objective[1]),
        `Open Stations` = stations_txt
      )
    } else {
      covered <- map_dbl(r$alts, \(a) a$res$covered_demand)
      tibble(
        Rank = seq_along(covered),
        `Demand Covered` = scales::percent(map_dbl(r$alts, \(a) a$res$pct_covered), accuracy = 0.1),
        `Incidents Covered` = covered,
        `Difference vs Best` = covered - covered[1],
        `Open Stations` = stations_txt
      )
    }
  })

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
      selection = list(mode = 'single', selected = 1)
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
      label <- if (is.null(r)) 'all_stations' else paste0('alternative_', current()$res$rank)
      paste0('run_card_', label, '_', Sys.Date(), '.csv')
    },
    content = function(file) write_csv(runcard_table(), file, na = '')
  )

  output$download <- downloadHandler(
    filename = function() paste0('assignments_alternative_', current()$res$rank, '_', Sys.Date(), '.csv'),
    content = function(file) write_csv(display_table(), file)
  )
}

shinyApp(ui, server)

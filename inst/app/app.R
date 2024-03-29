library(shiny)
library(shinyWidgets)
library(tidyverse)
library(focustools)

## data dir
## list files in data dir
data_dir <- .GlobalEnv$.submission_dir
## note that fps are reversed so that most recent *should* appear first
fps <- rev(list.files(data_dir, pattern = "*.csv$", recursive = TRUE, full.names = TRUE))
## ignore params csv if present
fps <- fps[!grepl("params", fps)]
usafull <- .GlobalEnv$.data

## helper function used in the renderUI for renderPlot calls
get_plots <- function(n, ...) {

  plot_output_list <- list()

  for(i in 1:length(n)) {
    plotname <- paste("plot", i, sep="")
    plot_output_object <- renderPlot({
      plot_forecast(...)
    },
    height = n*250)
    plot_output_list[[i]] <- plot_output_object
  }

  return(plot_output_list)
}

ui <- fluidPage(
  titlePanel("FOCUS Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("forecast", "Select forecast", choices = basename(fps)),
      uiOutput("loc_checkbox"),
      htmlOutput("valid"),
      tags$br(),
      downloadButton("download"),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Welcome", includeMarkdown("welcome.md")),
        tabPanel("Visualization", uiOutput("plots")),
        tabPanel("Table", DT::dataTableOutput("table")),
        tabPanel("Summary",
                 tags$br(),
                 verbatimTextOutput("horizons"),
                 tabsetPanel(
                   tabPanel("Cumulative",
                            fluidRow(
                              column(
                                tags$h3("Cases"),
                                tableOutput("counts_ccases"),
                                width = 6),
                              column(
                                tags$h3("Deaths"),
                                tableOutput("counts_cdeaths"),
                                width = 6)
                            )
                   ),
                   tabPanel("Incident Counts",
                            fluidRow(
                              column(
                                tags$h3("Cases"),
                                tableOutput("counts_icases"),
                                width = 6),
                              column(
                                tags$h3("Deaths"),
                                tableOutput("counts_ideaths"),
                                width = 6)
                            )
                   ),
                   tabPanel("Incident % Change",
                            fluidRow(
                              column(
                                tags$h3("Cases"),
                                tableOutput("percdiff_icases"),
                                width = 6),
                              column(
                                tags$h3("Deaths"),
                                tableOutput("percdiff_ideaths"),
                                width = 6)
                            )
                    )
                 )
                 )
      )
    )
  )
)

server <- function(input, output) {

  ## reactive to read in the original submission file
  ## this is reactive because the data will change depending on which input$forecast is supplied
  submission_raw <- reactive({
    ## get the path to the forecast file from the input basename
    tmp_fp <- grep(input$forecast, fps, value = TRUE)
    data <- read_csv(paste0(dirname(tmp_fp), "/", input$forecast))
    return(list(data = data))
  })

  ## reactive engine that drives the bus here ...
  submission <- reactive({

    req(!is.null(submission_raw()))

    ## get the *names* (not codes) for locations
    locs <-
      focustools:::locations %>%
      filter(abbreviation %in% c("US", state.abb, "DC")) %>%
      filter(location %in% unique(submission_raw()$data$location))

      tmp_loc <-
        locs %>%
        filter(location_name %in% input$location) %>%
        pull(location) %>%
        unique(.)

      data <-
        submission_raw()$data %>%
        filter(location %in% tmp_loc)

      return(list(data = data, selected_loc = tmp_loc))

  })

  ## reactive engine that drives the bus here ...
  summary_dat <- reactive({

    req(!is.null(submission()))
    req(nrow(submission()$data) > 0)
    ## get the *names* (not codes) for locations
    locs <-
      focustools:::locations %>%
      filter(abbreviation %in% c("US", state.abb, "DC")) %>%
      filter(location %in% unique(submission_raw()$data$location))

    tmp_loc <-
      locs %>%
      filter(location_name %in% input$location) %>%
      pull(location) %>%
      unique(.)

    submission_summary(.data = usafull, submission = submission()$data, location = submission()$selected_loc)

  })

  ## reactive engine that drives the bus here ...
  validate_dat <- reactive({

    req(!is.null(submission()))
    tmp_file <- file.path(tempdir(), "submission-tmp.csv")

    submission()$data %>%
      write_csv(., tmp_file)

    ## should NOT be valid to have no locations selected
    if(nrow(submission()$data) == 0) {
      "<br><font color=\"#b22222\"><b>FORECAST FILE IS NOT VALID</b></font><br>"
    } else if(validate_forecast(tmp_file)$valid) {
      "<br><font color=\"#228B22\"><b>FORECAST FILE IS VALID</b></font><br>"
    } else {
      "<br><font color=\"#b22222\"><b>FORECAST FILE IS NOT VALID</b></font><br>"
    }

  })

  output$valid <- renderText({
    req(!is.null(validate_dat()))
    validate_dat()
  })
  ## checkbox to select locations
  ## this is a renderUI option
  output$loc_checkbox <- renderUI({

    ## requires that the original submission file has been read in ...
    req(!is.null(submission_raw()))

    ## get the *names* (not codes) for locations
    locs <-
      focustools:::locations %>%
      filter(abbreviation %in% c("US", state.abb, "DC")) %>%
      filter(location %in% unique(submission_raw()$data$location))

    ## checkbox choices are *names* (not codes) ... see above
    pickerInput("location","Select location", choices = locs$location_name, selected = locs$location_name, options = list(`actions-box` = TRUE),multiple = T)
  })

  ## renders all of the plots (individual renderPlot calls generated as a list by get_plots)
  output$plots <- renderUI({

    ## before trying to render plots make sure that locations are selected
    if(nrow(submission()$data) == 0) {
      HTML("<em>No locations selected.</em>")
    } else {
      ## call get_plots
      ## defined above
      ## effectively wraps focustools::plot_forecast() ...
      ## submission is reactive data from submission() reactive ...
      ## as is the location
      get_plots(n = length(unique(submission()$data$location)),
                .data = usafull,
                submission = submission()$data,
                location = submission()$selected_loc)
    }

  })

  ## tabular output
  output$table <- DT::renderDataTable({
    submission()$data
  })

  ## text explaining dates
  output$horizons <- renderText({

    tmp <-
      submission()$data %>%
      dplyr::distinct(target,target_end_date) %>%
      tidyr::separate(target, into = c("horizon", "tmp"), sep = " wk ahead ") %>%
      dplyr::select(-tmp) %>%
      dplyr::distinct() %>%
      dplyr::arrange(horizon) %>%
      dplyr::mutate(frmt = toupper(paste0(horizon, "w ahead: week ending in ", target_end_date)))

    ## get the date for horizon = 1
    ## used to
    h1_date <-
      tmp %>%
      filter(horizon == 1) %>%
      pull(target_end_date)

    prev <-
      tibble(horizon  = "Previous", target_end_date = h1_date - 7) %>%
      mutate(frmt = toupper(paste0(horizon, ": week ending in ", target_end_date)))

    bind_rows(prev, tmp) %>%
      pull(frmt) %>%
      paste0(., collapse = "\n")
  })
  ## summary table cumulative case counts
  ## NOTE: this is more involved because we dont forecats case counts directly
  ## but useful to include for reviewing?
  ## code below basically calculates cumulative cases from incident case forecasts
  output$counts_ccases <- renderTable({
    x <- summary_dat()$counts$icases

    ## get epiweek and epiyear for week before based on submission data
    ## this will be used find event count to determine 1wk horizon % change
    submission_ew <- min(lubridate::epiweek(submission()$data$target_end_date))
    submission_ey <- min(lubridate::epiyear(submission()$data$target_end_date))

    previous_ew <- ifelse(submission_ew == 1, 53, submission_ew - 1)
    previous_ey <- ifelse(submission_ew == 1, submission_ey - 1, submission_ey)

    previous_week <-
      usafull %>%
      dplyr::as_tibble() %>%
      dplyr::group_by(location) %>%
      ## restrict to appropriate epiyear/epiweek for week prior to submission
      dplyr::filter(epiyear == previous_ey, epiweek == previous_ew) %>%
      ## add a column for horizon 0 so we can stack on submission data (see below)
      dplyr::mutate(horizon = 0) %>%
      dplyr::select(horizon, location, count = ccases) %>%
      dplyr::left_join(select(focustools:::locations, location, location_name)) %>%
      ungroup() %>%
      select(-location) %>%
      select(location = location_name, week = horizon, count)

    x <-
      x %>%
      select(-Previous) %>%
      gather(week,count, -location) %>%
      mutate(week = gsub("w ahead", "", week)) %>%
      mutate(week = as.numeric(week)) %>%
      bind_rows(previous_week) %>%
      arrange(week) %>%
      group_by(location) %>%
      mutate(ccount = cumsum(count)) %>%
      ungroup() %>%
      dplyr::mutate(week = ifelse(week == 0, "Previous", paste0(week, "w ahead"))) %>%
      select(-count) %>%
      spread(week, ccount) %>%
      ungroup() %>%
      dplyr::select(location, Previous, dplyr::everything())

    ## make sure US is on top
    x <- bind_rows(dplyr::filter(x, location == "US"), dplyr::filter(x, location !="US"))

    names(x) <- gsub(" ahead", "", names(x))
    names(x) <- toupper(names(x))

    x

  },
  digits = 0,
  bordered = TRUE)

  ## summary table ideaths counts
  output$counts_cdeaths <- renderTable({
    x <- summary_dat()$counts$cdeaths
    names(x) <- gsub(" ahead", "", names(x))
    names(x) <- toupper(names(x))
    x
  },
  digits = 0,
  bordered = TRUE)

  ## summary table icases counts
  output$counts_icases <- renderTable({
    x <- summary_dat()$counts$icases
    names(x) <- gsub(" ahead", "", names(x))
    names(x) <- toupper(names(x))
    x
  },
  digits = 0,
  bordered = TRUE)

  ## summary table ideaths counts
  output$counts_ideaths <- renderTable({
    x <- summary_dat()$counts$ideaths
    names(x) <- gsub(" ahead", "", names(x))
    names(x) <- toupper(names(x))
    x
  },
  digits = 0,
  bordered = TRUE)

  ## summary table icases perc cahnge
  output$percdiff_icases <- renderTable({
    x <- summary_dat()$perc_diff$icases
    names(x) <- gsub(" ahead", "", names(x))
    names(x) <- toupper(names(x))
    x
  },
  digits = 0,
  bordered = TRUE)

  ## summary table ideaths perc change
  output$percdiff_ideaths <- renderTable({
    x <- summary_dat()$perc_diff$ideaths
    names(x) <- gsub(" ahead", "", names(x))
    names(x) <- toupper(names(x))
    x
  },
  digits = 0,
  bordered = TRUE)

  ## handler to download the selected data
  output$download <- downloadHandler(
    filename = function() {
      input$forecast
    },
    content = function(file) {
      readr::write_csv(submission()$data, file)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)

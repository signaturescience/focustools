library(shiny)
library(tidyverse)
library(focustools)

## data dir
## list files in data dir
data_dir <- .GlobalEnv$.submission_dir
## note that fps are reversed so that most recent *should* appear first
fps <- rev(list.files(data_dir, pattern = "*.csv$", recursive = TRUE, full.names = TRUE))

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
      downloadButton("download"),
      uiOutput("loc_checkbox"),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualization", uiOutput("plots")),
        tabPanel("Table", DT::dataTableOutput("table"))
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
    checkboxGroupInput("location", "Select location", choices = locs$location_name, selected = c("US"))

  })

  ## renders all of the plots (individual renderPlot calls generated as a list by get_plots)
  output$plots <- renderUI({

    ## call get_plots
    ## defined above
    ## effectively wraps focustools::plot_forecast() ...
    ## submission is reactive data from submission() reactive ...
    ## as is the location
    get_plots(n = length(unique(submission()$data$location)),
              .data = usafull,
              submission = submission()$data,
              location = submission()$selected_loc)

  })


  ## tabular output
  output$table <- DT::renderDataTable({
    submission()$data
  })

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

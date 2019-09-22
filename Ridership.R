library( ggplot2 )
library( dplyr )
library( magrittr )
library( shiny )
library( readxl )
library( lubridate )
library( tidyr )
library( chron )
library( shiny )
library( shinyWidgets )
library( ggthemes )
library( ggiraph )
library( gtools )

CAPMETRO_RIDERSHIP_URL <- "https://www.capmetro.org/uploadedFiles/New2016/About_Capital_Metro/Dashboards/BI_ADA/Ridership%20Accessibility%20Data.xlsx"
DAYCOUNT               <- 365 / 12 / 7
WEEKDAY_OPTIONS        <- c("Weekday", "Saturday", "Sunday")

# Create Ridership dataframe.
tmp <- tempfile()
download.file( CAPMETRO_RIDERSHIP_URL, tmp)
ridership <- read_xlsx(tmp, skip = 1) %>%
  separate(ROUTE_NAME, sep = "-", c("Route.number", NA), remove = FALSE, extra = "merge") %>%
  mutate( Month_year = mdy(MONTH_YEAR),
          Month      = month( Month_year ),
          Year       = year( Month_year),
          Route.number = as.integer( Route.number ),
          Service    = case_when(
            is.na(Route.number) ~ "Metro Access",
            Route.number < 400   ~ "Metro Bus",
            Route.number < 500   ~ "Shuttle and Night Owl",
            Route.number < 600   ~ "Red line",
            Route.number < 700   ~ "UT Shuttle",
            Route.number < 900   ~ "Metro Rapid",
            Route.number < 1000  ~ "Metro Express"
          )) %>%
  slice(-n())

weekday_count <- Vectorize( function(month_year, days) {
  day_count <- days_in_month(month_year)
  sum( day.of.week(month( month_year), seq(day_count), year(month_year)) %in% days )
}, vectorize.args = "month_year")

day_count <- ridership %>%
  distinct( Month_year) %>%
  mutate( Weekdays  = weekday_count( Month_year, 1:5),
          Saturdays = weekday_count( Month_year, 6),
          Sundays   = weekday_count( Month_year, 0))


shinyApp(
  ui = function(request) {
    # Create widgets to select routes to display
    routePickerInput <- function(service, routes) {
      pickerInput(paste0("routes.", service), paste0(service, " Routes:"), 
                  choices  = mixedsort(routes),
                  selected = routes,
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE))
    }
    
    pickers <- lapply( group_split( ridership, Service), function( service_block ) {
      routePickerInput( unique(service_block$Service), unique(service_block$ROUTE_NAME))
    })
    
    inputs <- c(pickers, list(
      checkboxInput("normalize", "Normalize Days of Week?"),
      helpText("Normalizing shows ~4.35 of each day of the week per month. This avoids noise from differing numbers of days and weekdays across months and years."),
      pickerInput( "Days", "Day of Week:",
                   choices = WEEKDAY_OPTIONS,
                   selected = WEEKDAY_OPTIONS,
                   multiple = TRUE)
    ))
    
    fluidPage( title = "Cap Metro Ridership Visualizer", 
    sidebarLayout(
      do.call( sidebarPanel, inputs ),
      mainPanel(girafeOutput("plot", width="100%", height = "750px"),
                bookmarkButton())
    ))
  },

  server = function(input, output) {
    
    selectedRoutes <- reactive({
      input_names <- names( input )
      route_pickers <- input_names[ startsWith( input_names , "routes." ) ]
      routes <- c( lapply( route_pickers, function(x) { input[[x]] } ), recursive = TRUE )
      validate(
        need(length(routes) > 0, "Must select at least one route.")
      )
      routes
    })

    weekday_weights <- reactive({
      validate(
        need(length(input$Days) > 0, "Must select at least one type of weekday.")
      )
      list(
        Sat = if_else("Saturday" %in% input$Days, 1L, 0L ),
        Sun = if_else("Sunday"   %in% input$Days, 1L, 0L ),
        Wd  = if_else("Weekday"  %in% input$Days, 1L, 0L )
      )
    })
        
    aggregated <- reactive({
      wts <- weekday_weights()
      ridership %>%
        filter( ROUTE_NAME %in% selectedRoutes()) %>%
        group_by( Month, Year, Month_year, DAY_TYPE) %>%
        summarize( Totals = sum(as.integer(SUM_RIDERSHIP_AVERAGE))) %>%
        pivot_wider(id_cols = c(Month, Year, Month_year), names_from = DAY_TYPE, values_from = Totals)   %>%
        inner_join( day_count, by = "Month_year" ) %>%
        mutate( Blended.ridership = Saturday * DAYCOUNT / Saturdays * wts$Sat + 
                                    Sunday   * DAYCOUNT / Sundays   * wts$Sun + 
                                5 * Weekday  * DAYCOUNT / Weekdays  * wts$Wd,
                Actual.ridership  = Saturday * wts$Sat + Sunday * wts$Sun + Weekday * wts$Wd,
                Normalize = isTRUE(input$normalize), 
                Ridership = if_else(Normalize, as.integer(round(Blended.ridership)), Actual.ridership)) %>%
        ungroup() %>% 
        select( -Month_year ) %>%
        arrange( Year, Month ) %>%
        mutate( Year = as.factor(Year),
                Tooltip = paste("Ridership:", scales::comma(Ridership)))
    })

    output$plot = renderGirafe({
        plot <- ggplot(aggregated(), aes( x = Month, y = Ridership, group = Year, color = Year)) +
          geom_line() +
          geom_point_interactive(aes(tooltip = Tooltip), size = 1) +
          scale_y_continuous(labels = scales::comma) + 
          scale_x_continuous(breaks = 1:12, labels = function(x) month.abb[x]) +
          theme_fivethirtyeight() +
          theme(panel.grid.major.x = element_blank()) +
          scale_color_tableau() +
          labs( caption = "Powered by Austin on Your Feet, with data from CapMetro. http://austinonyourfeet.com") +
          ggtitle("Cap Metro Ridership (selected routes)")
        girafe(ggobj = plot)
    })
  },
  enableBookmarking = "url"
)

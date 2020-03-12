library( httr )
library( ggplot2 )
library( dplyr )
library( magrittr )
library( shiny )
library( shinyWidgets )
library( lubridate )
library( tidyr )
library( ggthemes )
library( ggiraph )
library( chron)
library( zoo )
library( readr )
library( gtools )
library( stringr )

CAPMETRO_APP_URL       <- "https://app.capmetro.org/ADAReports/Report/RidershipReports"
CAPMETRO_RIDERSHIP_URL <- "https://app.capmetro.org/ADAReports/Report/ExportRidershipReport"
DAYCOUNT               <- 365 / 12 / 7
WEEKDAY_OPTIONS        <- c("Weekday", "Saturday", "Sunday")

# Initialize connection to app, which sets cookies necessary for the next step.
GET( CAPMETRO_APP_URL )
ridership <- GET( CAPMETRO_RIDERSHIP_URL) %>% 
  content() %>%
  separate(`ROUTE NAME`, sep = "-", c("Route.number", NA), remove = FALSE, extra = "merge") %>%
  mutate( `MONTH YEAR` = str_replace(`MONTH YEAR`, "-(\\d{2})$", "-20\\1"),
          Month_year = mdy(`MONTH YEAR`),
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

disaggregatedRidership <- function(wts, routes, normalize = TRUE ) {
  ridership %>%
    filter( `ROUTE NAME` %in% routes) %>%
    mutate( Ridership = as.integer(`SUM RIDERSHIP AVERAGE`)) %>%
    pivot_wider(id_cols = c(Month, Year, Month_year, Route.number, `ROUTE NAME`), names_from = c( `DAY TYPE`), values_from = Ridership) %>%
    inner_join( day_count, by = "Month_year" ) %>%
    mutate( Blended.ridership = Saturday * DAYCOUNT / Saturdays * wts$Sat + 
              Sunday   * DAYCOUNT / Sundays   * wts$Sun + 
              5 * Weekday  * DAYCOUNT / Weekdays  * wts$Wd,
            Actual.ridership  = Saturday * wts$Sat + Sunday * wts$Sun + Weekday * wts$Wd,
            Normalize = isTRUE(normalize),
            Ridership = if_else(Normalize, as.integer(round(Blended.ridership)), Actual.ridership)) %>%
    select( Month, Year, Month_year, Route.number, Ridership ) %>%
    arrange( Year, Month ) %>%
    group_by( Route.number ) %>%
    mutate( Moving_average  = rollsumr( Ridership, 12, fill = NA)) %>%
    filter( Month_year >= "2019-06-01") %>%
    mutate( Growth          = ( Moving_average / first(Moving_average) ) - 1)  %>%
    ungroup() %>%
    mutate( Route.number = as.factor(Route.number),
            Tooltip      = paste(Route.number, ":", scales::percent(Growth)))
}

aggregateRoutes <- function( wts, routes, normalize = TRUE ) {
  ridership %>%
    filter( `ROUTE NAME` %in% routes) %>%
    group_by( Month, Year, Month_year, `DAY TYPE`) %>%
    summarize( Totals = sum(as.integer(`SUM RIDERSHIP AVERAGE`))) %>%
    pivot_wider(id_cols = c(Month, Year, Month_year), names_from = `DAY TYPE`, values_from = Totals)   %>%
    inner_join( day_count, by = "Month_year" ) %>%
    mutate( Blended.ridership = Saturday * DAYCOUNT / Saturdays * wts$Sat + 
              Sunday   * DAYCOUNT / Sundays   * wts$Sun + 
              5 * Weekday  * DAYCOUNT / Weekdays  * wts$Wd,
            Actual.ridership  = Saturday * wts$Sat + Sunday * wts$Sun + Weekday * wts$Wd,
            Normalize = isTRUE(normalize), 
            Ridership = if_else(Normalize, as.integer(round(Blended.ridership)), Actual.ridership)) %>%
    ungroup() %>% 
    group_by( Month ) %>%
    arrange( Year) %>%
    mutate( Year_over_year   = Ridership / lag( Ridership) - 1) %>%
    ungroup() %>%
    arrange( Year, Month ) %>%
    mutate( Year            = as.factor(Year),
            Moving_average  = rollsumr( Ridership, 12, fill = NA),
            Tooltip         = paste("Ridership:", scales::comma(Ridership)),
            Change.tooltip  = paste(month( Month, label = TRUE), Year, "Change:", scales::percent( Year_over_year)),
            Rolling.tooltip = paste(month( Month, label = TRUE), Year, "Trailing:", scales::comma( Moving_average)),
            "Cap Remap"     = case_when(
              Month_year < "2018-06-01" ~ "Before",
              Month_year >= "2018-06-01" ~ "After"),
            "Cap Remap"     = factor( `Cap Remap`, levels = c("Before", "After")))
}

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
      routePickerInput( unique(service_block$Service), unique(service_block[["ROUTE NAME"]]))
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
      mainPanel(
        tabsetPanel(
          tabPanel( "Ridership",           girafeOutput("plot",        width="100%", height = "750px"), bookmarkButton()),
          tabPanel( "Change in ridership", girafeOutput("changePlot",  width="100%", height = "750px")),
          tabPanel( "12-month average",    girafeOutput("rollingPlot", width="100%", height = "750px")),
          tabPanel( "Route Growth",        girafeOutput("growthPlot",  width="100%", height = "750px"))
        )
      )
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
        
    aggregated    <- reactive({ aggregateRoutes(weekday_weights(), selectedRoutes(), input$normalize) })
    disaggregated <- reactive({ disaggregatedRidership(weekday_weights(), selectedRoutes(), input$normalize) })
    
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
        girafe(ggobj = plot, width_svg = 10, height_svg = 8)
    })

    output$changePlot = renderGirafe({
        separated <- aggregated() %>%
          filter(! is.na( Year_over_year ))
        plot <- ggplot(separated, aes( x = Month_year, y = Year_over_year, color = `Cap Remap`, group = `Cap Remap`)) +
          geom_line() +
          geom_point_interactive(aes(tooltip = Change.tooltip), size = 1) +
          geom_smooth(method = "lm", se = FALSE) +
          scale_y_continuous(labels = scales::percent) +
          theme_fivethirtyeight() +
          theme(panel.grid.major.x = element_blank()) +
          scale_color_tableau() +
          labs( caption = "Powered by Austin on Your Feet, with data from CapMetro. http://austinonyourfeet.com") +
          ggtitle("Year-over-year Ridership Change (selected routes)")
        girafe(ggobj = plot, width_svg = 10, height_svg = 8)
    })

    output$rollingPlot = renderGirafe({
      separated <- aggregated() %>%
        filter(! is.na( Moving_average ))
      plot <- ggplot(separated, aes( x = Month_year, y = Moving_average, color = `Cap Remap`, group = `Cap Remap`)) +
        geom_line() +
        geom_point_interactive(aes(tooltip = Rolling.tooltip), size = 1) +
        scale_y_continuous(labels = scales::comma) +
        theme_fivethirtyeight() +
        theme(panel.grid.major.x = element_blank()) +
        scale_color_tableau() +
        labs( caption = "Powered by Austin on Your Feet, with data from CapMetro. http://austinonyourfeet.com") +
        ggtitle("Trailing 12-month Ridership (selected routes)")
      girafe(ggobj = plot, width_svg = 10, height_svg = 8)
    })
    
    output$growthPlot = renderGirafe({
      plot <- ggplot(disaggregated(), aes( x = Month_year, y = Growth, group = Route.number, color = Route.number) ) +
        geom_line() +
        geom_point_interactive(aes(tooltip = Tooltip), size = 1) +
        scale_y_continuous(labels = scales::percent) + 
        theme_fivethirtyeight() +
        theme(panel.grid.major.x = element_blank()) +
        labs( caption = "Powered by Austin on Your Feet, with data from CapMetro. http://austinonyourfeet.com") +
        ggtitle("Cap Metro Ridership (selected routes)")
      girafe(ggobj = plot, width_svg = 10, height_svg = 8)
    })
    
  }, enableBookmarking = "url"
)

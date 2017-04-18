
rm(list = ls())
setwd("~/Google-Drive/8.Visualization/hw/hw2")
library(shiny)
library(ggvis)
library(dplyr)

life <- read.csv("life.csv")
fer <- read.csv("fertility.csv")
region <- read.csv("region.csv")
pop <- read.csv("population.csv")

region <- region[c(1,2)]
life <- life[-c(3,4)]
fer <- fer[-c(3,4)]
pop <- pop[-c(3,4)]

names(life)[3:ncol(life)] <- as.character(1960:(1960+ncol(life)-3))
names(fer)[3:ncol(fer)] <- as.character(1960:(1960+ncol(fer)-3))
names(pop)[3:ncol(pop)] <- as.character(1960:(1960+ncol(pop)-3))

remove <- c("Arab World", "Central Europe and the Baltics", "Caribbean small states", 
            "East Asia & Pacific (excluding high income)", "Early-demographic dividend",
            "East Asia & Pacific","Europe & Central Asia (excluding high income)", 
            "Europe & Central Asia", "European Union","Fragile and conflict affected situations",
            "High income","Heavily indebted poor countries (HIPC)","IBRD only","IDA & IBRD total", 
            "IDA total", "IDA blend","IDA only","Not classified", "Not classified",
            "Latin America & Caribbean (excluding high income)", "Latin America & Caribbean", 
            "Least developed countries: UN classification", "Low income", "Lower middle income",
            "Low & middle income","Late-demographic dividend", "Middle East & North Africa", 
            "Middle income", "Middle East & North Africa (excluding high income)", "North America", 
            "OECD members", "Other small states", "Pre-demographic dividend", 
            "Pacific island small states", "Post-demographic dividend", "South Asia", 
            "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa", "Small states", 
            "East Asia & Pacific (IDA & IBRD countries)", 
            "Europe & Central Asia (IDA & IBRD countries)", 
            "Latin America & the Caribbean (IDA & IBRD countries)", 
            "Middle East & North Africa (IDA & IBRD countries)", "South Asia (IDA & IBRD)", 
            "Sub-Saharan Africa (IDA & IBRD countries)", "Upper middle income", "World","Euro area")


ui <- fixedPage(
  # fixedPanel(
    headerPanel('Life Expectancy vs Fertility Rate'),
    fluidRow(
          column(7,
                mainPanel(
                uiOutput("ggvis_ui"),
                ggvisOutput("ggvis"),
                width=30
                      )),
      
          column(5,
                 sidebarPanel(
                  width = 8,
                  checkboxGroupInput("region",
                                     "Select Continent(s)",
                                     choices = c(
                                       "Latin America & Caribbean",
                                       "South Asia",
                                       "Sub-Saharan Africa",
                                       "Europe & Central Asia",
                                       "Middle East & North Africa",
                                       "East Asia & Pacific",
                                       "North America"),
                                     selected = c(
                                       "Latin America & Caribbean",
                                       "South Asia",
                                       "Sub-Saharan Africa",
                                       "Europe & Central Asia",
                                       "Middle East & North Africa",
                                       "East Asia & Pacific",
                                       "North America")),
                  
                  tags$head(
                    tags$style(
                      HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
                           visibility: hidden !important;}'))),
                  # double-end slider bar
                  sliderInput(inputId = "population_size",
                              label = "Population",
                              min=0,
                              max = 1.3,
                              value = 1.3,
                              ticks = F
                              # step = 0.1
                              ),
                  sliderInput(inputId = "speed", 
                              label = "Interval Speed (ms)",
                              min=100,
                              max=200, 
                              value=150,
                              step=10
                              # values = c("Fast","Medium","Slow")
                              ),
                  uiOutput("choose_slide")
                        ))))
  

server <- function(input, output) {

  # Play time series slider by customised speed 
  output$choose_slide <- renderUI({
    sliderInput(inputId = "year", 
                label = "Year", 
                min=1960,
                max=2014,
                value=1960,
                step = 1,
                animate = animationOptions(
                  playButton = 'Play Time Series',
                  pauseButton = 'Pause',
                  interval = input$speed
                  )
                )
    })
  
  # reactive year input
  v <- reactiveValues(year = 1962)
  observeEvent(input$year, {
    v$year <- input$year
  })
  
  # data to plot by using reactive year
  df <- reactive({
    l <- life[c('Country.Name','Country.Code',paste(v$year))]
    f <- fer[c('Country.Code', paste(v$year))]
    p <- pop[c('Country.Code', paste(v$year))]

    names(l) <- c('Country.Name','Country.Code','life.exp')
    names(f) <- c('Country.Code', 'fertility')
    names(p) <- c('Country.Code', 'population')

    df <- l %>%
      left_join(f,by='Country.Code') %>%
      left_join(p,by='Country.Code') %>%
      left_join(region, by='Country.Code')
    
    df <- df[! df$Country.Name %in% remove, ]
    # selective continent
    df <- df[df$Region %in%  input$region, ]
    # selective population size
    df$population <- 10 + (df$population * 1e-06) ^ input$population_size
    df
  })
  
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- df()[df()$Country.Code == x$Country.Code, ]
    names(row) <- c("Country Name", "Country Code", "Life Expectancy", "Fertility", 
                    "Population", "Region")
    paste0(names(row), ": ", format(row), collapse = "<br />")
  }
  
  df %>% 
    ggvis(~life.exp, ~fertility,
          key := ~Country.Code,
          fill = ~factor(Region)
          ) %>% 
    # scale_numeric("x", domain = c(30, 85), niCce = T) %>%
    scale_numeric("y", domain = c(0, 9), nice = T) %>%
    add_tooltip(all_values, "hover") %>%
    layer_points(
                 fillOpacity := 0.6,
                 size := ~ population
                 ) %>%
    add_relative_scales() %>%
    add_legend("fill", 
               title = "Continent",
               properties = legend_props(
                 legend = list(
                 x = scaled_value("x_rel", 0.02),
                 y = scaled_value("y_rel", 0.358)
                 )
               )
  ) %>% 
    add_axis("y", title = "Fertility")  %>%
    add_axis("x", title = "Life Expectancy") %>%
    set_options(height = 500, width = 700, duration=0) %>% 
    bind_shiny("ggvis", "ggvis_ui")
  
}


shinyApp(ui = ui, server = server)
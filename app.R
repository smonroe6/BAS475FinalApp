#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(fpp3)
library(ggplot2)
library(ggeasy)
library(plotly)
library(ggpubr)
library(GGally)
data("souvenirs")

introtab <- menuItem("Welcome to My App", tabName = "Welcome")
topictab <- menuItem("Time Series of Topic", tabName = "TopicPlot")
choosetab <- menuItem("Aspects of Series", tabName = "Choose")
forecasttab <- menuItem("Simple Forecast", tabName = "SimpleForecast")


sidebar <- dashboardSidebar(sidebarMenu(
  introtab,
  topictab,
  choosetab,
  forecasttab
))

body <- dashboardBody(tabItems(
  tabItem(
    tabName = "Welcome",
    h1("Welcome!"),
    h2("This app has 3 tabs with features."),
    h3("1. A tab with a plot of monthly souvenir sales from a shop in Queensland, Australia from January 1987 to December 1993"),
    h3("2. A tab with the option to choose from seeing graphics that display seasonality, autocorrelation, or decomposition"),
    h3("3. A tab that shows the sales forcast based on the forecast model that is selected.")
  ),
  tabItem(
    tabName = "TopicPlot",
    h2("A plot displaying monthly souvenir sales from a shop in Queensland, Australia from January 1987 to December 1993"),
    h3("The plot can be zoomed in on by dragging around desired area and then returned to view by double clicking."),
    plotlyOutput("timeplot"),
    h4("The plot generally has an upward trend.  This means that over time sales are increasing.
         It appears to be growing faster towards the end of the data.
         There does appear to be seasonality, as there is a peak around the same time each year.
         This is around Christmas.  In Australia it is Summer at Christmas, so there is a lot of travel.
         There is positive autocorrelation.
         This means that the previous month or months can help to provide information on the current one.
         This helps to support the positive trend observation.
         The degree of autocorrelation is strongest 12 months apart, as this is the distance between peaks.\
         This supports the seasonality observation.")
  ),
  tabItem(
    tabName = "Choose",
    h2("Select seasonality, autocorrelation, or decomposition from drop down box and a plot that describes that feature will appear."),
    selectInput(
      inputId = "SeasonAutoDecomp",
      label = "Choose a component to see plotted",
      choices = list("Seasonality", "Autocorrelation", "Decomposition"),
      selected = 1
    ),
    plotOutput("descriptor")
  ),
  tabItem(
    tabName = "SimpleForecast",
    h2("Select a forecast model from the drop down box.  The options are Naive, Mean, Seasonal Naive, and Drift.
       A plot showing that forecast will then appear"),
    selectInput(
      inputId = "SimpleForecast",
      label = "Choose a component to see plotted",
      choices = list("Naive", "Mean", "Seasonal Naive", "Drift"),
      selected = 1
    ),
    plotOutput("simpleforecast")
  )
))

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Souvenir Sales"),
    sidebar,
    body
  )
)


server <- function(input, output) {
  output$timeplot <- renderPlotly({
    souvenirs %>% autoplot(Sales)
  })

  output$descriptor <- renderPlot({
    if (input$SeasonAutoDecomp == "Seasonality") {
      souvenirs %>%
        select(Sales) %>%
        gg_season()
    } else if (input$SeasonAutoDecomp == "Autocorrelation") {
      souvenirs %>%
        select(Sales) %>%
        ACF() %>%
        autoplot()
    } else {
      souvenirs %>%
        model(classical_decomposition(Sales, type = "multiplicative")) %>%
        components() %>%
        autoplot()
    }
  })


  output$simpleforecast <- renderPlot({
    sales <- souvenirs %>%
      filter_index("1987 Jan" ~ "1991 Dec") %>%
      select(Sales)

    if (input$SimpleForecast == "Naive") {
      sales_fit <- sales %>%
        model(
          `Naïve` = NAIVE(Sales)
        )

      sales_fc <- sales_fit %>% forecast(h = 24)

      sales_fc %>%
        autoplot(sales, level = NULL) +
        autolayer(
          filter_index(souvenirs, "1992 Jan" ~ "1993 Dec"),
          colour = "black"
        ) +
        labs(
          y = "Australian Dollars",
          title = "Forecasts for monthly souvenir sales"
        ) +
        guides(colour = guide_legend(title = "Forecast"))
    } else if (input$SimpleForecast == "Mean") {
      sales_fit <- sales %>%
        model(
          Mean = MEAN(Sales)
        )

      sales_fc <- sales_fit %>% forecast(h = 24)

      sales_fc %>%
        autoplot(sales, level = NULL) +
        autolayer(
          filter_index(souvenirs, "1992 Jan" ~ "1993 Dec"),
          colour = "black"
        ) +
        labs(
          y = "Australian Dollars",
          title = "Forecasts for monthly souvenir sales"
        ) +
        guides(colour = guide_legend(title = "Forecast"))
    } else if (input$SimpleForecast == "Seasonal Naive") {
      sales_fit <- sales %>%
        model(
          `Seasonal naïve` = SNAIVE(Sales)
        )

      sales_fc <- sales_fit %>% forecast(h = 24)

      sales_fc %>%
        autoplot(sales, level = NULL) +
        autolayer(
          filter_index(souvenirs, "1992 Jan" ~ "1993 Dec"),
          colour = "black"
        ) +
        labs(
          y = "Australian Dollars",
          title = "Forecasts for monthly souvenir sales"
        ) +
        guides(colour = guide_legend(title = "Forecast"))
    } else {
      sales_fit <- sales %>%
        model(
          Drift = NAIVE(Sales ~ drift())
        )

      sales_fc <- sales_fit %>% forecast(h = 24)

      sales_fc %>%
        autoplot(sales, level = NULL) +
        autolayer(
          filter_index(souvenirs, "1992 Jan" ~ "1993 Dec"),
          colour = "black"
        ) +
        labs(
          y = "Australian Dollars",
          title = "Forecasts for monthly souvenir sales"
        ) +
        guides(colour = guide_legend(title = "Forecast"))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

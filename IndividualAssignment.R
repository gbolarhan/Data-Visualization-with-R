install.packages(c("fastmap","shiny", "forecast", "ggplot2", "tidyverse", "DT"))
library(fastmap)
library(shiny)
library(forecast)
library(ggplot2)
library(tidyverse)
library(DT)
fsales <- "https://raw.githubusercontent.com/multidis/hult-inter-bus-reports-r/main/forecasting/sales_weekly.csv"
sales <- read_csv(fsales)
ui <- fluidPage(
  titlePanel("Retail Sales Forecasting"),
  sidebarLayout(
    sidebarPanel(
      selectInput("store", "Select Store:", choices = unique(sales$Store)),
      actionButton("forecast", "Generate Forecast")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Historical Sales",
                 DTOutput("salesTable"),
                 plotOutput("salesPlot")
        ),
        tabPanel("Sales Forecast",
                 plotOutput("forecastPlot"),
                 tableOutput("forecastTable")
        )
      )
    )
  )
)

server <- function(input, output) {
  nweek_now <- max(sales$Week)
  
  store_data <- reactive({
    req(input$store)
    sales %>% filter(Store == input$store)
  })
  
  output$salesTable <- renderDT({
    store_data() %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$salesPlot <- renderPlot({
    ggplot(store_data(), aes(x = Week, y = Weekly_Sales)) +
      geom_line() +
      labs(title = "Historical Weekly Sales", x = "Week", y = "Weekly Sales")
  })
  
  observeEvent(input$forecast, {
    sales_hist <- store_data() %>% filter(Week <= nweek_now - 13)
    sales_last <- store_data() %>% filter(Week > nweek_now - 13)
    
    sales_hist_ts <- ts(sales_hist$Weekly_Sales, frequency = 52)
    arima_model <- auto.arima(sales_hist_ts, seasonal.test = "seas")
    arima_pred <- forecast(arima_model, h = 13)
    
    output$forecastPlot <- renderPlot({
      sales_pred_eval <- data.frame(predicted = as.numeric(arima_pred$mean),
                                    actual = sales_last$Weekly_Sales,
                                    Week = sales_last$Week)
      ggplot(sales_pred_eval, aes(x = Week)) +
        geom_line(aes(y = predicted, col = "Predicted")) +
        geom_line(aes(y = actual, col = "Actual")) +
        labs(title = "Sales Forecast vs Actual", x = "Week", y = "Weekly Sales")
    })
    
    output$forecastTable <- renderTable({
      data.frame(Week = sales_last$Week,
                 Predicted = as.numeric(arima_pred$mean),
                 Actual = sales_last$Weekly_Sales)
    })
  })
}
shinyApp(ui = ui, server = server)

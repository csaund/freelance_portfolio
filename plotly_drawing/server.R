#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(dplyr)
library(shiny)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  load('w.ws.RData')
  reactiveMaster <- reactive({
    dat <- w.ws %>% 
      filter(Date >= input$inDateRange[1] & Date < input$inDateRange[2])
    return(dat)
  })
  
  fig <- reactiveVal()
  
  output$fig <- renderPlotly({
    print(fig())
    plot_data <- reactiveMaster()
      plot_ly(plot_data,
            x=~Date,
            type='candlestick',
            open=~Open,
            high=~High,
            low=~Low,
            close=~Settle)
    })
  output$figs <- renderPlotly(
    us10y.p$cwc$f.d5.w[j:k] %>%
      plot_ly(x=w.ws[j:k,Date],
              y=~us10y.f.d5.suw,
              type='scatter',
              mode='lines+markers')
  )
  output$figp <- renderPlotly(
    us10y.p$cwc$f.d5.w[j:k]  %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d5.prw,
            type='scatter',
            mode='lines+markers')
  )
  output$figs10 <- renderPlotly(
    us10y.p$cwc$f.d10.w[j:k]  %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d10.suw,
            type='scatter',
            mode='lines+markers')
  )
  output$figp10 <- renderPlotly(
    us10y.p$cwc$f.d10.w[j:k]  %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d10.prw,type='scatter',
            mode='lines+markers')
  )
  
  click_data <- reactive({
    req(fig())
    event_data("plotly_click", source = "fig")
  })
  
  observeEvent(click_data(),{
    print(click_data())
  })
  
  #l <- list(
  #  type='line',
  #  x0='2020-04-24', 
  #  x1 = '2020-05-18',
  #  y0=0.579,
  #  y1=0.739,
  #  xref='x',
  #  yref='y',
  #  line=list(color='green',width=0.5))

})

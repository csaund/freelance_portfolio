library(shiny)

ui <- fluidPage(
  dateRangeInput("inDateRange", "Date range input:", 
                 start="2018-05-19",
                 end="2019-05-19"),
  plotlyOutput("p"),
  verbatimTextOutput("info"),
  verbatimTextOutput("clicks")
)

server <- function(input, output, session) {
  load('w.ws.RData')
  
  lines <- list(
    list(
      text = "hot",
      x = 0.5, 
      y = 0.5, 
      xref = "paper",
      yref = "paper",
      showarrow = FALSE
    ),
    list(
      text = "fire",
      x = 0.5, 
      y = 0.5, 
      xref = "paper",
      yref = "paper"
    )
  )
  
  reactiveMaster <- reactive({
    dat <- w.ws %>% 
      filter(Date >= input$inDateRange[1] & Date < input$inDateRange[2])
    return(dat)
  })
  
  output$p <- renderPlotly({
    plot_ly(reactiveMaster(), 
            x=~Date,
            type='candlestick',
            open=~Open,
            high=~High,
            low=~Low,
            close=~Settle) %>%
      layout(
        annotations = lines) %>%
      config(editable = TRUE)
  })
  
  output$info <- renderPrint({
    event_data("plotly_relayout")
  })
  output$clicks <- renderPrint({
    event_data("plotly_click")
  })
  
}

shinyApp(ui, server)
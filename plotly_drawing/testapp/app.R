library(shiny)

ui <- fluidPage(
  sidebarPanel(
    dateRangeInput("inDateRange", "Date range input:", 
                   start="2018-05-19",
                   end="2019-05-19"),
    verbatimTextOutput("info"),
    verbatimTextOutput("clicks"),
    h3("manual line entry:"),
    dateRangeInput("lineDrawDateRange", "Date range input:", 
                   start="2018-05-19",
                   end="2019-05-19"),
    numericInput("y0", label="y0", value=3),
    numericInput("y1", label="y1", value=3),
    actionButton("addLine", "Add Line From Values Above"),
    actionButton("addLineDrag", "Add line by clicking"),
    actionButton("testbutton", "Test dat button")
  ),
  mainPanel(
    plotlyOutput("p")
  )
)

server <- function(input, output, session) {
  load('w.ws.RData')
  
  examples <- list(
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

  values <- reactiveValues(val=NULL, lines=list())
  
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
        annotations = examples,
        shapes = values$lines) %>%
      config(editable = TRUE)
  })
  
  output$info <- renderPrint({
    event_data("plotly_relayout")
  })
  output$clicks <- renderPrint({
    event_data("plotly_click")
  })
  
  observeEvent(input$addLine, {
    l <- list(
      type='line',
      x0 = input$lineDrawDateRange[1], 
      x1 =input$lineDrawDateRange[2],
      y0 = input$y0,
      y1 = input$y1,
      xref='x',yref='y',
      line=list(color='green',width=0.5))
    print("defo clicked the button")
    values$lines <- append(values$lines, l)
    print(values$lines)
  })
  
  observeEvent(input$addLineDrag, {
    # wait for first  click
    # wait for second click
    # draw dat line
  })
  
  observeEvent(input$testbutton, {
    examples <- append(examples, 
      list(
        text = "SO HOT",
        x = 0.5, 
        y = 0.5, 
        xref = "paper",
        yref = "paper",
        showarrow = FALSE
      ))
  })
  
}

shinyApp(ui, server)
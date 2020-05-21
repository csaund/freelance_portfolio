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
    actionButton("addLine", "Add Line Manually"),
    actionButton("addLineDrag", "Add line by clicking")
  ),
  mainPanel(
    plotlyOutput("p")
  )
)

server <- function(input, output, session) {
  load('w.ws.RData')

  values <- reactiveValues(val=NULL, lines=list())
  
  l1 <- list(
    type='line',
    x0 = lines_data$x0[2], 
    x1 = lines_data$x1[2],
    y0 = lines_data$y0[2],
    y1 = lines_data$y1[2],
    xref='x',yref='y',
    line=list(color='red',width=0.5))
  l2 <- list(
    type='line',
    x0 = lines_data$x0[1], 
    x1 = lines_data$x1[1],
    y0 = lines_data$y0[1],
    y1 = lines_data$y1[1],
    xref='x',yref='y',
    line=list(color='green',width=0.5))
  
  test_lines <- list(l1, l2)
  i <- length(test_lines)+1
  test_lines[[i]] <- list(
    type='line',
    x0 = lines_data$x0[1], 
    x1 = lines_data$x1[1],
    y0 = lines_data$y0[2],
    y1 = lines_data$y1[1],
    xref='x',yref='y',
    line=list(color='blue',width=0.5))
  
  lines <- reactive({
    print("re-getting lines")
    print(length(values$lines))
    return(values$lines)
  })

  reactiveMaster <- reactive({
    dat <- w.ws %>% 
      filter(Date >= input$inDateRange[1] & Date < input$inDateRange[2])
    return(dat)
  })
  
  p <- reactive({
    reactiveMaster()
  })
  
  output$p <- renderPlotly({
    plot_ly(p(), 
            x=~Date,
            type='candlestick',
            open=~Open,
            high=~High,
            low=~Low,
            close=~Settle) %>%
      layout(
        shapes = lines()) %>%
      config(editable = TRUE)
  })
  
  output$info <- renderPrint({
    event_data("plotly_relayout")
  })
  output$clicks <- renderPrint({
    event_data("plotly_click")
  })
  
  observeEvent(input$addLine, {
    print("manually adding line")
    i <- length(values$lines)+1
    values$lines[[i]] <- list(
      type='line',
      x0 = input$lineDrawDateRange[1], 
      x1 =input$lineDrawDateRange[2],
      y0 = input$y0,
      y1 = input$y1,
      xref='x',yref='y',
      line=list(color='green',width=0.5))
  })
  
  observeEvent(input$addLineDrag, {
    # wait for first  click
    # wait for second click
    # draw dat line
  })
  
}

shinyApp(ui, server)
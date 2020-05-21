library(shiny)

ui <- fluidPage(
  sidebarPanel(
    dateRangeInput("inDateRange", "Date range input:", 
                   start="2018-05-19",
                   end="2019-05-19"),
    verbatimTextOutput("info"),
    verbatimTextOutput("clicks"),
    h3("manual line entry:"),
    selectInput("line_color", "Line Color:",
                c("Green" = "green",
                  "Red" = "red",
                  "Blue" = "blue",
                  "Black" = "black"),
                selected="Green"),
    dateRangeInput("lineDrawDateRange", "Date range input:", 
                   start="2018-05-19",
                   end="2019-05-19"),
    numericInput("y0", label="y0", value=3),
    numericInput("y1", label="y1", value=3),
    actionButton("addLine", "Add Line Manually"),
    actionButton("addLineDrag", "Add line by clicking")
  ),
  mainPanel(
    plotlyOutput("p"),
    plotlyOutput("figs"),
    plotlyOutput("figp"),
    plotlyOutput("figs10"),
    plotlyOutput("figp10")
  )
)

server <- function(input, output, session) {
  load('w.ws.RData')

  values <- reactiveValues(val=NULL, lines=list())
  
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
  # TODO make this inputable
  j=2600
  k=2700
  output$figs <- renderPlotly({
    us10y.p$cwc$f.d5.w[j:k] %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d5.suw,
            type='scatter',
            mode='lines+markers') 
  })
  output$figp <- renderPlotly({
    us10y.p$cwc$f.d5.w[j:k]  %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d5.prw,
            type='scatter',
            mode='lines+markers')
  })
  output$figs10 <- renderPlotly({
    us10y.p$cwc$f.d10.w[j:k]  %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d10.suw,
            type='scatter',
            mode='lines+markers')
  })
  output$figp10 <- renderPlotly({
    us10y.p$cwc$f.d10.w[j:k]  %>%
      plot_ly(x=w.ws[j:k,Date],
            y=~us10y.f.d10.prw,type='scatter',
            mode='lines+markers')
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
      line=list(color=input$line_color,width=0.5))
  })
  
  observeEvent(input$addLineDrag, {
    # wait for first  click
    # wait for second click
    # draw dat line
  })
  
}

shinyApp(ui, server)
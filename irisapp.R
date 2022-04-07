library(shiny)
source("iris.R")

ui <- fluidPage(
  titlePanel("Retrieving most similar Iris flowers"),
  sidebarLayout(
    sidebarPanel(
      numericInput("sepal.length.input", "Sepal length in cm", value = NA),
      numericInput("sepal.width.input", "Sepal width in cm", value = NA),
      numericInput("petal.length.input", "Petal length in cm", value = NA),
      numericInput("petal.width.input", "Petal width in cm", value = NA),
      actionButton("getResult", "Get top 10 similar flowers"),
      plotOutput('errorMessage')
    ),
    mainPanel(
      h3(textOutput("tableHeader")),
      h4(htmlOutput("inputDimensions")),
      tableOutput("result")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$getResult, {
    output$errorMessage <- renderPlot({
      validate(
        need(isTruthy(input$sepal.length.input), "Please input the sepal length."),
        need(input$sepal.width.input!="", "Please input the sepal width"),
        need(input$petal.length.input!="", "Please input the petal length."),
        need(input$petal.width.input!="", "Please input the petal width")
      )
    })
    
    req(input$sepal.length.input, input$sepal.width.input, input$petal.length.input, input$petal.width.input)
    flower <- c(input$sepal.length.input, input$sepal.width.input, input$petal.length.input, input$petal.width.input)
    
    output$inputDimensions <- renderText({
      paste0("Input dimensions: <br>", 
             "sepal length <font color=\"#FF0000\"><b>", flower[1], "</b></font>cm, <br>",
             "sepal width <font color=\"#FF0000\"><b>", flower[2], "</b></font>cm, <br>",
             "petal length <font color=\"#FF0000\"><b>", flower[3], "</b></font>cm, <br>",
             "petal width <font color=\"#FF0000\"><b>", flower[4], "</b></font>cm")
    })
    output$tableHeader <- renderText({
      paste0("10 similar Iris flowers")
    })
    
    output$result <- renderTable({
      dataset <- getTop10(flower)
      dataset
    })

  })
  
  
  
}

shinyApp(ui, server)



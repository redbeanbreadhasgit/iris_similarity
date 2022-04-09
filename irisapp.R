library(shiny)
library(shinyjs)
library(shinyvalidate)
library(shinydashboard)
source("iris.R")

ui <- dashboardPage(
  dashboardHeader(title="Iris"),
  dashboardSidebar(
    sidebarMenu(
      #https://fontawesome.com/icons?d=gallery
      menuItem(text="Welcome", tabName = "welcome", icon = icon("door-open")),
      menuItem(text="10 Similar Data Points", tabName = "similarity", icon = icon("table")),
      menuItem(text="PCA", tabName = "pca", icon = icon("check"))
    )
  ),
  dashboardBody(
    tabItems(tabItem(tabName = "welcome",
                     h2("View 10 similar datapoints or visualise classes of Iris dataset"),
                     p("The '10 Similar Data Points' tab allows you to view the top 10 most similar datapoints to your input flower dimensions."),
                     p("The 'PCA' tab allows you to visualise the Iris dataset by their classes, as well as your input dimensions.")
                     ),
             tabItem(tabName = "similarity",
                     sidebarPanel(
                       h2("10 Similar Iris Flowers"),
                       selectInput("method", label = "Select Similarity Method", choices = c(getDistMethods())),
                       numericInput("sepal.length.input", "Sepal length in cm", value = NA),
                       numericInput("sepal.width.input", "Sepal width in cm", value = NA),
                       numericInput("petal.length.input", "Petal length in cm", value = NA),
                       numericInput("petal.width.input", "Petal width in cm", value = NA),
                       actionButton("getResult", "Get top 10 similar flowers"),
                       actionButton("resetInput", "Reset inputs"),
                       h4(htmlOutput("inputDimensions"))
                     ),
                     mainPanel(
                       tableOutput("result"),
                       plotOutput("ggplot")
                     )
                     ),
             tabItem(tabName = "pca",
                     sidebarPanel(
                       h2("Use PCA to visualise class"),
                       numericInput("sepal.length.input.pca", "Sepal length in cm", value = NA),
                       numericInput("sepal.width.input.pca", "Sepal width in cm", value = NA),
                       numericInput("petal.length.input.pca", "Petal length in cm", value = NA),
                       numericInput("petal.width.input.pca", "Petal width in cm", value = NA),
                       actionButton("getResult.pca", "Get PCA plot"),
                       actionButton("resetInput.pca", "Reset inputs")
                     ),
                     mainPanel(
                       h4(htmlOutput("inputDimensions.pca")),
                       plotOutput("ggplot.pca")
                     )
             )
             )

  )
)

server <- function(input, output, session) {
  iv <- InputValidator$new()
  iv$add_rule("sepal.length.input", sv_required())
  iv$add_rule("sepal.width.input", sv_required())
  iv$add_rule("petal.length.input", sv_required())
  iv$add_rule("petal.width.input", sv_required())
  iv$add_rule("sepal.length.input.pca", sv_required())
  iv$add_rule("sepal.width.input.pca", sv_required())
  iv$add_rule("petal.length.input.pca", sv_required())
  iv$add_rule("petal.width.input.pca", sv_required())
  
  iv$enable()
  
  observeEvent(input$getResult, {
    req(input$sepal.length.input, input$sepal.width.input, input$petal.length.input, input$petal.width.input)
    a <- input$sepal.length.input
    b <- input$sepal.width.input
    c <- input$petal.length.input
    d <- input$petal.width.input
    input_method <- input$method
    
    dataset <- getTop10_distance(a,b,c,d,input_method)
    
    sepalgroup <- dataset %>% select(sepal.length, sepal.width, class) %>%
      rename(Length=sepal.length, Width=sepal.width) %>% 
      mutate(Part="Sepal")
    
    petalgroup <- dataset %>% select(petal.length, petal.width, class) %>%
      rename(Length=petal.length, Width=petal.width) %>% 
      mutate(Part="Petal")
    
    irisgroups <- rbind(sepalgroup, petalgroup)
    
    plot <- ggplot(data=irisgroups, mapping=aes(x=Length, y=Width, color=class))+
      geom_point(size=3) + facet_wrap(~Part, ncol=2) + theme(text = element_text(size=15))
    
    output$inputDimensions <- renderText({
      paste0("<u><b>Chosen method:</u></b><br>",
             input_method, "<br><br>",
             "<u><b>Input dimensions:</u></b><br>", 
             "sepal length <font color=\"#FF0000\"><b>", a, "</b></font>cm, <br>",
             "sepal width <font color=\"#FF0000\"><b>", b, "</b></font>cm, <br>",
             "petal length <font color=\"#FF0000\"><b>", c, "</b></font>cm, <br>",
             "petal width <font color=\"#FF0000\"><b>", d, "</b></font>cm<br><br>")
    })
    
    output$result <- renderTable({
      dataset
    })
    
    output$ggplot <- renderPlot({
      plot
    })

  })
  
  observeEvent(input$getResult.pca, {
    req(input$sepal.length.input.pca, input$sepal.width.input.pca, input$petal.length.input.pca, input$petal.width.input.pca)
    a <- input$sepal.length.input.pca
    b <- input$sepal.width.input.pca
    c <- input$petal.length.input.pca
    d <- input$petal.width.input.pca

    plot <- getPCAplot(a,b,c,d)
    # plot <- ggplot(data=dataset, mapping=aes(x=PC1, y=PC2)) + geom_point(aes(color=class),size=3) + theme(text = element_text(size = 20))
  
    output$inputDimensions.pca <- renderText({
      paste0("<u><b>Input dimensions:</u></b><br>", 
             "sepal length <font color=\"#FF0000\"><b>", a, "</b></font>cm, <br>",
             "sepal width <font color=\"#FF0000\"><b>", b, "</b></font>cm, <br>",
             "petal length <font color=\"#FF0000\"><b>", c, "</b></font>cm, <br>",
             "petal width <font color=\"#FF0000\"><b>", d, "</b></font>cm<br><br>")
    })
    
    output$ggplot.pca <- renderPlot({
      plot
    })
    
  })
  
  observeEvent(input$resetInput, {
    updateNumericInput(session, "sepal.length.input", value="")
    updateNumericInput(session, "sepal.width.input", value="")
    updateNumericInput(session, "petal.length.input", value="")
    updateNumericInput(session, "petal.width.input", value="")
  })
  
  observeEvent(input$resetInput.pca, {
    updateNumericInput(session, "sepal.length.input.pca", value="")
    updateNumericInput(session, "sepal.width.input.pca", value="")
    updateNumericInput(session, "petal.length.input.pca", value="")
    updateNumericInput(session, "petal.width.input.pca", value="")
  })

}

shinyApp(ui, server)



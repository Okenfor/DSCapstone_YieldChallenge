#' @import shiny methods
#' @export
hc_cluster <- function(names, datasets) {
  
  shinyApp(
    
    ui = fluidPage(responsive = FALSE,
                   fluidRow(style = "padding-bottom: 20px;",
                            column(4, selectInput('xcol', 'Data set', names)),
                            column(4,sliderInput("slider1", label = h3("Sliders"),
                                                 min = 0, max = 100, value = 50))
                            column(4, sliderInput('ycol', 'Number of clusters', dataset[nam],
                                                  selected=names(dataset)[[2]])),
                            column(4, numericInput('clusters', 'Cluster count', 3,
                                                   min = 1, max = 9))
                   ),
                   fluidRow(
                     plotOutput('kmeans', height = "400px")
                   )
    ),
    
    server = function(input, output, session) {
      
      # Combine the selected variables into a new data frame
      selectedData <- reactive({
        dataset[, c(input$xcol, input$ycol)]
      })
      
      clusters <- reactive({
        kmeans(selectedData(), input$clusters)
      })
      
      output$kmeans <- renderPlot(height = 400, {
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
      })
    },
    
    options = list(height = 500)
  )
}
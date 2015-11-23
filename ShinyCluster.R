library(shiny)
library(proxy)
library(cluster)

hierarchicalCluster <- function(m_norm, distM, clustM){
  ### this is going to take 4-ever (O(n^2))
  distances <- dist(t(m_norm), method=distM) ##warning: t() is the transpose function, we look for clustering words not documents
  hclust(d=distances, method=clustM)
}

getCounts <- function(df.terms){
  df.terms.freqs <- colSums(df.terms)
  df.terms <- df.terms[,names(df.terms.freqs[df.terms.freqs>3])]
  m <- as.matrix(df.terms)
  rownames(m) <- 1:nrow(m)
  
  ### don't forget to normalize the vectors so Euclidean makes sense
  norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
  norm_eucl(m)
}

hc_cluster <- function(datasets) {
  
  shinyApp(
    
    ui = fluidPage(responsive = FALSE,
            fluidRow(style = "padding-bottom: 20px;",
                   column(4, selectInput('dataset', 'Data set', names(datasets)),
                   column(4, selectInput('distM', 'Distance method', c("cosine","euclidean"))),
                   column(4, selectInput('clustM', 'Clustering method', c("average", "ward.D2")))
            ),
            fluidRow(style = "padding-bottom: 20px;",
                     column(4, sliderInput('k', 'Number of clusters',
                                           min = 2, max = 10, value = 5)),
                     column(4, sliderInput("minFreqTerms", label = h3("Min. frequency of terms"),
                                           min = 1, max = 100, value = 3)),
                     column(4, numericInput('clusters', 'Cluster count', 3,
                                            min = 1, max = 9))
            ),
            fluidRow(
              plotOutput('tree', height = "400px")
            )
         )
    ),
    
    server = function(input, output, session) {
      
      reactive({
        summary(datasets[input$dataset])
      }
      )
      
      # Combine the selected variables into a new data frame
      selectedData <- reactive({
        datasets[input$dataset]
      })
      
      clusters <- reactive({
        hierarchicalCluster(getCounts(selectedData()), input$distM, input$clustM)
      })
      
      output$tree <- renderPlot(height = 400, {
        hc <- clusters()
        par(mar = c(5.1, 4.1, 0, 1))
        plot(hc, hang=-1, main = input$dataset)
        groups <- cutree(hc, k=input$clusters)   # "k=" defines the number of clusters you are using
        rect.hclust(hc, k=input$clusters, border="red") # draw dendogram with red borders around the 5 clusters   
        
        #cl <- cutree(hc, 30)
        #table(cl)
        
      })
    },
    
    options = list(height = 500)
  )
}
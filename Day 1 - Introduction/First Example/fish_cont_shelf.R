#from https://doi.pangaea.de/10.1594/PANGAEA.900866
library(shiny)
url <- url("https://raw.githubusercontent.com/zsiders/BuildingRShinyCourse/refs/heads/main/Day%201%20-%20Introduction/First%20Example/TraitCollectionFishNAtlanticNEPacificContShelf.csv")
fish <- read.csv(url)
nvars <- colnames(fish)[sapply(fish,is.numeric)]
gvars <- c('family','habitat','feeding.mode','body.shape','fin.shape','spawning.type')

ui <- pageWithSidebar(
  headerPanel('N. Atlantic and NE Pacific continental shelf fish k-means clustering'),
  sidebarPanel(
    selectizeInput('covs', 'Traits', nvars,
                selected = nvars[1:2],
                multiple = TRUE),
    numericInput('clusters', 'Number of Clusters',
                 3, min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output, session) {

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    na.omit(fish[, input$covs])
  })

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })

  output$plot1 <- renderPlot({
    palette(viridisLite::viridis(length(clusters()$centers)))

    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })

}

shinyApp(ui = ui, server = server)
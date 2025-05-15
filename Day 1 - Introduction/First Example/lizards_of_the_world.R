#from https://datadryad.org/dataset/doi:10.5061/dryad.f6t39kj#usage
  library(shiny)
  library(bslib)
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  url <- url("https://github.com/zsiders/BuildingRShinyCourse/raw/refs/heads/main/Day%201%20-%20Introduction/First%20Example/Meiri_Lizard_traits.csv")
  lizard <- read.csv(url)

  nvars <- colnames(lizard)[sapply(lizard,is.numeric)]
  #clean up names
  nvars <- tools::toTitleCase(gsub("\\.|\\.\\."," ",nvars))
  nvars <- gsub('Tb','Body Temp.',nvars)
  nvars[5] <- 'Body Allometry'
  colnames(lizard)[sapply(lizard,is.numeric)] <- nvars
  nvars <- nvars[-c(1:4)]
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
ui <- page_sidebar(
  title = 'Traits of lizards of the world: k-means clustering',
  sidebar = sidebar(
    selectInput('xcol', 'First Trait', nvars,
                selected = nvars[1]),
    selectInput('ycol', 'Second Trait', nvars,
                selected = nvars[9]),
    numericInput('clusters', 'Number of Clusters',
                 3, min = 1, max = 9)
  ),
  card(
    card_header("Bivariate k-means"),
    plotOutput('plot1')
  )
)
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
server <- function(input, output, session) {

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    na.omit(lizard[, c(input$xcol, input$ycol)])
  })

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })

  output$plot1 <- renderPlot({
    palette(viridisLite::viridis(nrow(clusters()$centers)))

    par(mar = c(5.1, 4.1, 0, 1),
        cex.axis = 1.2)
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3, las = 1)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4,
           col = 'red')
  })

}
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
shinyApp(ui = ui, server = server)
#from https://doi.pangaea.de/10.1594/PANGAEA.900866


ui <- fluidPage(

	vars <- setdiff(names(iris), "Species")

	pageWithSidebar(
	  headerPanel('N. Atlantic and NE Pacific continental shelf fish k-means clustering'),
	  sidebarPanel(
	    selectInput('xcol', 'X Variable', vars),
	    selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
	    numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
	  ),
	  mainPanel(
	    plotOutput('plot1')
	  )
	)
)

server <- function(input, output, session) {

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
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
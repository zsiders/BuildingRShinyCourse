library(shiny)
library(bslib)

ui <- page_fluid(
	actionButton('goButton','GO'),
	verbatimTextOutput('io')
)
server <- function(input, output, session){
	output$io <- renderPrint({input$goButton})
}
shinyApp(ui = ui, server = server)
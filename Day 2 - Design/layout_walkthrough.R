library(shiny)
library(bslib)

ui <- page_fluid(
  layout_columns(
 	card('Card 1'),
 	card('Card 2',
 	     layout_columns(
            card('2.1',
                 layout_sidebar(
                   fillable = TRUE,
                   sidebar=sidebar(
                           actionButton("btn", "A button")
                                   )
                )),
            card('2.2'),
            col_widths = c(6,6)
            )
 	     ),
 	col_widths=c(4,8)
	),
  layout_column_wrap(
     card('Card 3',
          plotOutput('plot1')),
     card('Card 4'),
     card('Card 5'),
     card('Card 6')
  )
)

server <- function(input,output){
	output$plot1 <- renderPlot({
		 palette(c('dodgerblue1','firebrick1','darkorchid1'))

		plot(iris[,1],iris[,2],pch=16,
		     col = iris$Species)
	})
}

shinyApp(ui = ui, server = server)
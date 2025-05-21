library(shiny)
library(bslib)
library(plotly)
library(ggplot2)

ui <- page_fluid(
  layout_columns(
 	card(
 	     card_header(
		tooltip(
		  span("Card 1 ", bsicons::bs_icon("question-circle-fill")),
		  "This is the iris dataset",
		  placement = "right"
		)
		),
		plotlyOutput('plot1')
	),
 	card('Card 2',
 	     layout_columns(
            card('2.1',
                 actionButton("btn", "A button")
                 ),
            card('2.2',
                 plotlyOutput('plot2')
                 ),
            col_widths = c(4,8)
            )
 	     ),
 	col_widths=c(6,6)
	),
  layout_column_wrap(
     card('Card 3',
          tooltip(
          	plotOutput('plot3',
                     hover = hoverOpts(id = 'plot3_hover')),
          	'This is a plot of the iris dataset',
          	placement = 'right')
          ),
     card('Card 4',
          tableOutput('plot3_table')
          )
  )
)

server <- function(input,output,session){
	output$plot1 <- renderPlotly({
		g <- ggplot(iris, aes(x=Sepal.Length, y = Sepal.Width, col = Species))  
		p <-  g + geom_point()
		ggplotly(p)
	})
	output$plot2 <- renderPlotly({
		g <- ggplot(iris, aes(Species))  
		p <-  g + geom_bar()

		ggplotly(p)
	})
	output$plot3 <- renderPlot({
		palette(c('dodgerblue1','firebrick1','darkorchid1'))

		plot(iris[,1],iris[,2],pch=16,
		     col = iris$Species)
	})
	output$plot3_table <- renderTable({
	    	req(input$plot3_hover)
	    	nearPoints(df = iris, 
	    	           xvar = 'Sepal.Length',
	    	           yvar = 'Sepal.Width',
	    	           coordinfo = input$plot3_hover)
	})
	observeEvent(input$btn,{
		showModal( 
	      modalDialog( 
	        title = "Somewhat important message", 
	        easy_close = TRUE, 
	        "This is your important message." 
	      ) 
	    ) 
	})
}

shinyApp(ui = ui, server = server)
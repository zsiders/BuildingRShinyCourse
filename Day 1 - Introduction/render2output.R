library(shiny)
library(bslib)
library(DT)
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
ui <- page_fluid(
  	card(
  	     card_header('Plot'),
  	     plotOutput('plot')
  	),
  	card(
  	     card_header('Plot as image'),
  	     imageOutput('img')
  	),
  	card(
  	     card_header('Print'),
  	     verbatimTextOutput('print')
  	),
  	card(
  	     card_header('Text'),
  	     textOutput('text')
  	),
  	card(
  	     card_header('Table'),
  	     tableOutput('tab')
  	),
  	card(
  	     card_header('DataTable'),
  	     DTOutput('dt')
  	)
)
server <- function(input, output, session){
	#plot
	output$plot <- renderPlot({
	   palette(c('dodgerblue1',
	             rgb(97,37,155,maxColorValue=255),
	             rgb(110,88,212,maxColorValue=255)))
	   plot(iris[,1:2],
	        col = factor(iris$Species),
	        pch = 16, 
	        xlab = colnames(iris)[1],
	        ylab = colnames(iris)[2],
	        las = 1)
	})
	#static plot with image
	output$img <- renderImage({
	    # A temp file to save the output.
	    # This file will be removed later by renderImage
	    outfile <- tempfile(fileext = '.png')

	    # Generate the PNG
	    png(outfile, width = 400, height = 300)
	    	palette(c('dodgerblue1',
		             rgb(97,37,155,maxColorValue=255),
		             rgb(110,88,212,maxColorValue=255)))
		   	plot(iris[,1:2],
		        col = factor(iris$Species),
		        pch = 16, 
		        xlab = colnames(iris)[1],
		        ylab = colnames(iris)[2],
		        las = 1)
	    dev.off()

	    # Return a list containing the filename
	    list(src = outfile,
	         contentType = 'image/png',
	         width = 400,
	         height = 300,
	         alt = "This is alternate text")
	  }, deleteFile = TRUE)

	#R output
	output$print <- renderPrint({
		paste0("Mean Sepal Length = ", mean(iris[,1]))
	})

	#Same as ^ but as Text output
	output$text <- renderText({
		paste0("Mean Sepal Length = ", mean(iris[,1]))
	})

	#Table output
	output$tab <- renderTable({
      tab <- table(iris$Species)
	})

	#Data table
	output$dt <- DT::renderDT(iris,
      options=list(lengthChange = FALSE)
	)
}

shinyApp(ui = ui, server = server)
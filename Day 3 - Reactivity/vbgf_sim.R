library(shiny)
library(bslib)
library(reactlog)

reactlog_enable()

vbgm_sim <- function(n, Linf = 120, k = 0.2, Lknot = 20, sig = 0.1, min.age = 0, max.age = 15){
	#generate uniform draws of ages
	ages <- runif(n, min.age, max.age)
	#calculate mean growth
	mu <- Linf - (Linf - Lknot)*exp(-k * ages)
	#draw lognormal deviates
	val <- rlnorm(n, log(mu), sig)
	return(data.frame(age = ages, L = val))
}

ui <- page_sidebar(title='Conductor Example',
 	sidebar = sidebar(
 	    card('von Bertalanffy parameters',
		     sliderInput(inputId = 'Linf',
		                 label = 'Linf (avg. max)',
		                 min = 50, max = 150,
		                 value = 120, step = 10),
		     sliderInput(inputId = 'k',
		                 label = 'k (growth rate)',
		                 min = 0.05, max = 0.5,
		                 value = 0.2, step = 0.05),
		     sliderInput(inputId = 'Lknot',
		                 label = 'L0 (size at birth)',
		                 min = 0, max = 32,
		                 value = 20, step = 4)
	    ),
		card(title = 'Selectivity parameters',
		     sliderInput(inputId = 'age',
		                 label = 'Ages to draw',
		                 min = 0, max = 20,
		                 value = c(0,15), step = 1),
		),
		card(title = 'Simulation parameters',
		     sliderInput(inputId = 'samp',
		                 label = '# of Samples',
		                 min = 250, max = 2000,
		                 value = 500, step = 250),
		     sliderInput(inputId = 'sig',
		                 label = 'Variability',
		                 min = 0.05, max = 0.5,
		                 value = 0.1, step = 0.05)
		), width = 350, open = 'always'
	),
 	plotOutput('plot')
)

server <- function(input,output,session){
	sim1 <- reactive({
		vbgm_sim(n = input$samp,
		         Linf = input$Linf,
		         k = input$k,
		         Lknot = input$Lknot,
		         sig = input$sig,
		         min.age = input$age[1],
		         max.age = input$age[2])
	})
	output$plot <- renderPlot({
		plot(sim1()$age,
		     sim1()$L,
		     pch = 16,
		     xlab = 'Age',
		     ylab = 'Length',
		     ylim = c(0,max(sim1()$L)+5),
		     xlim = c(-0.1, max(sim1()$age)+0.1),
		     xaxs='i', yaxs='i', las = 1)
		text(0, max(sim1()$L),
		     "von Bertalanffy growth simulation",
		     font = 4, cex = 1.5,
		     adj = c(0,1))
	})
}

shinyApp(ui = ui, server = server)
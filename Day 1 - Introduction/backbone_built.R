library(shiny)
library(bslib)

url <- url("https://github.com/zsiders/BuildingRShinyCourse/raw/refs/heads/main/Day%201%20-%20Introduction/First%20Example/Meiri_Lizard_traits.csv")
lizard <- read.csv(url)

lizard$Activity.Time <- factor(lizard$Activity.time,
                 levels = c('Diurnal','Nocturnal','Cathemeral'))

acttime <- levels(lizard$Activity.Time)

plotchoices <- c('slope','smallest.clutch','largest.clutch','minimum.mean.Tb','maximum.mean.Tb')
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
ui <- page_sidebar(
   title = "Example App",
   sidebar = sidebar(
     selectInput(inputId = 'active',
                 label = "Choose an Activity Time",
                 choices = acttime),
     selectInput(inputId = 'plotchoice',
                 label = "Choose what to plot",
                 choices = plotchoices)
   ),
   plotOutput('plot1')
)
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
server <- function(input, output, session){
	output$plot1 <- renderPlot({
		#filter by activity time
		tmp <- lizard[lizard$Activity.Time == input$active, ]
		#plot by user choice
		boxplot(tmp[,input$plotchoice])
	})
}

shinyApp(ui = ui, server = server)

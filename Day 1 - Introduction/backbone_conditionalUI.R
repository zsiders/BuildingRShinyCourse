library(shiny)
library(bslib)

url <- url("https://github.com/zsiders/BuildingRShinyCourse/raw/refs/heads/main/Day%201%20-%20Introduction/First%20Example/Meiri_Lizard_traits.csv")
lizard <- read.csv(url)

families <- sort(unique(lizard$Family))
families <- families[which(nchar(families)>1)]

genera <- sort(unique(lizard$Genus))
genera <- genera[which(nchar(genera)>1)]

#lookup
famgen <- aggregate(Genus~Family,
          data = lizard, 
          FUN = unique)
famgen <- famgen[-1,]

plotchoices <- c('slope','smallest.clutch','largest.clutch','minimum.mean.Tb','maximum.mean.Tb')
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
ui <- page_sidebar(
   title = "Example App",
   sidebar = sidebar(
     selectizeInput(inputId = 'family',
                 label = "Choose an Family",
                 choices = families,
                 multiple = TRUE,
                 options = list(maxItems = 3,
                                placeholder = 'Choose')),
     selectizeInput(inputId = 'genus',
                 label = "Choose an Genus",
                 choices = genera,
                 options = list(placeholder = 'Choose')),
     selectizeInput(inputId = 'plotchoice',
                 label = "Choose what to plot",
                 choices = plotchoices)
   )
)
    
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
server <- function(input, output, session){

  observeEvent(input$family,{
    ii <- which(famgen[,1] %in% input$family)
    newgenera <- famgen[ii,2]
    names(newgenera) <- famgen[ii,1]
    updateSelectizeInput(session = session,
                         inputId = 'genus',
                         label = 'Choose a Genus',
                         choices = newgenera,
                         options = list(placeholder = "Choose"))
  })
}

shinyApp(ui = ui, server = server)

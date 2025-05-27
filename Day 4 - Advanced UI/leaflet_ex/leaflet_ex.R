library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)
library(dplyr)
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(sf)
path <- '/Users/zach/Dropbox/Teaching/2025 - FAS 6932 - RShiny/BuildingRShinyCourse/Day 4 - Advanced UI/leaflet_ex/'
setwd(path)
tracks <- st_read("./tracks/panthers.shp")
tracks <- st_transform(tracks,'EPSG:4326')
tracks$ID <- factor(tracks$CatID,
                    levels = unique(tracks$CatID),
                    labels = LETTERS[1:length(unique(tracks$CatID))])
tracks <- tracks %>%
			group_by(ID) %>% 
			count() %>% 
			st_cast("LINESTRING")
tracks$len <- st_length(tracks) %>% units::set_units(km)
tracks$popup <- paste(sep = "<br/>",
  paste0("<b>Panther ",tracks$ID,"</b>"),
  paste0("# of Pings: ",tracks$n),
  paste0("Track Length: ", round(tracks$len)," km")
) 


fancy_names <- lapply(as.list(levels(tracks$ID)),
                      function(x)HTML(paste0("<customLabel>",x,"</customLabel>")))
customLabel <- tag("customLabel", list(class = "customLabel"))

def_bbox <- as.vector(st_bbox(tracks))
# def_cent <- as.vector(st_coordinates(st_centroid(st_union(tracks))))
pal <- colorFactor(rcartocolor::carto_pal(nlevels(tracks$ID)+1,'Pastel')[1:nlevels(tracks$ID)],domain=levels(tracks$ID))


###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#https://shiny.posit.co/r/articles/build/tag-glossary/
ui <- page_fluid(
   tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Potta+One&family=Rubik+Moonrocks&display=swap');
      h1 {
        font-family: 'Rubik Moonrocks', san-serif;
      }
      h3 {
        font-family: 'Potta One', san-serif;
      }
      customLabel {
        font-family: 'Potta One', san-serif;
      }"
      )
    ),               
   h1('Florida Panther Tracks'),
   tagList(
        sidebar_tabs(id = "mysidebarid",
         list(icon('gears'),
              icon('ruler-combined')),
         sidebar_pane(
           title = "Selection", id = "blah_id",
           icon = icon('stroopwafel'),
           tagList(br(),
                   checkboxGroupButtons(
                        inputId = "panther_choice",
			        	label = HTML("<h3><em>Choose an Panther</em></h2>"),
			        	choiceNames = fancy_names,
				        choiceValues = levels(tracks$ID),
				        selected = as.character(levels(tracks$ID)),
				        direction = 'vertical',
				        size = 'lg',
				        justified = TRUE,
				        checkIcon = list(yes=icon("ok-sign",lib = "glyphicon"))
			        )
             	)
         ),
         sidebar_pane(
           title = "Statistics", id = "home_id",
           icon = icon('stroopwafel'),
           tagList(
             br(),
             uiOutput("pings"),
             uiOutput("dist")
           )
         )
	 )),
   leafletOutput("map_plot",width = "100%", height = "600px")
   # verbatimTextOutput('bug')
)
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
server <- function(input, output, session){
	filt_tracks <- reactive({
		if(length(input$panther_choice)==0) return(tracks)
		tracks[tracks$ID %in% input$panther_choice,]
	})

	output$map_plot <- renderLeaflet(
        leaflet(data=tracks) %>%
            addProviderTiles('Esri.WorldImagery') %>%
                fitBounds(lng1 = def_bbox[1],
                          lng2 = def_bbox[3],
                          lat1 = def_bbox[2],
                          lat2 = def_bbox[4]) %>%
                addSidebar(
			        id = "mysidebarid",
			        options = list(position = "left")
			      ) %>% 
                addLegend(pal = pal,
                          values = ~ID, 
                          opacity = 0.7, 
                          title = "Panthers", 
                          position = "bottomright") %>%
                addPolylines(color = ~pal(tracks$ID),
                         opacity = 0.8,
                         popup = ~popup,
                         highlightOptions = highlightOptions(
						    weight = 8,
						    opacity = 1,
						    bringToFront = TRUE))
            
    )
    # output$bug <- renderPrint(input$panther_choice)
    observe({
        leafletProxy("map_plot", data = filt_tracks()) %>%
        	clearShapes() %>%
            addPolylines(color = ~pal(filt_tracks()$ID),
                         opacity = 0.8,
                         popup = ~popup,
                         highlightOptions = highlightOptions(
						    weight = 8,
						    opacity = 1,
						    bringToFront = TRUE))
    })
    ping_plot <- renderPlot({
    	par(mar=c(4,4,1,1))
    	barplot(n~ID, data = filt_tracks(), las = 1,
    	        ylab = '# of Pings',
    	        col = pal(filt_tracks()$ID))
    })
    len_plot <- renderPlot({
    	par(mar=c(4,4,1,1))
    	barplot(len~ID, data = filt_tracks(), las = 1,
    	        ylab = 'Length of Track (km)',
    	        col = pal(filt_tracks()$ID))
    })
    output$pings <- renderUI({
    	if(length(input$panther_choice)==1){
    		return(
    		       tagList(
    		               value_box(
			    title = h1(paste0('Panther ',input$panther_choice)),
			    value = filt_tracks()$n,
			    showcase = bsicons::bs_icon("wifi"),
			    theme = value_box_theme(bg = pal(filt_tracks()$ID)),
			    p("# of Pings")
			  ))
	        )
    	}else{
    		return(tagList(card(ping_plot,max_height = "250px")))
    	}
    })
    output$dist <- renderUI({
    	if(length(input$panther_choice)==1){
    		return(
    		       tagList(
    		               value_box(
			    title = h1(paste0('Panther ',input$panther_choice)),
			    value = round(filt_tracks()$len),
			    showcase = bsicons::bs_icon("rulers"),
			    theme = value_box_theme(bg = pal(filt_tracks()$ID)),
			    p("Length of Track (km)")
			  ))
	        )
    	}else{
    		return(tagList(card(len_plot,max_height = "250px")))
    	}
    })
}
shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)
library(gplots)

#read in
	spp_list <- read.csv('rockfish.csv')
	spp_list <- spp_list[spp_list$Species %in% c('China','Canary','Tiger','Quillback'),]
	spp_list$btn <- paste0(spp_list$Species, '_btn')
	spp_list$tab <- paste0(spp_list$Species, '_tab')
	spp_list$bgcol <- gplots::col2hex(spp_list$bgcol)
	spp_list$col <- gplots::col2hex(spp_list$col)
###-----------------------------------------------------
#		Dynamic UI
###-----------------------------------------------------
	spp_card_list <- list()
	spp_tab_list <- list()
	for (i in 1:nrow(spp_list)){
		spp_card_list[[i]] <- card(
		              card_header(h4(paste(spp_list$Species[i], 'Rockfish')), 
		              	style = paste0("display:inline; color:",
		              	               spp_list$col[i])),
		              #change to src and delete "./www/" for web hosting
		              card_image(file = paste0('./www/',
		                                       spp_list$Species[i],
		                                       ".png"),
		              			height = '150px'),
		              actionButton(inputId = spp_list$btn[i], 
		                           label = h5('Details'),
		                           width = '200px',
		                           style = 'background:black; color:white'),
		              style = paste0('background:',spp_list$bgcol[i]),
		                max_height = '400px',
		                class = 'm-4'
		        )
		spp_tab_list[[i]] <- nav_panel(title = tags$h4(paste(spp_list$Species[i], 'Rockfish')),	card(
		                 p("Species details"),
		                 actionButton(inputId = paste0(spp_list$Species[i], '_btnclose'), label = h5('Close'),
		                              width = '200px',
		                              style = 'background:black; color:white'),
		                 style = 'width: 300 px'),
						value = spp_list$tab[i]
						)
		                 
	}
	names(spp_tab_list) <- spp_list$Species
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

ui <- page_fillable(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Jersey+10&family=Silkscreen:wght@400;700&family=Teko:wght@300..700&display=swap');
      h1 {
        font-family: 'Jersey 10', san-serif;
      }
      h4 {
        font-family: 'Teko', serif;
        font-weight: 700;
      }
      h5 {
        font-family: 'Silkscreen', san-serif;
        font-weight: 400;
      }"
      )
    ),
	navset_tab(
	    id = 'tabcont',
		nav_panel(title = h1("Species"),
		          layout_columns(
                     fluidRow(uiOutput('spp_cards'),
                     style='margin-left:auto;margin-right:auto'
                  )),
		          value = 'tab1'),
		nav_panel_hidden(value = 'tab2')
	)	
)
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
server <- function(input, output, session){
	output$spp_cards <- renderUI(spp_card_list)
	# output$spp_tabs <- renderUI(spp_tab_list)

	observeEvent(input$Canary_btn,
        nav_insert(id = "tabcont",
                   target = 'tab2',
                   select = TRUE,
                   nav = spp_tab_list$Canary)
    )
    observeEvent(input$China_btn,
        nav_insert(id = "tabcont",
                   target = 'tab2',
                   select = TRUE,
                   nav = spp_tab_list$China)
    )
    observeEvent(input$Quillback_btn,
        nav_insert(id = "tabcont",
                   target = 'tab2',
                   select = TRUE,
                   nav = spp_tab_list$Quillback)
    )
    observeEvent(input$Tiger_btn,
        nav_insert(id = "tabcont",
                   target = 'tab2',
                   select = TRUE,
                   nav = spp_tab_list$Tiger)
    )
    observeEvent(input$Canary_btnclose,
        nav_remove(id = "tabcont",
                   target = 'Canary_tab')
    )
    observeEvent(input$China_btnclose,
        nav_remove(id = "tabcont",
                   target = 'China_tab')
    )
    observeEvent(input$Quillback_btnclose,
        nav_remove(id = "tabcont",
                   target = 'Quillback_tab')
    )
    observeEvent(input$Tiger_btnclose,
        nav_remove(id = "tabcont",
                   target = 'Tiger_tab')
    )
}

shinyApp(ui = ui, server = server)

library(shinydashboard)
library(wordcloud2)

dashboardPage(
    dashboardHeader(title = "JairG's Text Predictor"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Executive Presentation", tabName = "presentation", icon = icon("desktop")),
            menuItem("Documentation", tabName = "documentation", icon = icon("file-text"))
        )),
    dashboardBody( 
        tabItems(
            tabItem(tabName="dashboard",
                    fluidRow(
                        box(footer="Note: The algorithm will complete the word you are typing until you input a space, in which case it will predict the next word.",title=strong(h1("Next Predicted Words")), width=12, wordcloud2Output("predicted"),
                            br(),
                            textInput(placeholder = "Start typing, be original", inputId = "textid", label='Type Here')
                        )
                    )
            ),
            tabItem(tabName="presentation",
                    box(footer="Click inside the presentation and use your keyboard arrows to go forward", status = "info",width=12,
                    tags$iframe(src="Pitch.html", style="height:600px; width:100%", frameborder=0))
                    
            ),
            
            tabItem(tabName="documentation",
                    
                    box(status = "info",width=12,
                        tags$iframe(src="Complete_Documentation.nb.html", style="height:850px; width:100%", frameborder=0, scrolling="yes"))
                        #htmlOutput("inc"))
            )
        )
    )
)



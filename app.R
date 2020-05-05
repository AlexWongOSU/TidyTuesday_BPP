#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Lahman)
library(dplyr)
ModernTeams<- Lahman::Teams%>%
    filter(yearID>=1960)

# Define UI for application
ui <- fluidPage(
selectInput(inputId = "Type",
            label = "Statistic",
            choices = names(ModernTeams[,c(9,10,15:40)])),
selectInput(inputId = "team",
            label = "Team",
            choices = unique(ModernTeams$name)),

        

        mainPanel(
           plotOutput("statPlot"),
           tags$a(href  = "http://www.seanlahman.com/baseball-archive/statistics/", "Data Source: Sean Lahman's Baseball Database")
        )
)

# Define server logic
server <- function(input, output) {
    
    SelectedTeams<- reactive({
        ModernTeams%>%
            filter(name == input$team)%>%
            select(yearID, input$Type)
    })

    output$statPlot <- renderPlot({
       plot(x=SelectedTeams()[[1]], y=SelectedTeams()[[2]], type = "l", xlab = "Season Year", ylab = input$Type)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)

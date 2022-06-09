#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 visualisation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(population_density,
                        total_deaths_per_million,
                        location))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput(outputId = "plot_population_density_eva")
        )
    )


# Define server logic required to draw a histogram
server <- function(input,output) {
  output$value <- renderPrint({input$select})
  
  output$plot_population_density_eva <- renderPlotly({
    plot <- ggplot(data = covid_A6_eva) +
      geom_point(aes(
        x = location,
        y = population_density,
        color = total_deaths_per_million
      )
      )
    ggplotly(plot)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

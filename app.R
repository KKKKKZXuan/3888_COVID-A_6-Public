#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
group1 = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70-79", "80-89","90+")
group2 = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70-79", "80+")
covid_data_age = read.csv("combine_plot.csv")

ui <- fluidPage(

    titlePanel("Plot for Sweden and Romania"),
    
    #sidebarLayout()
        #sidebarPanel( 
            #selectInput("variable", "Variable:",
                       # c("0-9" = "0-9",
                        #  "Transmission" = "10-19",
                        #  "Gears" = "gear")),
        
    mainPanel(
            tabsetPanel(
                tabPanel("Sweden",plotOutput(outputId = "covid_data_Sweden",width = "400px")
                ),
                tabPanel("Romania",plotOutput(outputId = "covid_data_Romania",width = "400px")
              ))
            )
)
    

server <- function(input, output) {

    output$value <- renderPrint({ input$checkGroup })
    
    output$covid_data_Sweden <- renderPlot({
        
        covid_data_age %>%
            filter(Location == "Sweden") %>%
            filter(Date <= "2021-12-31" & Date >= "2020-06-01") %>%
            filter(excelsheet == "PHAS_Data") %>%
            filter(Age_group %in% group1) %>%
            mutate(Month = month(Date)) %>%
            plot_ly(x = ~ Date, 
                    y = ~ Group_deaths_number,
                    color = ~ Age_group, 
                    type = "scatter",
                    mode = "line") %>%
            layout(title = "COVID-19- Deaths by age group in the Sweden in 2021",
                   yaxis = list(title = "Number of Deaths"),
                   xaxis = list(title = "Date"))
    })
    
    
    output$covid_data_Sweden <- renderPlot({
        covid_data_age %>%
            filter(Location == "Romania") %>%
            filter(Date <= "2021-12-31" & Date >= "2020-06-01") %>%
            filter(Age_group %in% group2) %>%
            mutate(Month = month(Date)) %>%
            plot_ly(x = ~ Date, 
                    y = ~ Group_deaths_number,
                    color = ~ Age_group, 
                    type = "scatter",
                    mode = "line") %>%
            layout(title = "COVID-19- Deaths by age group in the Romania in 2021",
                   yaxis = list(title = "Number of Deaths"),
                   xaxis = list(title = "Date"))
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

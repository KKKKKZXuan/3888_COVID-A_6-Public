# library ----
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinydashboard)
library(shinyjs)

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  useShinyjs(),
  
  ## App theme ----
  theme = shinytheme("superhero"),
  
  ## App title ----
  titlePanel("Factors and habits between developing and developed countries in COVID-19"),
  
  hr(),
  
  tabsetPanel(
    
    ## TAB-Panel for intro page ----
    tabPanel("Hello",
             
             fluidRow(
               
               column(8,
                      
                      h3("Welcome"),
                      
                      hr(),
                      
                      h4("Research Question"),
                      
                      p("What are certain factors and habits which could 
                        increase the risk of someone developing severe 
                        symptoms of covid and/or death by covid-19 between 
                        developing and developed countries?"),
                      
                      # br(),
                      
                      h4("Purpose"),
                      
                      p("This app is used to determine the possible factors 
                        which could determine whether an individual infected 
                        with the Coronavirus Disease - 2019 (COVID-19) could 
                        possibly develop further severe symptoms or even death.
                        It could be used to determine whether an individual 
                        with underlying diseases or habits should require extra 
                        care in the case that they become infected."),
                      
                      # br(),
                      
                      h4("Usage"),
                      
                      p("Factors which have been researched to determine 
                        whether it has an effect on the severity of a 
                        COVID-19 case includes each country’s ICU 
                        admission rates, each country’s Population Density, 
                        patients with underlying diseases in each country, 
                        Mortality, as well as the patient’s vaccination 
                        status as well as whether the patient of interest is 
                        currently residing in a developed or a 
                        developing country. Users can select which factors 
                        are relevant to themselves in order to discover 
                        relevant information."),
                      
                      hr(),
                      
                      div(
                        HTML("<h4>
                             How to use this web app
                             </h4>
                             <p>
                             1. Choose the part which you are interseted in. <br>
                             2. Choose the range of date on the 'Date range selector'. <br>
                             3. Selected type of 'Countries / Regions'. <br>
                             </p>")
                      ),
                      
                      br(),
                      
                      ),
               
               column(4,
                      
                      div(
                        HTML("<h4>
                             Links
                             </h4>
                             <p>
                             <a href='https://github.sydney.edu.au/zmao0489/3888_COVID-A_6'>
                             Github Repository</a><br>
                             <a href='https://docs.google.com/drawings/d/1Un5NXqsKF9vy32ImFFUzcq4e2NaFLAKlHOT93P7VRh0/edit'>
                             Work Flow</a><br>
                             <a href='https://www.canva.cn/design/DAFBftGJWnk/hER8pe-AqvSNvW6iGj2ICg/edit#'>
                             Slide</a><br>
                             </p>")
                      ),
                      
                      # a(href="https://github.sydney.edu.au/zmao0489/3888_COVID-A_6", "Github"),
                      # a(href="https://docs.google.com/drawings/d/1Un5NXqsKF9vy32ImFFUzcq4e2NaFLAKlHOT93P7VRh0/edit", "Work Flow"),
                      # a(href="https://docs.google.com/presentation/d/1WpSBN6z67BPioud3tl2KvkIoy0mQ15f0b8PPn4pfyi0/edit#slide=id.p", "Google Slide"),

                      hr(),
                      
                      # h4("Collaborators"),
                      
                      # uiOutput("eva_link"),
                      # uiOutput("pat_link"),
                      # uiOutput("amber_link"),
                      # uiOutput("ken_link"),
                      # uiOutput("gary_link"),
                      
                      # a(href="https://zmao0489.shinyapps.io/3888_covid-a-6_shiny", "Shiny APP"),
                      
                      # hr(),
                      
                      # h4("Information"),
                      
                      div(
                        HTML("<h4>
                             Group name:
                             </h4>
                             
                             <p>
                             COVID_A6
                             <br>
                             <br>
                             </p>
                             
                             <h4>
                             Group members:
                             </h4>
                             
                             <p>
                             <a href='https://github.sydney.edu.au/cyin8697'>
                             Changwa-Yin (Eva)</a><br>
                             <a href='https://github.sydney.edu.au/nthe8110'>
                             Pat-Thepyasuwan</a><br>
                             <a href='https://github.sydney.edu.au/zche7728'>
                             Zhanqiu-Chen (Amber)</a><br>
                             <a href='https://github.sydney.edu.au/zmao0489'>
                             Zhekai-Mao (Ken)</a><br>
                             <a href='https://github.sydney.edu.au/zdou2598'>
                             Zhenyao-Dou (Gary)</a>
                             <br>
                             <br>
                             </p>
                             
                             <h4>
                             Aim of your project:
                             </h4>
                             
                             <p>
                             Factors and habits to determine severe symptoms or 
                             death between developing and developed countries
                             in COVID-19
                             <br>
                             <br>
                             </p>
                             
                             <h4>
                             Shiny app link:
                             </h4>
                             
                             <p>
                             <a href='https://zmao0489.shinyapps.io/3888_covid-a-6_shiny'>
                             https://zmao0489.shinyapps.io/3888_covid-a-6_shiny</a>
                             <br>
                             </p>")
                      ),
                      
                      ),
               
               ),
             
             ),
    
    ## TAB-Panel for IDA page ----
    tabPanel("Initial Data Analysis (IDA)",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 dateRangeInput("date_range_00_ida",
                                h4("Date range selector"),
                                min = '2019-06-01',
                                max = '2021-12-31',
                                start = '2019-06-01',
                                end = '2021-12-31'),
                 
                 hr(),
                 
                 h4("Developed / Developing countries table"),
                 
                 actionButton("full_table_develop", label = "Show All"),
                 
               ),
               
               mainPanel(
                 
                 withSpinner(plotlyOutput(outputId = "world_map_develop")),
                 
                 hr(),
                 
                 fluidRow(
                   
                   column(4,
                          
                          h4("Table of developed countries"),
                          
                          withSpinner(tableOutput(outputId = "table_developed_countries")),
                          
                          ),
                   
                   column(4,
                          
                          h4("Table of developing countries"),
                          
                          withSpinner(tableOutput(outputId = "table_developing_countries")),
                          
                          ),
                 ),
                 
                 hr(),
                 
                 h4("Quick look of data"),

                 withSpinner(verbatimTextOutput("skim_data")),
               
               ),
               
               ),
             
             ),
    
    ### Q1 Ken part ----
    tabPanel("ICU",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 dateRangeInput("date_range_01_ken",
                                h4("Date range selector"),
                                min = '2019-06-01',
                                max = '2021-12-31',
                                start = '2019-06-01',
                                end = '2021-12-31'),
                 
                 hr(),
                 
                 ### ----
                 selectInput("select_research_01_ken",
                             h4("Selecte the type of research"),
                             choices = list("Initial Data Analysis" = 1,
                                            "Model" = 2),
                             selected = 1),
                 
                 ### Input: Select the country ----
                 selectInput("select_countries_01_ken",
                             h4("Countries / Regions"),
                             choices = list("Research Counrties" = 1,
                                            "Developed \ Developing Countries" = 2),
                             selected = 2),
                 
                 selectInput("select_data_type_01_ken",
                             h4("Select the type of data"), 
                             choices = list("New Cases" = 1,
                                            "New Deaths" = 2,
                                            "New ICU" = 3,
                                            "ICU Occupancy Rate" = 4), 
                             selected = 3),
                 
                 ),
               
               mainPanel(
                 
                 h4("ICU"),
                 
                 p("Does a higher ICU admission and hospitalization
                   predict a higher mortality rate in developing countries
                   like Malaysia, and Chile compared to developed countries
                   like the United States, and Switzerland?"),
                 
                 hr(),
                 
                 withSpinner(plotlyOutput("plot_icu_ken")),
                 plotOutput("plot_icu_ken_re"),
                 
                 hr(),
                 
                 p("Use the ARIMA model to predict the difference in 
                   the number of new ICUs in developing and developed countries. 
                   Select different data in the IDA to see how they are 
                   trending."),
                 
                 p("More detailed relevant medical data was found using 
                   the source data link in Github via Covid data. 
                   After using Logistic regression and Correlation to 
                   judge the availability of ICU daily increase case data, 
                   the rationality of the data was confirmed 
                   by ARIMA's display function."),
                 
                 p("Between developing and developed countries, 
                   it is not difficult to find that the daily increase 
                   in ICU cases predicted by the ARIMA model is very similar, 
                   and there is no major difference due to 
                   economic conditions.")
                 
               ),
               
             ),
             
             ),
    
    ## Q2 Eva Part ----
    tabPanel("Population Density",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 # dateRangeInput("date_range_02_eva",
                 #                h4("Date range selector"),
                 #                min = '2019-06-01',
                 #                max = '2021-12-31',
                 #                start = '2019-06-01',
                 #                end = '2021-12-31'),
                 
                 selectInput("select_countries_02_eva",
                             h4("Select the country of data"), 
                             choices = list("Developed" = 1,
                                            "Developing" = 2,
                                            "All" = 3), 
                             selected = 3),
                 
               ),
               
               mainPanel(
                 
                 h4("Population Density"),
                 
                 p("Does population density have an influence towards
                   COVID mortality rate in countries such as
                   France, Switzerland compared to a developing country
                   such as India?"),
                 
                 hr(),
                 
                 ### Output: plot of new cases ----
                 withSpinner(plotlyOutput(outputId = "plot_pd_eva")),
                 
                 hr(),
                 
                 p("Through test for association/correlation between 
                   paired samples and chart analysis, it can be found that 
                   developing countries with high population density 
                   do not exhibit high mortality."),
                 
                 p("At the same time, some developed countries with 
                   low population density have experienced high 
                   mortality rates."),
                 
                 p("We can say that there is no clear relationship 
                   between population density and mortality in developing and 
                   developed countries."),
                 
               ),
               
             ),
             
             ),
    
    ## Q3 Pat Part ----
    tabPanel("Diseases",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 # dateRangeInput("date_range_03_pat",
                 #                h4("Date range selector"),
                 #                min = '2019-06-01',
                 #                max = '2021-12-31',
                 #                start = '2019-06-01',
                 #                end = '2021-12-31'),
                 
                 selectInput("select_develop_type_03_pat",
                             h4("Select the type of data"), 
                             choices = list("Developed" = 1,
                                            "Developing" = 2), 
                             selected = 1),
                 
                 selectInput("select_data_type_03_pat",
                             h4("Select the type of data"), 
                             choices = list("Full" = 1,
                                            "None" = 2,
                                            "Diabetes" = 3,
                                            "Aids" = 4,
                                            "Cardiovascular" = 5), 
                             selected = 3),
                 
               ),
               
               mainPanel(
                 
                 h4("Diseases"),
                 
                 p("Does having pre-existing diseases such as
                   diabetes or underlying cardiovascular diseases
                   increase your risk of developing severe symptoms for cases in
                   the U.S. compared to those in India?"),
                 
                 hr(),
                 
                 ### Output: plot of new cases ----
                 
                 withSpinner(plotlyOutput(outputId = "plot_diseases_pat")),
                 
                 withSpinner(verbatimTextOutput(outputId = "text_diseases_lm_pat")),
                 
                 hr(),
                 
                 p("Through the calculation of fitting linear models, 
                   it can be found that there are a large number of patients 
                   with pre-existing diseases among the deaths in 
                   developing countries. This situation is not so obvious in 
                   developed countries."),
                 
                 p("Therefore, we believe that differences in this aspect 
                   may be caused by differences in medical infrastructure and 
                   environmental sanitation in developing countries."),
                 
                 ),
               
               ),
             
             ),
    
    ## Q4 Gary Part ----
    tabPanel("Age",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 dateRangeInput("date_range_04_gary",
                                h4("Date range selector"),
                                min = '2019-06-01',
                                max = '2021-12-31',
                                start = '2019-06-01',
                                end = '2021-12-31'),
                 
                 hr(),
                 
                 selectInput("select_countries_04_gary",
                             h4("Select the country of data"), 
                             choices = list("Developed" = 1,
                                            "Developing" = 2), 
                             selected = 1),
                 
               ),
               
               mainPanel(
                 
                 h4("Age"),
                 
                 p("Does the different age group of patients in 
                   developing countries have an impact on 
                   COVID-19 mortality compared to developed countries?"),
                 
                 hr(),
                 
                 ### Output: plot of new cases ----
                 withSpinner(plotlyOutput(outputId = "plot_age_gary")),
                 
                 # withSpinner(textOutput(outputId = "text_age_gary")),
                 
                 hr(),
                 
                 p("It can be found that the age distribution of mortality 
                   in developing countries is almost the same as that 
                   in developed countries."),
                 
                 p("Although the development speed of the epidemic is different, 
                   it does not affect the age distribution. This is also 
                   reflected in the test for association/correlation 
                   between paired samples."),
                 
               ),
               
               ),
             
             ),
    
    ## Q5 Amber Part ----
    tabPanel("Vaccination",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 dateRangeInput("date_range_05_Amber",
                                h4("Date range selector"),
                                min = '2019-06-01',
                                max = '2021-12-31',
                                start = '2019-06-01',
                                end = '2021-12-31'),
                 
                 hr(),
                 
                 selectInput("select_research_type_05_Amber",
                             h4("Select the country of data"), 
                             choices = list("EDA" = 1,
                                            "Total Cases" = 2,
                                            "ACF" = 3,
                                            "PACF" = 4,
                                            "Forecast" = 5), 
                             selected = 1),
                 
                 selectInput("select_countries_05_Amber",
                             h4("Select the country of data"), 
                             choices = list("-none-" = 1),
                             selected = 1),
                 
               ),
               
               mainPanel(
                 
                 h4("Vaccination"),
                 
                 p("Does vaccination reduce death rates in the population of
                   developed countries compared to developing countries"),
                 
                 hr(),
                 
                 ### Output: plot of new cases ----
                 withSpinner(plotlyOutput(outputId = "plot_aus_mal_vaccinations_amber")),
                 plotOutput(outputId = "plot_aus_mal_vaccinations_amber_re"),
                 
                 hr(),
                 
                 p("The difference between the vaccination and 
                   the mortality rate reflects the difference between 
                   developed and developing countries. The ARIMA model was 
                   also used for analysis, and the validation of ACF and 
                   PACF demonstrated the availability of data."),
                 
                 p("In the final development data, we found that 
                   the predicted mortality data in developed countries is 
                   significantly higher than that in developing countries."),
                 
               ),
               
               ),
             
             ),
    
    ## Summary Page ----
    tabPanel("Summary",
             
             fluidRow(
               
               column(8,
                      
                      h3("Summary"),
                      
                      hr(),
                      
                      p("Our research focused on factors such as an individual’s age, 
                         underlying diseases, and vaccination status as well as 
                         a country’s ICU admissions rate, and its population density. 
                         Additionally these factors may vary whether the individual is 
                         planning to travel to a developing or a developed country. 
                         This was a consideration because the availability and quality of 
                         healthcare varies between countries. Commonly, countries with 
                         a greater human development index, used to determine if a country 
                         is classified as developed or developing, often correlates to 
                         higher healthcare spending (Nuhu, McDaniel,Alorbi & Ruiz, 2018)."),
                      
                      p("In Summary, individuals looking to travel to developing 
                         countries who are over 60 with underlying diseases such as 
                         diabetes or cardiovascular diseases should take extra care of 
                         themselves since they are vulnerable to developing severe 
                         symptoms of Covid. These individuals should consider getting 
                         vaccinated from the virus if they are not already to further 
                         protect themselves from the virus."),
                      
                      br(),
                      
                      p("The relevant conclusions are also mentioned on the 
                        page, ICU, Population Density and Age do not show 
                        particularly obvious differences between developing and 
                        developed countries, but there are more 
                        significant differences in diseases and 
                        vaccination rates."),
                      
                      p("As the COVID-19 pandemic further develops, so too do 
                        the risks of travel. We recommend further observation 
                        of the countries to be visited, instead of making blind 
                        judgments based solely on their economic conditions."),
                      
                      br(),
                      
               ),
               
               column(4,
                      
                      div(
                        HTML("<h4>
                             Links
                             </h4>
                             <p>
                             <a href='https://github.sydney.edu.au/zmao0489/3888_COVID-A_6'>
                             Github Repository</a><br>
                             <a href='https://docs.google.com/drawings/d/1Un5NXqsKF9vy32ImFFUzcq4e2NaFLAKlHOT93P7VRh0/edit'>
                             Work Flow</a><br>
                             <a href='https://www.canva.cn/design/DAFBftGJWnk/hER8pe-AqvSNvW6iGj2ICg/edit#'>
                             Slide</a><br>
                             </p>")
                      ),
                      
                      hr(),

                      div(
                        HTML("<h4>
                             Group name:
                             </h4>
                             
                             <p>
                             COVID_A6
                             <br>
                             <br>
                             </p>
                             
                             <h4>
                             Group members:
                             </h4>
                             
                             <p>
                             <a href='https://github.sydney.edu.au/cyin8697'>
                             Changwa-Yin (Eva)</a><br>
                             <a href='https://github.sydney.edu.au/nthe8110'>
                             Pat-Thepyasuwan</a><br>
                             <a href='https://github.sydney.edu.au/zche7728'>
                             Zhanqiu-Chen (Amber)</a><br>
                             <a href='https://github.sydney.edu.au/zmao0489'>
                             Zhekai-Mao (Ken)</a><br>
                             <a href='https://github.sydney.edu.au/zdou2598'>
                             Zhenyao-Dou (Gary)</a>
                             <br>
                             <br>
                             </p>
                             
                             <h4>
                             Aim of your project:
                             </h4>
                             
                             <p>
                             Factors and habits to determine severe symptoms or 
                             death between developing and developed countries
                             in COVID-19
                             <br>
                             <br>
                             </p>
                             
                             <h4>
                             Shiny app link:
                             </h4>
                             
                             <p>
                             <a href='https://zmao0489.shinyapps.io/3888_covid-a-6_shiny'>
                             https://zmao0489.shinyapps.io/3888_covid-a-6_shiny</a>
                             <br>
                             </p>")
                      ),
                      
               ),
               
             ),
             
             ),
    
    ),
  )

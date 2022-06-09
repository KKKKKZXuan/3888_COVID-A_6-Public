# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  # Plot of the new cases with selected countries ----
  # This expression that generates a plot is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  # 
  # output$new_cases_plot <- renderPlot({
  #   countries <- input$countries
  #   
  #   ggplot(covid %>% filter(location == countries), 
  #          aes(x = date, y = new_cases, 
  #              group = location, color = location)) +
  #     geom_line(lwd = 1) +
  #     theme_bw() +
  #     ylab("Number of new cases") +
  #     scale_y_continuous(labels = scales::comma) +
  #     scale_x_date(date_breaks = "1 month") +
  #     theme(axis.text.x = element_text(angle = 90)) +
  #     labs(color = "Country/Region") +
  #     xlab("")
  #   
  #   
  # })

  
  
  observe({
    
    updateDateRangeInput(session,
                         inputId = "date_range_01_ken",
                         start = input$date_range_00_ida[1],
                         end = input$date_range_00_ida[2]
    )
    
    updateDateRangeInput(session,
                         inputId = "date_range_02_eva",
                         start = input$date_range_00_ida[1],
                         end = input$date_range_00_ida[2]
    )
    
    updateDateRangeInput(session,
                         inputId = "date_range_03_pat",
                         start = input$date_range_00_ida[1],
                         end = input$date_range_00_ida[2]
    )

    updateDateRangeInput(session,
                         inputId = "date_range_04_gary",
                         start = input$date_range_00_ida[1],
                         end = input$date_range_00_ida[2]
    )

    updateDateRangeInput(session,
                         inputId = "date_range_05_Amber",
                         start = input$date_range_00_ida[1],
                         end = input$date_range_00_ida[2]
    )

    updateDateRangeInput(session,
                         inputId = "date_range_06_summary",
                         start = input$date_range_00_ida[1],
                         end = input$date_range_00_ida[2]
    )
    
  })
  
  # Hello page ----
  
  
  
  # TAB-Panel for IDA page ----
  
  reactives_developed_countries <- reactive({
    
    if (input$full_table_develop %% 2 == 0) {
      covid_data %>%
        filter(as.numeric(human_development_index) >= 0.854) %>%
        select(location,human_development_index) %>%
        arrange(desc(human_development_index)) %>%
        distinct() %>%
        mutate(rank = row_number()) %>% 
        head(10)
    }
    
    else {
      covid_data %>%
        filter(as.numeric(human_development_index) >= 0.854) %>%
        select(location,human_development_index) %>%
        arrange(desc(human_development_index)) %>%
        distinct() %>%
        mutate(rank = row_number())
    }
    
  })
  
  reactives_developing_countries <- reactive({
    
    if (input$full_table_develop %% 2 == 0) {
      covid_data %>%
        filter(as.numeric(human_development_index) < 0.854) %>%
        select(location,human_development_index) %>%
        arrange(desc(human_development_index)) %>%
        distinct() %>%
        mutate(rank = row_number()) %>% 
        head(10)
    }
    
    else {
      covid_data %>%
        filter(as.numeric(human_development_index) < 0.854) %>%
        select(location,human_development_index) %>%
        arrange(desc(human_development_index)) %>%
        distinct() %>%
        mutate(rank = row_number())
    }
    
  })
  
  output$world_map_develop <- renderPlotly({
    
    data <- world
    
    plot <- ggplot(data = world) +
      geom_sf(aes(fill = developed)) + 
      labs(title = "Map of developed and developing countries",
           subtitle = "From COVID A6",
           x = "Latitude",
           y = "Longitude",
           fill = "Developed Country / Region"
      )
    
    ggplotly(plot)
    
    # ggsave("Map_develop.png",
    #        plot = plot,
    #        width = 2560,
    #        height = 1440,
    #        units = "px",
    #        dpi = "retina",
    #        path = "./plot/")
    
  })
  
  output$table_developed_countries <- renderTable({
    
    table <- reactives_developed_countries()
    table
    
  })
  
  output$table_developing_countries <- renderTable({
    
    # covid_data %>%
    #   filter(as.numeric(human_development_index) < 0.854) %>%
    #   select(location,human_development_index) %>%
    #   arrange(human_development_index) %>%
    #   distinct() %>%
    #   mutate(rank = row_number())
    
    table <- reactives_developing_countries()
    table
    
  })
  
  output$skim_data <- renderPrint({
    skim(covid_data)
  })
  
  
  # Q1 Ken Part ----
  
  ## Reactives ----
  
  observe({
    
    if (input$select_research_01_ken == 1) {
      hide("plot_icu_ken_re")
      show("plot_icu_ken")
    }
    
    else if (input$select_research_01_ken == 2) {
      hide("plot_icu_ken")
      show("plot_icu_ken_re")
    }
    
  })
  
  observe({
    
    if (input$select_research_01_ken == 1) {
      updateSelectInput(session,
                        "select_countries_01_ken",
                        h4("Countries / Regions"),
                        choices = list("Research Counrties" = 1,
                                       "Developed \ Developing Countries" = 2),
                        selected = 2)
    }
    else if (input$select_research_01_ken == 2) {
      updateSelectInput(session,
                        "select_countries_01_ken",
                        h4("Countries / Regions"),
                        choices = list("Research Counrties" = 1,
                                       "Developed Countries" = 2,
                                       "Developing Countries" = 3),
                        selected = 2)
    }
    
  })
  
  reactives_covid_A6_ken <- reactive({
    # validate(
    #   need(input$countries, "Please select at least one country")
    # )
    # countries <- input$countries
    subset <- covid_A6_ken %>%
      dplyr::filter(
        as.Date(date) <= input$date_range_01_ken[2],
        as.Date(date) >= input$date_range_01_ken[1]
      )
    return(subset)
  })
  
  reactives_covid_A6_ken_develop <- reactive({
    # validate(
    #   need(input$countries, "Please select at least one country")
    # )
    # countries <- input$countries
    subset <- covid_A6_ken_develop %>%
      dplyr::filter(
        as.Date(date) <= input$date_range_01_ken[2],
        as.Date(date) >= input$date_range_01_ken[1]
      )
    return(subset)
    
  })
  
  reactives_covid_A6_ken_developed <- reactive({
    # validate(
    #   need(input$countries, "Please select at least one country")
    # )
    # countries <- input$countries
    subset <- covid_A6_ken_develop %>%
      dplyr::filter(
        developed == "Yes",
        as.Date(date) <= input$date_range_01_ken[2],
        as.Date(date) >= input$date_range_01_ken[1]
      )
    return(subset)
    
  })
  
  reactives_covid_A6_ken_developing <- reactive({
    # validate(
    #   need(input$countries, "Please select at least one country")
    # )
    # countries <- input$countries
    subset <- covid_A6_ken_develop %>%
      dplyr::filter(
        developed == "No",
        as.Date(date) <= input$date_range_01_ken[2],
        as.Date(date) >= input$date_range_01_ken[1]
        )
    return(subset)
    
  })
  
  ## Plots ----
  
  output$plot_icu_ken <- renderPlotly({
    
    if (input$select_research_01_ken == 1) {
      # IF-1 IDA
      
      if (input$select_countries_01_ken == 1) {
        # IF-1-1 Research Counrties
        
        data <- reactives_covid_A6_ken()
        
        if (input$select_data_type_01_ken == 1) {
          # IF-1-1-1 New Cases
          
          plot <- ggplot(data,
                         aes(x = date,
                             y = new_cases_smoothed_per_million,
                             group = developed,
                             color = location
                         )
          ) +
            geom_line() +
            labs(title = "Comparison of new cases of covid 19 between research countries",
                 subtitle = "By Zhekai-Mao (Ken) from COVID A6",
                 x = "Date",
                 y = "Number of new cases",
                 color = "Country / Region"
            )
          
        }
        
        else if (input$select_data_type_01_ken == 2) {
          # IF-1-1-2 New Deaths
          
          plot <- ggplot(data,
                         aes(x = date,
                             y = new_deaths_smoothed_per_million,
                             group = developed,
                             color = location)
          ) +
            geom_line() + 
            labs(title = "Comparison of new deaths of covid 19 between research countries",
                 subtitle = "By Zhekai-Mao (Ken) from COVID A6",
                 x = "Date",
                 y = "Number of new deaths",
                 color = "Country / Region"
            )
          
        }
        
        else if (input$select_data_type_01_ken == 3) {
          # IF-1-1-3 New ICU
          
          plot <- ggplot(data, 
                         aes(x = date,
                             y = new_icu_covid_per_million,
                             group = developed,
                             color = location)
          ) +
            geom_line() + 
            labs(title = "Comparison of new covid ICU of covid 19 between research countries",
                 subtitle = "By Zhekai-Mao (Ken) from COVID A6",
                 x = "Date",
                 y = "Number of new covid ICU",
                 color = "Country / Region"
            )
          
        }
        
        else if (input$select_data_type_01_ken == 4) {
          # IF-1-1-4 ICU Occupancy Rate
          
          plot <- ggplot(data, 
                         aes(x = date,
                             y = icu_occupancy_rate,
                             group = developed,
                             color = location)
          ) +
            geom_line() + 
            labs(title = "Comparison of ICU occupancy rate of covid 19 between research countries",
                 subtitle = "By Zhekai-Mao (Ken) from COVID A6",
                 x = "Date",
                 y = "ICU occupancy rate",
                 color = "Country / Region"
            )
          
        }
        
      }
      
      else if (input$select_countries_01_ken == 2) {
        # IF-1-2 Developed & Developing Countries
        
        data <- reactives_covid_A6_ken_develop()
        
        if (input$select_data_type_01_ken == 1) {
          # IF-1-2-1 New Cases
          
          plot <- ggplot(data,
                         aes(x = date,
                             y = new_cases_smoothed_per_million,
                             group = developed,
                             color = developed
                         )
          ) +
            geom_line() +
            labs(title = "Comparison of new cases of covid 19 between developed and developing countries",
                 subtitle = "By Zhekai-Mao (Ken) from COVID A6",
                 x = "Date",
                 y = "Number of new cases",
                 color = "Developed Country / Region"
            )
          
        }
        
        else if (input$select_data_type_01_ken == 2) {
          # IF-1-2-2 New Deaths
          
          plot <- ggplot(data,
                         aes(x = date,
                             y = new_deaths_smoothed_per_million,
                             group = developed,
                             color = developed)
          ) +
            geom_line() + 
            labs(title = "Comparison of new deaths of covid 19 between developed and developing countries",
                 subtitle = "By Zhekai-Mao (Ken) from COVID A6",
                 x = "Date",
                 y = "Number of new deaths",
                 color = "Developed Country / Region"
            )
          
        }
        
        else if (input$select_data_type_01_ken == 3) {
          # IF-1-3-3 New ICU
          
          plot <- ggplot(data, 
                         aes(x = date,
                             y = new_icu_covid_per_million,
                             group = developed,
                             color = developed)
          ) +
            geom_line() + 
            labs(title = "Comparison of new covid ICU of covid 19 between developed and developing countries",
                 subtitle = "By Zhekai-Mao (Ken) from COVID A6",
                 x = "Date",
                 y = "Number of new covid ICU",
                 color = "Developed Country / Region"
            )
          
        }
        
        else if (input$select_data_type_01_ken == 4) {
          # IF-1-4-4 ICU Occupancy Rate
          
          plot <- ggplot(data, 
                         aes(x = date,
                             y = icu_occupancy_rate,
                             group = developed,
                             color = developed)
          ) +
            geom_line() + 
            labs(title = "Comparison of ICU occupancy rate of covid 19 between developed and developing countries",
                 subtitle = "By Zhekai-Mao (Ken) from COVID A6",
                 x = "Date",
                 y = "ICU occupancy rate",
                 color = "Developed Country / Region"
            )
          
        }
        
      }
      
      ggplotly(plot)
      
    }
    
    else if (input$select_research_01_ken == 3) {
      # IF-2 Model
      
      if (input$select_countries_01_ken == 1) {
        # IF-2-1 Research Counrties
        
        data <- reactives_covid_A6_ken()
        
        if (input$select_data_type_01_ken == 1) {
          # IF-2-1-1 New Cases
          
          plot <- autoplot(forecast(auto.arima(data$new_cases_smoothed_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 2) {
          # IF-2-1-2 New Deaths
          
          plot <- autoplot(forecast(auto.arima(data$new_deaths_smoothed_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 3) {
          # IF-2-1-3 New ICU
          
          plot <- autoplot(forecast(auto.arima(data$new_icu_covid_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 4) {
          # IF-2-1-4 ICU Occupancy Rate
          
          plot <- autoplot(forecast(auto.arima(data$icu_occupancy_rate), 30))
          
        }
      }
      
      else if (input$select_countries_01_ken == 2) {
        # IF-2-1 Research Counrties
        
        data <- reactives_covid_A6_ken_develop()
        
        if (input$select_data_type_01_ken == 1) {
          # New Cases
          
          plot <- autoplot(forecast(auto.arima(data$new_cases_smoothed_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 2) {
          # New Deaths
          
          plot <- autoplot(forecast(auto.arima(data$new_deaths_smoothed_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 3) {
          # New ICU
          
          plot <- autoplot(forecast(auto.arima(data$new_icu_covid_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 4) {
          # ICU Occupancy Rate
          
          plot <- autoplot(forecast(auto.arima(data$icu_occupancy_rate), 30))
          
        }
      }
      
      plot(plot)
      
    }
  })
  
  output$plot_icu_ken_re <- renderPlot({
    
    if (input$select_research_01_ken == 3) {
      # IF-1 IDA
      
      if (input$select_countries_01_ken == 1) {
        # IF-1-1 Research Counrties
        
        data <- reactives_covid_A6_ken()
        
        if (input$select_data_type_01_ken == 1) {
          # IF-1-1-1 New Cases
          
          plot <- ggplot(data,
                         aes(x = date,
                             y = new_cases_smoothed_per_million,
                             group = developed,
                             color = location
                         )
          ) +
            geom_line() +
            ylab("Number of new cases") +
            ggtitle("plot_new_cases_per_million") +
            labs(color = "Country / Region")
          
        }
        
        else if (input$select_data_type_01_ken == 2) {
          # IF-1-1-2 New Deaths
          
          plot <- ggplot(data,
                         aes(x = date,
                             y = new_deaths_smoothed_per_million,
                             group = developed,
                             color = location)
          ) +
            geom_line() + 
            ylab("Number of new deaths") +
            ggtitle("plot_new_deaths_per_million") +
            labs(color = "Country / Region")
          
        }
        
        else if (input$select_data_type_01_ken == 3) {
          # IF-1-1-3 New ICU
          
          plot <- ggplot(data, 
                         aes(x = date,
                             y = new_icu_covid_per_million,
                             group = developed,
                             color = location)
          ) +
            geom_line() + 
            ylab("Number of new covid ICU") +
            ggtitle("plot_new_icu_covid_per_million") +
            labs(color = "Country / Region")
          
        }
        
        else if (input$select_data_type_01_ken == 4) {
          # IF-1-1-4 ICU Occupancy Rate
          
          plot <- ggplot(data, 
                         aes(x = date,
                             y = icu_occupancy_rate,
                             group = developed,
                             color = location)
          ) +
            geom_line() + 
            ylab("Number of ICU occupancy rate") +
            ggtitle("plot_icu_occupancy_rate") +
            labs(color = "Country / Region")
          
        }
        
      }
      
      else if (input$select_countries_01_ken == 2) {
        # IF-1-2 Developed & Developing Countries
        
        data <- reactives_covid_A6_ken_develop()
        
        if (input$select_data_type_01_ken == 1) {
          # IF-1-2-1 New Cases
          
          plot <- ggplot(data,
                         aes(x = date,
                             y = new_cases_smoothed_per_million,
                             group = developed,
                             color = developed
                         )
          ) +
            geom_line() +
            ylab("Number of new cases") +
            ggtitle("plot_new_cases_per_million") +
            labs(color = "Developed Country / Region")
          
        }
        
        else if (input$select_data_type_01_ken == 2) {
          # IF-1-2-2 New Deaths
          
          plot <- ggplot(data,
                         aes(x = date,
                             y = new_deaths_smoothed_per_million,
                             group = developed,
                             color = developed)
          ) +
            geom_line() + 
            ylab("Number of new deaths") +
            ggtitle("plot_new_deaths_per_million") +
            labs(color = "Developed Country / Region")
          
        }
        
        else if (input$select_data_type_01_ken == 3) {
          # IF-1-3-3 New ICU
          
          plot <- ggplot(data, 
                         aes(x = date,
                             y = new_icu_covid_per_million,
                             group = developed,
                             color = developed)
          ) +
            geom_line() + 
            ylab("Number of new covid ICU") +
            ggtitle("plot_new_icu_covid_per_million") +
            labs(color = "Developed Country / Region")
          
        }
        
        else if (input$select_data_type_01_ken == 4) {
          # IF-1-4-4 ICU Occupancy Rate
          
          plot <- ggplot(data, 
                         aes(x = date,
                             y = icu_occupancy_rate,
                             group = developed,
                             color = developed)
          ) +
            geom_line() + 
            ylab("Number of ICU occupancy rate") +
            ggtitle("plot_icu_occupancy_rate") +
            labs(color = "Developed Country / Region")
          
        }
        
      }
      
      ggplotly(plot)
      
    }
    
    else if (input$select_research_01_ken == 2) {
      # IF-2 Model
      
      if (input$select_countries_01_ken == 1) {
        # IF-2-1 Research Counrties
        
        data <- reactives_covid_A6_ken()
        
        if (input$select_data_type_01_ken == 1) {
          # IF-2-1-1 New Cases
          
          plot <- autoplot(forecast(auto.arima(data$new_cases_smoothed_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 2) {
          # IF-2-1-2 New Deaths
          
          plot <- autoplot(forecast(auto.arima(data$new_deaths_smoothed_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 3) {
          # IF-2-1-3 New ICU
          
          plot <- autoplot(forecast(auto.arima(data$new_icu_covid_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 4) {
          # IF-2-1-4 ICU Occupancy Rate
          
          plot <- autoplot(forecast(auto.arima(data$icu_occupancy_rate), 30))
          
        }
      }
      
      else if (input$select_countries_01_ken == 2) {
        # IF-2-1 Developed Counrties
        
        data <- reactives_covid_A6_ken_developed()
        
        if (input$select_data_type_01_ken == 1) {
          # New Cases
          
          plot <- autoplot(forecast(auto.arima(data$new_cases_smoothed_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 2) {
          # New Deaths
          
          plot <- autoplot(forecast(auto.arima(data$new_deaths_smoothed_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 3) {
          # New ICU
          
          plot <- autoplot(forecast(auto.arima(data$new_icu_covid_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 4) {
          # ICU Occupancy Rate
          
          plot <- autoplot(forecast(auto.arima(data$icu_occupancy_rate), 30))
          
        }
      }
      
      else if (input$select_countries_01_ken == 3) {
        # IF-2-1 Developing Counrties
        
        data <- reactives_covid_A6_ken_developing()
        
        if (input$select_data_type_01_ken == 1) {
          # New Cases
          
          plot <- autoplot(forecast(auto.arima(data$new_cases_smoothed_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 2) {
          # New Deaths
          
          plot <- autoplot(forecast(auto.arima(data$new_deaths_smoothed_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 3) {
          # New ICU
          
          plot <- autoplot(forecast(auto.arima(data$new_icu_covid_per_million), 30))
          
        }
        
        else if (input$select_data_type_01_ken == 4) {
          # ICU Occupancy Rate
          
          plot <- autoplot(forecast(auto.arima(data$icu_occupancy_rate), 30))
          
        }
      }
      
      plot(plot)
      
    }
  })
  
  
  # Q2 Eva Part ----
  
  ## Reactives ----
  
  reactives_covid_A6_eva <- reactive({
    
    # subset <- covid_A6_eva
    
    if (input$select_countries_02_eva == 1) {
      
      subset <- covid_A6_eva_developed
      
    }
    
    else if (input$select_countries_02_eva == 2) {
      
      subset <- covid_A6_eva_developing
    }
    
    else {
      
      subset <- covid_A6_eva
      
    }
    
    return(subset)
    
  })
  
  ## Plots ----
  
  output$plot_pd_eva <- renderPlotly({
    
    data <- reactives_covid_A6_eva()
    
    plot <- ggplot(data = data) +
      geom_point(size = 1,
                 aes(x = location,
                     y = population_density,
                     color = total_deaths_per_million)) +
      labs(title = "Comparison of deaths rates in areas with different population densities",
           subtitle = "By Changwa-Yin (Eva) from COVID A6",
           # tag = "A",
           x = "Country / Region",
           y = "Population density",
           color = "Total deaths (per million)"
      ) +
      scale_colour_viridis(option = "C")
    
    ggplotly(plot)
    
  })
  
  
  # Q3 Pat Part ----
  
  ## Reactives ----
  
  reactives_covid_A6_pat_developed <- reactive({
    
    subset <- covid_A6_pat_developed
    
    # subset <- covid_A6_pat_developed %>%
    #   dplyr::filter(as.Date(date) <= input$date_range_03_pat[2],
    #                 as.Date(date) >= input$date_range_03_pat[1])
    
    return(subset)
    
  })
  
  reactives_covid_A6_pat_developing <- reactive({
    
    subset <- covid_A6_pat_developing
    
    # subset <- covid_A6_pat_developing %>%
    #   dplyr::filter(as.Date(date) <= input$date_range_03_pat[2],
    #                 as.Date(date) >= input$date_range_03_pat[1])
    
    return(subset)
    
  })
  
  observe({
    
    if (input$select_develop_type_03_pat == 1) {
      updateSelectInput(session,
                        "select_data_type_03_pat",
                        choices = list("Full" = 1,
                                       "None" = 2,
                                       "Diabetes" = 3,
                                       "Aids" = 4,
                                       "Cardiovascular" = 5), 
                        selected = 3)
    }
    else if (input$select_develop_type_03_pat == 2) {
      updateSelectInput(session,
                        "select_data_type_03_pat",
                        choices = list("Full" = 1,
                                       "None" = 2,
                                       "Hepatitis" = 3,
                                       "Cardiovascular" = 4,
                                       "Measles" = 5,
                                       "Diabetes" = 6,
                                       "Pertussis" = 7,
                                       "Low_bw" = 8), 
                        selected = 3)
    }

  })
  
  ## Plots ----
  
  output$plot_diseases_pat <- renderPlotly({
    
    if (input$select_develop_type_03_pat == 1) {
      
      data <- reactives_covid_A6_pat_developed()
      
      if (input$select_data_type_03_pat == 1) {
        
        plot <- ggplot(data = data,
                       aes(x = diabetes,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          ylim(0, 175000) +
          xlim(0,20) +
          labs(title = "Diabetes vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Diabetes",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      else if (input$select_data_type_03_pat == 2) {
        
        plot <- ggplot(data = data,
                       aes(x = diabetes,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          ylim(0, 175000) +
          xlim(0,20) +
          labs(title = "Diabetes vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Diabetes",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      else if (input$select_data_type_03_pat == 3) {
        
        plot <- ggplot(data = data,
                       aes(x = diabetes,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          ylim(0, 175000) +
          xlim(0,20) +
          labs(title = "Diabetes vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Diabetes",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      else if (input$select_data_type_03_pat == 4) {
        
        plot <- ggplot(data = data,
                       aes(x = Aids,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          ylim(0, 175000) +
          xlim(0,20) +
          labs(title = "Aids vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Aids",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      else if (input$select_data_type_03_pat == 5) {
        
        plot <- ggplot(data = data,
                       aes(x = cardiovascular,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          labs(title = "Cardiovascular vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Cardiovascular",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      
    }
    else if (input$select_develop_type_03_pat == 2) {
      
      data <- reactives_covid_A6_pat_developing()
      
      if (input$select_data_type_03_pat == 1) {
        
        plot <- ggplot(data = data,
                       aes(x = Hepatitis,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          ylim(0, 175000) +
          xlim(0,20) +
          labs(title = "Hepatitis vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Hepatitis",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      else if (input$select_data_type_03_pat == 2) {
        
        plot <- ggplot(data = data,
                       aes(x = Hepatitis,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          ylim(0, 175000) +
          xlim(0,20) +
          labs(title = "Hepatitis vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Hepatitis",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      else if (input$select_data_type_03_pat == 3) {
        
        plot <- ggplot(data = data,
                       aes(x = Hepatitis,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          ylim(0, 175000) +
          xlim(0,20) +
          labs(title = "Hepatitis vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Hepatitis",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      else if (input$select_data_type_03_pat == 4) {
        
        plot <- ggplot(data = data,
                       aes(x = cardiovascular,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          labs(title = "Cardiovascular vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Cardiovascular",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      else if (input$select_data_type_03_pat == 5) {
        
        plot <- ggplot(data = data,
                       aes(x = Measles,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          ylim(0, 175000) +
          xlim(0,20) +
          labs(title = "Measles vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Measles",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      else if (input$select_data_type_03_pat == 6) {
        
        plot <- ggplot(data = data,
                       aes(x = diabetes,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          ylim(0, 175000) +
          xlim(0,20) +
          labs(title = "Diabetes vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Diabetes",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      else if (input$select_data_type_03_pat == 7) {
        
        plot <- ggplot(data = data,
                       aes(x = Pertussis,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          ylim(0, 175000) +
          xlim(0,20) +
          labs(title = "Pertussis vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Pertussis",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      else if (input$select_data_type_03_pat == 8) {
        
        plot <- ggplot(data = data,
                       aes(x = Low_bw,
                           y = total_deaths,
                           color = hdi)) +
          geom_point(size = 3) +
          ylim(0, 175000) +
          xlim(0,20) +
          labs(title = "Low bw vs. covid 19 deaths by Human Development Index",
               subtitle = "By Pat-Thepyasuwan from COVID A6",
               x = "Low bw",
               y = "Total deaths",
               color = "Human Development Index"
          )
        
      }
      
    }
    
    ggplotly(plot)
    
  })
  
  output$text_diseases_lm_pat <- renderPrint({
    
    if (input$select_develop_type_03_pat == 1) {
      
      developing_countries_pat <- reactives_covid_A6_pat_developed()
      
      if (input$select_data_type_03_pat == 1) {
        
        lm1 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developing_countries_pat)
        summary(lm1)$coefficients %>% round(5)
        
      }
      else if (input$select_data_type_03_pat == 2) {
        
        lm1 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developing_countries_pat)
        drop1(lm1, test = "F")
        
      }
      else if (input$select_data_type_03_pat == 3) {
        
        lm1 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developing_countries_pat)
        lm1_2 <- update(lm1, . ~ . - diabetes)
        
        drop1(lm1_2, test = "F")
        
      }
      else if (input$select_data_type_03_pat == 4) {
        
        lm1 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developing_countries_pat)
        lm1_2 <- update(lm1, . ~ . - diabetes)
        lm1_3 <- update(lm1_2, . ~ . -Aids)
        
        drop1(lm1_3, test = "F")
        
      }
      else if (input$select_data_type_03_pat == 5) {
        
        lm1 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developing_countries_pat)
        lm1_2 <- update(lm1, . ~ . - diabetes)
        lm1_3 <- update(lm1_2, . ~ . -Aids)
        lm1_4 <- update(lm1_3, .~.-cardiovascular)
        
        drop1(lm1_4, test = "F")
        
      }
      
    }
    else if (input$select_develop_type_03_pat == 2) {
      
      developed_countries_pat <- reactives_covid_A6_pat_developing()
      
      if (input$select_data_type_03_pat == 1) {
        
        lm2 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developed_countries_pat)
        
        summary(lm2)$coefficients %>% round(4)
        
      }
      else if (input$select_data_type_03_pat == 2) {
        
        lm2 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developed_countries_pat)
        
        drop1(lm2, test="F")
        
      }
      else if (input$select_data_type_03_pat == 3) {
        
        lm2 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developed_countries_pat)
        lm2_2 <- update(lm2, .~. -Hepatitis)
        
        drop1(lm2_2, test="F")
        
      }
      else if (input$select_data_type_03_pat == 4) {
        
        lm2 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developed_countries_pat)
        lm2_2 <- update(lm2, .~. -Hepatitis)
        lm2_3 <- update(lm2_2, .~. -cardiovascular)
        
        drop1(lm2_3, test = "F")
        
      }
      else if (input$select_data_type_03_pat == 5) {
        
        lm2 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developed_countries_pat)
        lm2_2 <- update(lm2, .~. -Hepatitis)
        lm2_3 <- update(lm2_2, .~. -cardiovascular)
        lm2_4 <- update(lm2_3, .~. -Measles)
        
        drop1(lm2_4, test = "F")
        
      }
      else if (input$select_data_type_03_pat == 6) {
        
        lm2 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developed_countries_pat)
        lm2_2 <- update(lm2, .~. -Hepatitis)
        lm2_3 <- update(lm2_2, .~. -cardiovascular)
        lm2_4 <- update(lm2_3, .~. -Measles)
        lm2_5 <- update(lm2_4, .~. -diabetes)
        
        drop1(lm2_5, test = "F")
        
      }
      else if (input$select_data_type_03_pat == 7) {
        
        lm2 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developed_countries_pat)
        lm2_2 <- update(lm2, .~. -Hepatitis)
        lm2_3 <- update(lm2_2, .~. -cardiovascular)
        lm2_4 <- update(lm2_3, .~. -Measles)
        lm2_5 <- update(lm2_4, .~. -diabetes)
        lm2_6 <- update(lm2_5, .~. -Pertussis)
        
        drop1(lm2_6, test = "F")
        
      }
      else if (input$select_data_type_03_pat == 8) {
        
        lm2 <- lm(total_deaths ~ cardiovascular + diabetes + Aids + Hepatitis + Measles + Pertussis + Low_bw, developed_countries_pat)
        lm2_2 <- update(lm2, .~. -Hepatitis)
        lm2_3 <- update(lm2_2, .~. -cardiovascular)
        lm2_4 <- update(lm2_3, .~. -Measles)
        lm2_5 <- update(lm2_4, .~. -diabetes)
        lm2_6 <- update(lm2_5, .~. -Pertussis)
        lm2_7 <- update(lm2_6, .~. -Low_bw)
        
        drop1(lm2_7, test = "F")
        
      }
      
    }
    
    
  })
  
  
  # Q4 Gary Part ----
  
  ## Reactives ----
  
  reactives_covid_A6_gary <- reactive({

    # subset <- covid_A6_gary
    
    subset <- covid_A6_gary %>%
      dplyr::filter(Date <= input$date_range_04_gary[2],
                    Date >= input$date_range_04_gary[1])
    return(subset)
    
  })
  
  ## Plots ----
  
  output$plot_age_gary <- renderPlotly({
    
    if (input$select_countries_04_gary == 1) {
      
      data <- reactives_covid_A6_gary()
      
      group1 <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")
      data1 <- data %>%
        filter(Location == "Sweden") %>%
        filter(excelsheet == "PHAS_Data") %>%
        filter(Age_group %in% group1) %>%
        mutate(Month = month(Date))
        
      plot <- ggplot(data = data1,
                     aes(x = Date,
                         y = Group_deaths_number,
                         group = Age_group,
                         color = Age_group)) +
        geom_point() +
        geom_line(aes(color = Age_group)) + 
        labs(title = "COVID-19- Deaths by age group in the developed countries in 2021",
             subtitle = "By Zhenyao-Dou (Gary) from COVID A6",
             x = "Date",
             y = "Number of Deaths",
             color = "Age")
      
        # plot_ly(x = ~ Date, 
        #         y = ~ Group_deaths_number,
        #         color = ~ Age_group, 
        #         type = "scatter",
        #         mode = "line") %>%
        # layout(title = "COVID-19- Deaths by age group in the Sweden in 2021",
        #        yaxis = list(title = "Number of Deaths"),
        #        xaxis = list(title = "Date"))
      
    }
    
    else if (input$select_countries_04_gary == 2) {
      
      data <- reactives_covid_A6_gary()
      
      group2 <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
      data2 <- data %>%
        filter(Location == "Romania") %>%
        filter(Age_group %in% group2) %>%
        mutate(Month = month(Date))
      
      plot <- ggplot(data = data2,
                     aes(x = Date,
                         y = Group_deaths_number,
                         group = Age_group,
                         color = Age_group)) +
        geom_point() +
        geom_line(aes(color = Age_group)) + 
        labs(title = "COVID-19- Deaths by age group in the developing countries in 2021",
             subtitle = "By Zhenyao-Dou (Gary) from COVID A6",
             x = "Date",
             y = "Number of Deaths",
             color = "Age")
      
        # plot_ly(x = ~ Date, 
        #         y = ~ Group_deaths_number,
        #         color = ~ Age_group, 
        #         type = "scatter",
        #         mode = "line") %>%
        # layout(title = "COVID-19- Deaths by age group in the Romania in 2021",
        #        yaxis = list(title = "Number of Deaths"),
        #        xaxis = list(title = "Date"))
        
    }
    
    ggplotly(plot)
    
  })
  
  output$text_age_gary <- renderText({
    
    if (input$select_countries_04_gary == 1) {
      
      # data <- reactives_covid_A6_gary()
      
      ro <- filter(covid_A6_gary, Location == "Romania")
      group <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
      
      #group = c("0-19", "20-39", "40-59", "60-79", "80+")
      
      ro <- filter(ro, Date == input$date_range_04_gary[1] | Date == input$date_range_04_gary[2])
      # ro
      ro <- ro %>%
        filter(Age_group %in% group) %>%
        group_by(Age_group)%>%
        mutate(total = max(Group_deaths_number)-min(Group_deaths_number))%>%
        filter(Date !="2020-06-01")%>%
        select(Age_group,total)%>%
        mutate(Age_group = as.factor(Age_group))
      
      
      # ro = tibble::rowid_to_column(ro, "index")x
      ro_indx <- as.numeric(ro$index)
      ro_total <- as.numeric(ro$total)
      
      cor.test(ro_indx,ro_total)
    }
    
    else if (input$select_countries_04_gary == 2) {
      
      # data <- reactives_covid_A6_gary()
      
      sw <- filter(covid_A6_gary, Location == "Sweden")
      group3 <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")
      
      
      sw <- filter(sw, Date == input$date_range_04_gary[1] | Date == input$date_range_04_gary[2]) #due to limitation of dataset,we will use "2021-12-30" as the last day of 2021
      # sw
      sw <- sw %>%
        filter(Age_group %in% group3) %>%
        filter(excelsheet != "NBHW_Data")%>%
        group_by(Age_group)%>%
        mutate(total = max(Group_deaths_number)-min(Group_deaths_number))%>%
        filter(Date !="2020-06-01")%>%
        select(Age_group,total)%>%
        mutate(Age_group = as.factor(Age_group))
      
      # sw
      # sw = tibble::rowid_to_column(sw, "index")
      sw_indx <- as.numeric(sw$index)
      sw_total <- as.numeric(sw$total)
      
      cor.test(sw_indx,sw_total)
    }
    
  })  
  
  # Q5 Amber Part ----
  
  ## Reactives ----
  
  observe({
    
    if (input$select_research_type_05_Amber == 1) {
      updateSelectInput(session,
                        "select_countries_05_Amber",
                        choices = list("-none-" = 1), 
                        selected = 1)
    }
    else if (input$select_research_type_05_Amber == 2) {
      updateSelectInput(session,
                        "select_countries_05_Amber",
                        choices = list("Developed" = 1,
                                       "Developing" = 2), 
                        selected = 1)
    }
    else if (input$select_research_type_05_Amber == 3 | input$select_research_type_05_Amber == 4) {
      updateSelectInput(session,
                        "select_countries_05_Amber",
                        choices = list("Developed" = 1,
                                       "Developing" = 2), 
                        selected = 1)
    }
    
  })
  
  observe({
    
    if (input$select_research_type_05_Amber == 1 |
        input$select_research_type_05_Amber == 2 |
        input$select_research_type_05_Amber == 3 |
        input$select_research_type_05_Amber == 4) {
      hide("plot_aus_mal_vaccinations_amber_re")
      show("plot_aus_mal_vaccinations_amber")
    }
    
    else if (input$select_research_type_05_Amber == 5) {
      hide("plot_aus_mal_vaccinations_amber")
      show("plot_aus_mal_vaccinations_amber_re")
    }
    
  })
  
  reactives_covid_aus_mal_amber <- reactive({
    # validate(
    #   need(input$countries, "Please select at least one country")
    # )
    # countries <- input$countries
    
    # subset <- covid_aus_mal_amber
    
    subset <- covid_aus_mal_amber %>%
      dplyr::filter(as.Date(date) <= input$date_range_05_Amber[2],
                    as.Date(date) >= input$date_range_05_Amber[1])
    return(subset)
  })
  
  reactives_covid_mal_amber <- reactive({
    # validate(
    #   need(input$countries, "Please select at least one country")
    # )
    # countries <- input$countries
    
    # subset <- Malaysia_amber
    
    subset <- Malaysia_amber %>%
      dplyr::filter(as.Date(date) <= input$date_range_05_Amber[2],
                    as.Date(date) >= input$date_range_05_Amber[1])
    return(subset)
  })
  
  reactives_covid_aus_amber <- reactive({
    # validate(
    #   need(input$countries, "Please select at least one country")
    # )
    # countries <- input$countries
    
    # subset <- Australia_amber
    
    subset <- Australia_amber %>%
      dplyr::filter(as.Date(date) <= input$date_range_05_Amber[2],
                    as.Date(date) >= input$date_range_05_Amber[1])
    return(subset)
  })
  
  reactives_covid_amber_developed <- reactive({
    # validate(
    #   need(input$countries, "Please select at least one country")
    # )
    # countries <- input$countries
    
    # subset <- covid_A6_amber_developed
    
    subset <- covid_A6_amber_developed %>%
      dplyr::filter(as.Date(date) <= input$date_range_05_Amber[2],
                    as.Date(date) >= input$date_range_05_Amber[1])
    return(subset)
  })
  
  reactives_covid_amber_developing <- reactive({
    # validate(
    #   need(input$countries, "Please select at least one country")
    # )
    # countries <- input$countries
    
    # subset <- covid_A6_amber_developing
    
    subset <- covid_A6_amber_developing %>%
      dplyr::filter(as.Date(date) <= input$date_range_05_Amber[2],
                    as.Date(date) >= input$date_range_05_Amber[1])
    return(subset)
  })
  
  ## Plots ----

  output$plot_aus_mal_vaccinations_amber <- renderPlotly({
    
    if (input$select_research_type_05_Amber == 1) {
      # EDA
      
      data <- reactives_covid_aus_mal_amber()
      
      plot <- ggplot(data = data) +
        geom_point(aes(x = total_vaccinations_per_hundred,
                       y = total_deaths_per_million,
                       color = location)) +
        labs(title = "Comparison of vaccinations rates and deaths rates in different areas",
             subtitle = "By Zhanqiu-Chen (Amber) from COVID A6",
             # tag = "A",
             x = "Total vaccinations (per hundred)",
             y = "Total deaths (per million)",
             color = "Country / Region"
        )
    }
    
    else if (input$select_research_type_05_Amber == 2) {
      # Total Cases
      
      if (input$select_countries_05_Amber == 1) {
        
        data <- reactives_covid_aus_amber()
        
        plot <- ggplot(data = data) +
          geom_line(aes(x = date,
                        y = total_cases_per_million)) + 
          labs(title = "Covid 19 case data in developed countries",
               subtitle = "By Zhanqiu-Chen (Amber) from COVID A6",
               # tag = "A",
               x = "Date",
               y = "Total cases (per million)"
          )
      }
      
      else if (input$select_countries_05_Amber == 2) {
        
        data <- reactives_covid_mal_amber()
        
        plot <- ggplot(data = data) +
          geom_line(aes(x = date,
                        y = total_cases_per_million)) + 
          labs(title = "Covid 19 case data in developing countries",
               subtitle = "By Zhanqiu-Chen (Amber) from COVID A6",
               # tag = "A",
               x = "Date",
               y = "Total cases (per million)"
          )
      }
      
    }
    
    else if (input$select_research_type_05_Amber == 3) {
      # ACF
      
      if (input$select_countries_05_Amber == 1) {
        
        data <- reactives_covid_amber_developed()
        
        plot <- ggAcf(data$total_cases) +
          labs(title = "ACF of total cases in developed countries",
               subtitle = "By Zhanqiu-Chen (Amber) from COVID A6")
        
      }
      
      else if (input$select_countries_05_Amber == 2) {
        
        data <- reactives_covid_amber_developing()
        
        plot <- ggAcf(data$total_cases) +
          labs(title = "ACF of total cases in developing countries",
               subtitle = "By Zhanqiu-Chen (Amber) from COVID A6")
        
      }
      
    }
    
    else if (input$select_research_type_05_Amber == 3) {
      # ACF
      
      if (input$select_countries_05_Amber == 1) {
        
        data <- reactives_covid_amber_developed()
        
        plot <- ggAcf(data$total_cases) +
          labs(title = "ACF of total cases in developed countries",
               subtitle = "By Zhanqiu-Chen (Amber) from COVID A6")
        
      }
      
      else if (input$select_countries_05_Amber == 2) {
        
        data <- reactives_covid_amber_developing()
        
        plot <- ggAcf(data$total_cases) +
          labs(title = "ACF of total cases in developing countries",
               subtitle = "By Zhanqiu-Chen (Amber) from COVID A6")
        
      }
      
    }
    
    else if (input$select_research_type_05_Amber == 4) {
      # PACF
      
      if (input$select_countries_05_Amber == 1) {
        
        data <- reactives_covid_amber_developed()
        
        plot <- ggPacf(data$total_cases) +
          labs(title = "PACF of total cases in developed countries",
               subtitle = "By Zhanqiu-Chen (Amber) from COVID A6")
        
      }
      
      else if (input$select_countries_05_Amber == 2) {
        
        data <- reactives_covid_amber_developing()
        
        plot <- ggPacf(data$total_cases) +
          labs(title = "PACF of total cases in developing countries",
               subtitle = "By Zhanqiu-Chen (Amber) from COVID A6")
        
      }
      
    }
    
    ggplotly(plot)
    
  })
  
  output$plot_aus_mal_vaccinations_amber_re <- renderPlot({
    
    if (input$select_research_type_05_Amber == 5) {
      
      if (input$select_countries_05_Amber == 1) {
        
        data <- reactives_covid_amber_developed()
        
        plot <- autoplot(forecast(auto.arima(data$total_cases), 30))
        
      }
      
      else if (input$select_countries_05_Amber == 2) {
        
        data <- reactives_covid_amber_developing()
        
        plot <- autoplot(forecast(auto.arima(data$total_cases), 30))
        
      }
      
    }
    
    plot(plot)
    
  })

  # ---
  # Amber ACF acf_developed_countries_total_cases_amber

  # output$acf_developed_countries_total_cases_amber <-
  #   renderPlot({
  #     acf(covid_A6_amber_developed$total_cases)
  #   })
  # 
  # # ---
  # # Amber ARIMA developed_countries_total_cases_amber
  # 
  # fit_developed_countries_total_cases_amber <-
  #   auto.arima(covid_A6_amber_developed$total_cases)
  # 
  # output$forecast_fit_developed_countries_total_cases_amber <-
  #   renderTable({
  #     forecast(fit_developed_countries_total_cases_amber, 30)
  #   })
  # 
  # output$plot_forecast_fit_developed_countries_total_cases_amber <-
  #   renderPlotly({
  #     plotly(forecast(fit_developed_countries_total_cases_amber, 30))
  #   })
  # 
  # # ---
  # # ARIMA developing_countries_total_cases_amber
  # 
  # fit_developing_countries_total_cases_amber <-
  #   auto.arima(covid_A6_amber_developing$total_cases)
  # 
  # output$forecast_fit_developing_countries_total_cases_amber <-
  #   renderTable({
  #     forecast(fit_developing_countries_total_cases_amber, 30)
  #   })
  # 
  # output$plot_forecast_fit_developing_countries_total_cases_amber <-
  #   renderPlotly({
  #     plotly(forecast(fit_developing_countries_total_cases_amber, 30))
  #   })

}
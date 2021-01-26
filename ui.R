header <- dashboardHeader(title = "SEA Covid-19 Daskboard")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Covid-19 Cases Overview", tabName = "sea", icon = icon("globe-asia")),
        menuItem(text = "Cases Growth", tabName = "plot", icon = icon("chart-area"), badgeLabel = "real-time", badgeColor = "red"),
        menuItem(text = "Search Correlation Var", tabName = "corr", icon = icon("search")),
        menuItem(text = "Country Case", icon = icon("flag"), tabName = "country"),
        menuItem(text = "Map", icon = icon("map-marked-alt"), tabName = "map", badgeLabel = "updated data", badgeColor = "red"),
        menuItem(text = "Dataset", tabName = "data", icon = icon("database"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "sea", h1("Covid-19 Cases in Southeast Asian Countries", align = "center",
                                     
                                     h4("Southeast Asia was among the first regions to be affected by the pandemic. It was confirmed to have 
                                     spread to Southeast Asia on 13 January 2020, when a 61-year-old woman from Wuhan tested positive in Thailand, 
                                        making it the first country except China to report a case. By 24 March, all states in the region had announced 
                                        at least one case."),
                                     h4("This dashboard visualizes the information about Covid-19 Cases in Southeast Asian countries. The information
                                        is included the plot of total Covid-19 cases, plot of Covid-19 cases in each country, and the map showing the
                                        cases.")
                                     
                                     ),
                leafletOutput("sea_map", height = "600px")
               ),
        tabItem(tabName = "plot", h1("Total Cases of Covid-19", align = "center",
                                    h4("Southeast Asian countries with the highest numbers of confirmed coronavirus cases are 
                                       Indonesia, Philippines, Singapore, and Malaysia."),
                                    h4("Among the earliest countries to report Covid-19 cases after the outbreak in China were
                                       Thailand and Vietnam, but these countries had successfully controlled the pandemic."),
                                    h4("As of October, Indonesia has the highest number of cases, ahead of the Philippines.")
                                    ),
                plotlyOutput(outputId = "plot_line"),
                
                plotlyOutput(outputId = "plot_bar")),
        
        tabItem(tabName = "corr", h1("Correlation of Covid-19 Cases with Other Variables", align = "center",
                                     h4("Covid-19 cases show particular pattern that related with economic, health, and demographic
                                        characteristics."),
                                     ),
                selectInput(inputId = "numvar", 
                             label = "Choose Other Variable for X Axis", 
                             choices = sea_covid_update_info %>% 
                                 select(-c(country_region, Long, Lat, date, X, capital, 
                                           total_confirm, total_recover, total_death,
                                           growth_confirm, growth_death, growth_recover,
                                           cfr, recovery_rate, confirm_per_100000,
                                           country_flag)) %>% 
                                 colnames()
                ),
                plotlyOutput(outputId = "plot_corr")
        ),
        
        tabItem(tabName = "country", h1("Covid-19 Cases Per Country", align = "center",
                                     h4("See this page to get more detail information about Covid-19 cases in each 
                                        Southeast Asian Country."),
                                     ),
                selectInput(inputId = "country_in",
                            label = "Select Country", 
                            choices = unique(sea_case_growth2$country_region)),
             
                
                plotlyOutput(outputId = "plot_coun_line")
        ),
        
        tabItem(tabName = "map", align = "center",
                
                h2(paste("Souhteast Asian Cases per", sea_covid_update$date[1])),
                
                fluidRow(
                    
                    column(width = 5, 
                           valueBox(subtitle = "Number of Cases", 
                                    color = "red",
                                    value = number(sum(sea_covid_update_info$total_confirm, na.rm = T), accuracy = 1, big.mark = ","),
                                    icon = icon("virus"), 
                                    width = 12)
                    ),  
                    column(width = 5, 
                           valueBox(subtitle = "Number of Death", 
                                    color = "maroon",
                                    value = number(sum(sea_covid_update_info$total_death, na.rm = T), accuracy = 1, big.mark = ","),
                                    icon = icon("skull"),
                                    width = 12)
                    ),  
                    column(width = 5, 
                           valueBox(subtitle = "Number of Recovery", 
                                    color = "olive",
                                    value = number(sum(sea_covid_update_info$total_recover, na.rm = T), accuracy = 1, big.mark = ","),
                                    icon = icon("hospital-user"),
                                    width = 12
                           )
                    ),
                    column(width = 5, 
                           valueBox(subtitle = "Average Case Fatality Rates", 
                                    color = "lime",
                                    value = number(mean(sea_covid_update_info$cfr, na.rm = T), accuracy = 1, big.mark = ","),
                                    icon = icon("procedures"),
                                    width = 12
                           )
                    )
                    
                ),
                
                leafletOutput("covid_map", height = "600px")
                
        ),
        
        tabItem((tabName = "data"),
                dataTableOutput(outputId = "global_covid"))
    )
)

dashboardPage(
    header = header,
    body = body,
    sidebar = sidebar
)

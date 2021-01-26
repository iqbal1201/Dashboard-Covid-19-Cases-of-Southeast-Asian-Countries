function(input,output){
    
    output$plot_line <- renderPlotly({
       
        confirmed_plot_line <- sea_case_growth %>%
            ggplot(aes(date, total_confirm, color = country_region, group = country_region,
                       
                       text = glue("Country : {country_region}
                     Date : {date}
                     Case : {number(total_confirm, big.mark = ',')}
                    ")
            )) +
            scale_y_continuous(labels = number_format(big.mark = ",")) +
            scale_x_date(date_breaks = "1 month",
                         labels = date_format(format = "%b")
            ) +
            geom_line() +
            labs(title = "Total Confirm of Covid-19 in Southeast Asia Countries",
                 x = NULL,
                 y = "Number of Cases",
                 color = "Country"
            )+
            theme_algoritma+
            theme(legend.position = "none")
        
        
        ggplotly(confirmed_plot_line, tooltip = "text")
    })
    
    
    output$plot_bar <- renderPlotly({
        sea_covid_update <- sea_case_growth %>%
            filter(date == max(date))%>%
            mutate(cfr = total_death/total_confirm,
                   recovery_rate = total_recover/total_confirm)
        
        
        plot_bar <- sea_covid_update_info %>%
            ggplot(aes(x = total_confirm,
                       y = reorder(country_region, total_confirm),
                       fill = country_region,
                       text = glue("{country_region}
                         Number of Cases: {total_confirm}")))+
            geom_col()+
            scale_x_continuous(labels = scales::comma)+
            labs(title = "Total Cases of Covid-19 in Southeast Asia Countries",
                 x = "Total of Cases",
                 y = "Countries")+
            theme_algoritma+
            theme(legend.position = "none")
        
        ggplotly(plot_bar, tooltip = "text")
        
    })
    
    output$plot_corr <- renderPlotly({
        plot_corr <- sea_covid_update_info %>% 
            ggplot(aes_string(x = input$numvar, 
                       y = "confirm_per_100000")) +
            geom_jitter(aes(col = country_region,
                            text = glue("{str_to_upper(country_region)}
                         Total case per 100.000: {total_confirm}"
                              ))) +
            geom_smooth(method = "lm", formula = "y ~ x") +
            labs(x = "Variable Added",
                 y = "Total Case per 100.000",
                 title = "Correlation of Covid-19 Cases") +
            scale_color_brewer(palette = "Set3") +
            theme_algoritma +
            theme(legend.position = "none")
      
        ggplotly(plot_corr, tooltip = "text")
        
    })
    
    output$plot_coun_line <- renderPlotly({
      sea_covid_country <- sea_case_growth2 %>%
        filter(country_region == input$country_in) %>%
        pivot_longer(cols = c(total_confirm, total_recover, total_death), 
                     names_to = "var_case", 
                     values_to = "value")
      
      
      plot_coun_line <- sea_covid_country %>%
        ggplot(aes(x = date, y = value, group = var_case, color = var_case,
                   
                   text = glue("Country : {country_region}
                     Date : {date}
                     Case : {var_case}
                     Number : {number(value, big.mark = ',')}
                    ")
        )) +
        scale_y_continuous(labels = number_format(big.mark = ",")) +
        scale_x_date(date_breaks = "1 month",
                     labels = date_format(format = "%b")
        ) +
        geom_line() +
        
        labs(title = "Total Confirm of Covid-19 per Country",
             x = NULL,
             y = "Number of Cases"
        )+
        theme_algoritma +
        theme(legend.position = "none")
      
      ggplotly(plot_coun_line, tooltip = "text")
      
    })
    
    
    output$covid_map <- renderLeaflet({
      m <- leaflet(shape) %>% 
        addProviderTiles("CartoDB.DarkMatter") %>% 
        setView( lat=6, lng=115 , zoom=4) %>%
        # for choropleth
        addPolygons( 
          fillColor = ~mypalette(total_confirm), 
          color = "green",
          dashArray = "3", 
          fillOpacity = 0.6,
          weight=1,
          label = mytext,
          labelOptions = labelOptions( 
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"),
          popup = popup_shape
        ) %>%
        addLegend(pal=mypalette, 
                  values=~total_confirm, opacity=0.9, 
                  title = paste("Covid-19 Cases","<br>","Total Confirm"), 
                  position = "bottomleft")
      
      m
    })
    
    
    output$sea_map <- renderLeaflet({
        content_popup <- paste(sep = " ",
                               "Region :", sea_covid_update_info$country_region, "<br>",
                               "Capital :", sea_covid_update_info$capital, "<br>",
                               "Population :", number(sea_covid_update_info$population, big.mark = ",", accuracy = 1), "<br>",
                               "Population Density :", number(sea_covid_update_info$population_density, big.mark = ",", accuracy = 1), "<br>" 
                               
        )
        
        
        leaflet() %>%
            addProviderTiles(providers$Esri.WorldImagery) %>%
            addMarkers(data = sea_covid_update_info ,
                       lng = ~Long, lat = ~Lat, 
                       popup = content_popup,
                       clusterOptions = markerClusterOptions()
            )
        
    })
    
    output$global_covid <- renderDataTable({
        DT::datatable(data = sea_case_growth, options = list(scrollX = T))
    })
}
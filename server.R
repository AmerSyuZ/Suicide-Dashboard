#Main App
function(input, output)  {
   
  #PAGE1    
  #row1-----------------------------------------------------------------------------------------------------------   
  
  output$total_suicide <- renderValueBox({
    total_suicide <-  suicide %>%
      group_by(continent)  %>%
      summarise(jumlah=sum(suicides_numbers)) %>% 
      summarise(freq=sum(jumlah)) %>% 
      pull(freq)
    
    
    
    valueBox(
      value = comma(total_suicide),
      subtitle = "Number of Suicides Recorded",
      color = "purple",
      icon = icon("exclamation-triangle"),
      width = 12
    )
    
  })
  
  output$ratesui <- renderValueBox({
    rate_sui <- suicide %>%
      group_by(continent)  %>%
      summarise(ratarata=100000*sum(suicides_numbers)/sum(population)) %>% 
      summarise(freq=round(mean(ratarata),1)) %>% 
      pull(round(freq,1))
    
    
    
    valueBox(
      value = rate_sui,
      subtitle = "Number of Suicides per 100k Population",
      icon = icon("exclamation-triangle"),
      color = "purple",
      width = 12
    )
    
  })
  
  
  
  output$maxcountry <- renderValueBox({
    max_country <- suicide %>% 
      group_by(country) %>% 
      summarise(freq=sum(suicides_numbers)) %>% 
      arrange(desc(freq)) %>% 
      head(1) %>% 
      pull(country)
    
    
    
    valueBox(
      value = max_country,
      subtitle = "Country With The Highest Numbers of Suicides",
      color = "purple",
      icon = icon("exclamation"),
      width = 12
    )
    
  })
  
  
  
  output$ratemaxcountry <- renderValueBox({
    rate_max_country <- suicide %>% 
      group_by(country) %>% 
      summarise(freq=mean(suicides_rate)) %>% 
      arrange(desc(freq)) %>% 
      head(1) %>% 
      pull(country)
    
    
    
    valueBox(
      value = rate_max_country,
      subtitle = "Country With The Highest Suicides/100k Population",
      icon = icon("exclamation"),
      color = "purple",
      width = 12
    )
    
  })
  
  #row2---------------------------------------------------------------------------------------------------   
  
  reactive_continent <- reactive({
    suicide %>%
      filter(continent == input$continentrate)
  })
  
  output$numsuicont <- renderValueBox({
    num_sui_cont <- reactive_continent() %>%
      group_by(continent)  %>%
      summarise(jumlah=sum(suicides_numbers)) %>% 
      summarise(freq=sum(jumlah)) %>% 
      pull(freq)
    
    
    
    valueBox(
      value = comma(num_sui_cont),
      subtitle = "Number of Suicides",
      
      color = "teal",
      width = 12
    )
    
  })
  
  output$ratesuicont <- renderValueBox({
    
    rate_sui_cont <- reactive_continent () %>%
      group_by(continent) %>%
      summarise(freq= round(mean(suicides_rate),1)) %>%
      pull(freq)
    
    
    
    valueBox(
      value = rate_sui_cont,
      subtitle = "Suicides per 100k Population",
      color = "green",
      width = 12
    )
    
  })
  
  
  
  
  output$suitimeline <- renderPlotly({
    
    sui_timeline <- reactive_continent () %>%
      group_by(year,continent) %>%
      summarise(freq = sum(suicides_numbers)) %>%
      mutate(
        label = glue(
          "Year Recorded: {year}
                     Continent: {continent}
                     Number of Suicides: {comma(freq)}"
        )
      ) %>%
      ggplot(aes(
        x = year,
        y = freq,
        text = label,
        group = 1
      )) +
      geom_line(aes(col=continent)) + geom_point() + geom_area(fill="#02d6d9",alpha=0.4)+
      scale_y_continuous(labels=comma)+
      labs(title = "",
           x = "Year",
           y = "Number of Suicides") +
      theme_bw()
    
    ggplotly(sui_timeline, tooltip = "text") %>% layout(showlegend = F)
    
  })
  
  
  output$suiyearly <- renderPlotly({
    
    suicideyear <- reactive_continent () %>%
      group_by(year,continent) %>%
      summarise(suicidesrate = round(mean(suicides_rate),1)) %>%
      mutate(
        label = glue(
          "Year Recorded: {year}
                     Continent: {continent}
                     Avg Number of Suicides/100k Population: {suicidesrate}"
        )
      ) %>%
      ggplot(aes(
        x = year,
        y = suicidesrate,
        text = label,
        group = 1
      )) +
      geom_line(aes(col=continent)) + geom_point() + geom_area(fill="#54e88f",alpha=0.4)+
      labs(title = "",
           x = "Year",
           y = "Avg Number of Suicides/100k Population") +
      theme_bw()
    
    ggplotly(suicideyear, tooltip = "text") %>% layout(showlegend = F)
    
  })
  
  #Row3------------------------------------------------------------------------------------------------------------    
  
  reactive_country <- reactive({
    suicide %>%
      filter(country == input$countryrate)
  })
  
  output$numsuicountry <- renderValueBox({
    num_sui_country <- reactive_country() %>%
      group_by(country)  %>%
      summarise(jumlahcountry=sum(suicides_numbers)) %>% 
      summarise(freq=sum(jumlahcountry)) %>% 
      pull(freq)
    
    
    
    valueBox(
      value = comma(num_sui_country),
      subtitle = "Number of Suicides",
      
      color = "teal",
      width = 12
    )
    
  })
  
  output$ratesuicountry <- renderValueBox({
    
    rate_sui_country <- reactive_country () %>%
      group_by(country) %>%
      summarise(freq= round(mean(suicides_rate),1)) %>%
      pull(freq)
    
    
    
    valueBox(
      value = rate_sui_country,
      subtitle = "Suicides per 100k Population",
      
      color = "green",
      width = 12
    )
    
  })
  
  output$suitimelinecountry <- renderPlotly({
    
    sui_timeline_country <- reactive_country () %>%
      group_by(year,country) %>%
      summarise(freq = sum(suicides_numbers)) %>%
      mutate(
        label = glue(
          "Year Recorded: {year}
                     Country: {country}
                     Number of Suicides: {comma(freq)}"
        )
      ) %>%
      ggplot(aes(
        x = year,
        y = freq,
        text = label,
        group = 1
      )) +
      geom_line(aes(col="darkblue")) + geom_point() + geom_area(fill="#02d6d9",alpha=0.4)+
      scale_y_continuous(labels=comma)+
      labs(title = "",
           x = "Year",
           y = "Number of Suicides") +
      theme_bw()
    
    ggplotly(sui_timeline_country, tooltip = "text") %>% layout(showlegend = F)
    
  })
  
  
  
  
  output$suiage <- renderPlotly({
    
    sui_age <- reactive_country() %>%
      group_by(year,country) %>%
      summarise(freq = round(mean(suicides_rate),1)) %>%
      mutate(
        label = glue(
          "Year Recorded: {year}
                     Country: {country}
                     Avg Number of Suicides/100k Population: {freq}"
        )
      ) %>%
      ggplot(aes(
        x = year,
        y = freq,
        text = label,
        group = 1
      )) +
      geom_line(aes(col="darkblue")) + geom_point() + geom_area(fill="#54e88f",alpha=0.4)+
      labs(title = "",
           x = "Year",
           y = "Avg Number of Suicides/100k Population") +
      theme_bw()
    
    ggplotly(sui_age, tooltip = "text") %>% layout(showlegend = F)
    
    
  })
  # --------------------- PAGE 1: LINE PLOT
  
  output$lineContinent <- renderPlotly({
    continent_line_plot <-
      ggplot(data = continent_line_data,
             aes(x = year, y = rate,
                 group = continent, color = continent,
                 text = label)) +
      geom_line(lwd = 0.75) +
      geom_point(size = 1) +
      scale_x_continuous(breaks = seq(1985, 2016, by = 5),
                         limits = c(1985, 2016)) +
      labs(x = "Year", y = "Suicide per 100,000 population",
           title = "<b>Suicide Rates Over the Years</b>",
           color = "") +
      theme_minimal()
    
    ggplotly(continent_line_plot, tooltip = "text")
  })    
    
#PAGE2
#Row1--------------------------------------------------------------------------------------------------------------------------------------------
    
    output$nummale <- renderValueBox({
        num_male <-  suicide %>% 
            filter(sex=="male") %>% 
            group_by(sex)  %>%
            summarise(freq=sum(suicides_numbers)) %>% 
            pull(freq)
           
       
        
        
        
        valueBox(
            value = comma(num_male),
            subtitle = "Number of Suicides by Male",
            color = "aqua",
            icon = icon("mars"),
            width = 12
        )
        
    })
    
    output$numfemale <- renderValueBox({
        num_female <- suicide %>% 
            filter(sex=="female") %>% 
            group_by(sex)  %>%
            summarise(freq=sum(suicides_numbers)) %>% 
            pull(freq)
        
        
        
        valueBox(
            value = comma(num_female),
            subtitle = "Number of Suicides by Female",
            icon = icon("venus"),
            color = "fuchsia",
            width = 12
        )
        
    })
    
    output$maxage <- renderValueBox({
        max_age <- suicide %>% 
            group_by(group_age)  %>%
            summarise(freq=sum(suicides_numbers)) %>% 
            arrange(desc(freq)) %>% 
            head(1) %>% 
            pull(group_age)
        
        
        
        valueBox(
            value = max_age,
            subtitle = "Age Group With The Highest Numbers of Suicides",
            icon = icon("child"),
            color = "teal",
            width = 12
        )
        
    })
    
    output$minage <- renderValueBox({
        min_age <- suicide %>% 
            group_by(group_age)  %>%
            summarise(freq=sum(suicides_numbers)) %>% 
            arrange(desc(freq)) %>% 
            tail(1) %>% 
            pull(group_age)
        
        
        
        valueBox(
            value = min_age,
            subtitle = "Age Group With The Lowest Numbers of Suicides",
            icon = icon("child"),
            color = "olive",
            width = 12
        )
        
    })
    
#Row2------------------------------------------------------------------------------------------------------------------------------------------    
    
    reactive_gender <- reactive({
        suicide %>%
            filter(continent == input$contgender)
    })
    
    output$numcontmale <- renderValueBox({
        
        num_cont_male <- reactive_gender() %>%
            filter(sex=="male")  %>%
            group_by(sex) %>% 
            summarise(freq=sum(suicides_numbers)) %>% 
            pull(freq)
        
        
        
        valueBox(
            value = comma(num_cont_male),
            subtitle = "Number of Suicides by Male ",
            icon = icon("mars"),
            color = "aqua",
            width = 12
        )
        
    })
    
    output$numcontfemale <- renderValueBox({
        
        num_cont_female <- reactive_gender() %>%
            filter(sex=="female")  %>%
            group_by(sex) %>% 
            summarise(freq=sum(suicides_numbers)) %>% 
            pull(freq)
        
        
        
        valueBox(
            value = comma(num_cont_female),
            subtitle = "Number of Suicides by Female",
            icon = icon("venus"),
            color = "fuchsia",
            width = 12
        )
        
    })
    
    output$numgendercount <- renderPlotly({
        
        num_gender_count <- reactive_gender () %>%
            group_by(sex,group_age) %>% 
            
            summarise(freq=sum(suicides_numbers)) %>% 
            mutate(label=glue(
                "Age Group: {group_age}
                 Number of Sucides by {sex}: {comma(freq)}
                 Continent: {input$contgender}"
            )) %>% 
            ggplot(aes(y=sex,x=freq,text=label)) + geom_col(aes(col=group_age,fill=group_age),stat="identity", width=0.7) +
            scale_x_continuous(labels=comma)+
            scale_color_viridis_d() +
            scale_fill_viridis_d() +
            theme_bw() +
            labs(x="Number of Suicides",
                 y="",
                 fill=""
                 
            )
        
        ggplotly(num_gender_count, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.2, y = -0.3))
        
        
    })
    
#Row3--------------------------------------------------------------------------------------------------------------------------------------------   
 
    reactive_gendercountry <- reactive({
        suicide %>%
            filter(country == input$countrygender)
    })
    
    output$numcountrymale <- renderValueBox({
        
        num_country_male <- reactive_gendercountry() %>%
            filter(sex=="male")  %>%
            group_by(sex) %>% 
            summarise(freq=sum(suicides_numbers)) %>% 
            pull(freq)
        
        
        
        valueBox(
            value = comma(num_country_male),
            subtitle = "Number of Suicides by Male",
            icon = icon("mars"),
            color = "aqua",
            width = 12
        )
        
    })
    
    output$numcountryfemale <- renderValueBox({
        
        num_country_female <- reactive_gendercountry() %>%
            filter(sex=="female")  %>%
            group_by(sex) %>% 
            summarise(freq=sum(suicides_numbers)) %>% 
            pull(freq)
        
        
        
        valueBox(
            value = comma(num_country_female),
            subtitle = "Number of Suicides by Female",
            icon = icon("venus"),
            color = "fuchsia",
            width = 12
        )
        
    })
    
    output$numgendercountry <- renderPlotly({
        
        num_gender_country <- reactive_gendercountry () %>%
            group_by(sex,group_age) %>% 
            
            summarise(freq=sum(suicides_numbers)) %>% 
            mutate(label=glue(
                "Age Group: {group_age}
                 Number of Sucides by {sex}: {comma(freq)}
                 Country : {input$countrygender}"
            )) %>% 
            ggplot(aes(y=sex,x=freq,text=label)) + geom_col(aes(col=group_age,fill=group_age),stat="identity", width=0.7) +
            scale_x_continuous(labels=comma)+
            scale_color_viridis_d() +
            scale_fill_viridis_d() +
            theme_bw() +
            labs(x="Number of Suicides",
                 y="",
                 fill=""
                 
            )
        
        ggplotly(num_gender_country, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.2, y = -0.3))
        
        
    })
    
    output$age_bar <- renderPlotly({
      
      
      age_bar_data <-
        suicide %>% 
        
        group_by(group_age, sex) %>% 
        summarise(total_suicides = sum(suicides_numbers),
                  total_pop = sum(population)) %>% 
        ungroup() %>% 
        mutate(suicides_rate = 100000*total_suicides/total_pop) %>%
        mutate(sex = str_to_title(sex),
               label = paste0(
                 '<b>', sex, ' (', group_age, ' Years)</b><br>',
                 'Rate: ', round(suicides_rate, 2), ' per 100,000'))
      
      age_bar_plot <-
        ggplot(data = age_bar_data,
               aes(x = group_age, y = suicides_rate, text = label)) +
        geom_col(aes(fill = sex), position = "dodge") +
        labs(x = "Age Group (Years)",
             y = "Suicide per 100,000 population",
             fill = "") +
        theme_minimal()
      
      ggplotly(age_bar_plot, tooltip = "text") %>%
        layout(legend = list(orientation = "h",
                             xanchor = "center",
                             x = 0.5,
                             y = -0.25),
               margin = list(t = 70),
               title = list(x = 0,
                            y = 0.95,
                            text = paste0('<b>Suicide Rates by Age and Gender</b><br>',
                                          '<sup>',
                                          "Worldwide",
                                          '</sup>')))
    })
    
    
#Page3---------------------------------------------------------------------------------------------------------------------------------------------   
    
    
    reactive_map <- reactive({
      suicide %>%
        filter(year == input$suicide_map)
    })
    
    output$totalcountry <- renderValueBox({
      total_country <-  reactive_map() %>%
        group_by(country)  %>%
        summarise(sui=sum(suicides_numbers)) %>% 
        group_by(country) %>% 
        summarise(freq=n()) %>% 
        count() %>% 
        pull(n)
      
      
      
      valueBox(
        value = total_country,
        subtitle = "Countries ",
        color = "purple",
        icon = icon("globe"),
        width = 12
      )
      
    })
    
    output$totalnumyear <- renderValueBox({
      total_numyear <-  reactive_map() %>%
        group_by(continent)  %>%
        summarise(sui=sum(suicides_numbers)) %>% 
        summarise(freq=sum(sui)) %>% 
        pull(freq)
      
      
      
      valueBox(
        value = comma(total_numyear),
        subtitle = "Number of Suicides ",
        color = "purple",
        icon = icon("globe"),
        width = 12
      )
      
    })
    
    output$totalrateyear <- renderValueBox({
      total_rateyear <-  reactive_map() %>%
        group_by(continent)  %>%
        summarise(sui=mean(suicides_rate)) %>% 
        summarise(freq=mean(sui)) %>% 
        pull(freq)
      
      
      
      valueBox(
        value = round(total_rateyear,1),
        subtitle = "Number of Suicides/100K Population ",
        color = "purple",
        icon = icon("globe"),
        width = 12
      )
      
    })
    
    
    output$suicidemap <- renderPlotly({
      
      sui_map <- reactive_map() %>% group_by(country) %>% summarise(freq=round(sum(suicides_numbers),2)) %>% 
        mutate(country_code=countrycode(country,origin="country.name",destination="iso3c"))
      
      
      l <- list(color = toRGB("grey"), width = 0.5)
      
      g <- list(
        resolution=200,
        showcoastlines=T, coastlinecolor="RebeccaPurple",
        showland=F, landcolor="grey85",
        showocean=T, oceancolor="white",
        showlakes=T, lakecolor="Lightblue",
        showrivers=T, rivercolor="Lightblue",
        projection = list(type = 'natural earth', scale = 1)
      )
      
      plot_ly(sui_map, z = sui_map$freq , text = sui_map$country ,locations = sui_map$country_code,  type = 'choropleth',
              color = sui_map$freq, colors = 'YlGnBu', marker = list(line = l ),colorbar = list(title = "Number of Suicides")) %>% 
        layout(geo=g)
      
      
      
    })
    
    output$totaldemographic <- renderPlotly({
      
      total_demographic <- reactive_map () %>%
        group_by(sex,group_age) %>% 
        
        summarise(freq=sum(suicides_numbers)) %>% 
        mutate(label=glue(
          "Age Group: {group_age}
                 Number of Sucides by {sex}: {comma(freq)}
                 year: {input$suicide_map}
          "
        )) %>% 
        ggplot(aes(x=sex,y=freq,text=label)) + geom_bar(aes(col=group_age,fill=group_age),stat="identity",width=0.5) +
        scale_y_continuous(labels=comma)+
        theme_bw() +
        labs(y="Number of Suicides",
             x="",
             fill=""
             
        )
      
      ggplotly(total_demographic, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.1, y = -0.1))
      
      
    }) 
#Page4-------------------------------------------------------------------------------------------------------------------------------------------    
    
    output$datasuicide <- renderDataTable({suicide})
        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
}
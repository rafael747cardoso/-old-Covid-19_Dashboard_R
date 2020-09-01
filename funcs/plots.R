
### Plot the cumulative cases

plot_cumulative_cases = function(df, 
                                 type_color,
                                 type_scale){
    
    df$valueok = format(df$cases_cumulative, 
                            big.mark = " ")
    plot_ly(data = df,
            x = ~date,
            y = ~cases_cumulative,
            type = "scatter",
            mode = "lines+markers",
            marker = list(color = type_color,
                          size = 7),
            line = list(color = type_color,
                        width = 4),
            text = ~valueok,
            hovertemplate = paste0("<b>Cumulative cases: %{text}</b><br>",
                                   "<b>Date: %{x}</b>"),
            name = " "
            ) %>%
        layout(xaxis = list(title = "Date",
                            titlefont = list(size = 16)),
               yaxis = list(title = "Cumulative cases",
                            titlefont = list(size = 16),
                            type = type_scale)
               )
}

### Plot the cumulative deaths

plot_cumulative_deaths = function(df, 
                                  type_color, 
                                  type_scale){
    
    df$valueok = format(df$deaths_cumulative, 
                            big.mark = " ")
    plot_ly(data = df,
            x = ~date,
            y = ~deaths_cumulative,
            type = "scatter",
            mode = "lines+markers",
            marker = list(color = type_color,
                          size = 7),
            line = list(color = type_color,
                        width = 4),
            text = ~valueok,
            hovertemplate = paste0("<b>Cumulative deaths: %{text}</b><br>",
                                   "<b>Date: %{x}</b>"),
            name = " "
            ) %>%
        layout(xaxis = list(title = "Date",
                            titlefont = list(size = 16)),
               yaxis = list(title = "Cumulative deaths",
                            titlefont = list(size = 16),
                            type = type_scale))
}

### Plot the cumulative recovered

plot_cumulative_recovered = function(df, 
                                     type_color,
                                     type_scale){
    
    df$valueok = format(df$recovered_cumulative, 
                            big.mark = " ")
    plot_ly(data = df,
            x = ~date,
            y = ~recovered_cumulative,
            type = "scatter",
            mode = "lines+markers",
            marker = list(color = type_color,
                          size = 7),
            line = list(color = type_color,
                        width = 4),
            text = ~valueok,
            hovertemplate = paste0("<b>Cumulative recovered: %{text}</b><br>",
                                   "<b>Date: %{x}</b>"),
            name = " "
            ) %>%
        layout(xaxis = list(title = "Date",
                            titlefont = list(size = 16)),
               yaxis = list(title = "Cumulative recovered",
                            titlefont = list(size = 16),
                            type = type_scale))
}

### Plot the cases per day

plot_perday_cases = function(df, 
                             type_color, 
                             type_scale){
    
    df$valueok = format(df$cases_per_day, 
                            big.mark = " ")
    plot_ly(data = df,
            x = ~date,
            y = ~cases_per_day,
            type = "scatter",
            mode = "lines+markers",
            marker = list(color = type_color,
                          size = 7),
            line = list(color = type_color,
                        width = 4),
            text = ~valueok,
            hovertemplate = paste0("<b>Cases per day: %{text}</b><br>",
                                   "<b>Date: %{x}</b>"),
            name = " "
            ) %>%
        layout(xaxis = list(title = "Date",
                            titlefont = list(size = 16)),
               yaxis = list(title = "Cases per day",
                            titlefont = list(size = 16),
                            type = type_scale))
}

### Plot the deaths per day

plot_perday_deaths = function(df, 
                              type_color, 
                              type_scale){
    
    df$valueok = format(df$deaths_per_day, 
                            big.mark = " ")
    plot_ly(data = df,
            x = ~date,
            y = ~deaths_per_day,
            type = "scatter",
            mode = "lines+markers",
            marker = list(color = type_color,
                          size = 7),
            line = list(color = type_color,
                        width = 4),
            text = ~valueok,
            hovertemplate = paste0("<b>Deaths per day: %{text}</b><br>",
                                   "<b>Date: %{x}</b>"),
            name = " "
            ) %>%
        layout(xaxis = list(title = "Date",
                            titlefont = list(size = 16)),
               yaxis = list(title = "Deaths per day",
                            titlefont = list(size = 16),
                            type = type_scale))
}

### Plot the recovered per day

plot_perday_recovered = function(df,
                                 type_color,
                                 type_scale){
    
    df$valueok = format(df$recovered_per_day, 
                            big.mark = " ")
    plot_ly(data = df,
            x = ~date,
            y = ~recovered_per_day,
            type = "scatter",
            mode = "lines+markers",
            marker = list(color = type_color,
                          size = 7),
            line = list(color = type_color,
                        width = 4),
            text = ~valueok,
            hovertemplate = paste0("<b>Recovered per day: %{text}</b><br>",
                                   "<b>Date: %{x}</b>"),
            name = " "
            ) %>%
        layout(xaxis = list(title = "Date",
                            titlefont = list(size = 16)),
               yaxis = list(title = "Recovered per day",
                            titlefont = list(size = 16),
                            type = type_scale))
}

### Plot the cases trajectories for some countries

plot_countries_trajectories_cases = function(countries,
                                             type_scale,
                                             mov_avg){
    
    n_days = as.numeric(strsplit(mov_avg, " ")[[1]][1])
    data = list()
    for(i in 1:length(countries)){
        df = prepare_dataset(country_chosen = countries[i]) %>%
                 dplyr::select(cases_cumulative,
                               cases_per_day)
        mean_cases_cumulative = NULL
        mean_cases_per_day = NULL
        k = n_days
        for(j in seq(from = 1, to = nrow(df), by = n_days)){
            mean_cases_cumulative = c(mean_cases_cumulative,
                                      mean(df$cases_cumulative[j:k]))
            mean_cases_per_day = c(mean_cases_per_day,
                                   mean(df$cases_per_day[j:k]))
            k = k + n_days
        }
        df = data.frame("mean_cases_cumulative" = mean_cases_cumulative,
                        "mean_cases_per_day" = mean_cases_per_day) %>%
                  drop_na() %>%
                 dplyr::filter(mean_cases_cumulative > 0 &
                               mean_cases_per_day > 0)
        data[[i]] = df
        
    }
    names(data) = countries
    
    p = plot_ly()
    for(i in 1:length(data)){
        p = p %>%
            add_trace(data = data[[i]],
                      x = ~mean_cases_cumulative,
                      y = ~mean_cases_per_day,
                      type = "scatter",
                      mode = "lines",
                      line = list(width = 4),
                      text = names(data)[i],
                      name = names(data)[i],
                      hovertemplate = paste0("<b>%{text}</b><br>",
                                             "Cases per day: %{x:.1f}<br>",
                                             "Cumulative cases: %{y:.1f}")
                      )
    }
    p = p %>% 
        layout(xaxis = list(title = "Cumulative cases",
                            titlefont = list(size = 16),
                            type = type_scale),
               yaxis = list(title = "Cases per day",
                            titlefont = list(size = 16),
                            type = type_scale))

    p
}

### Plot the deaths trajectories for some countries

plot_countries_trajectories_deaths = function(countries,
                                           type_scale,
                                              mov_avg){
    
    n_days = as.numeric(strsplit(mov_avg, " ")[[1]][1])
    data = list()
    for(i in 1:length(countries)){
        df = prepare_dataset(country_chosen = countries[i]) %>%
                 dplyr::select(deaths_cumulative,
                               deaths_per_day)
        mean_deaths_cumulative = NULL
        mean_deaths_per_day = NULL
        k = n_days
        for(j in seq(from = 1, to = nrow(df), by = n_days)){
            mean_deaths_cumulative = c(mean_deaths_cumulative,
                                       mean(df$deaths_cumulative[j:k]))
            mean_deaths_per_day = c(mean_deaths_per_day,
                                    mean(df$deaths_per_day[j:k]))
            k = k + n_days
        }
        df = data.frame("mean_deaths_cumulative" = mean_deaths_cumulative,
                        "mean_deaths_per_day" = mean_deaths_per_day) %>%
                  drop_na() %>%
                 dplyr::filter(mean_deaths_cumulative > 0 &
                               mean_deaths_per_day > 0)
        data[[i]] = df
        
    }
    names(data) = countries
    
    p = plot_ly()
    for(i in 1:length(data)){
        p = p %>%
            add_trace(data = data[[i]],
                      x = ~mean_deaths_cumulative,
                      y = ~mean_deaths_per_day,
                      type = "scatter",
                      mode = "lines",
                      line = list(width = 4),
                      text = names(data)[i],
                      name = names(data)[i],
                      hovertemplate = paste0("<b>%{text}</b><br>",
                                             "Deaths per day: %{x:.1f}<br>",
                                             "Cumulative deaths: %{y:.1f}")
                      )
    }
    p = p %>% 
        layout(xaxis = list(title = "Cumulative deaths",
                            titlefont = list(size = 16),
                            type = type_scale),
               yaxis = list(title = "Deaths per day",
                            titlefont = list(size = 16),
                            type = type_scale))

    p
}

### Plot the recovered trajectories for some countries

plot_countries_trajectories_recovered = function(countries,
                                                 type_scale,
                                                 mov_avg){
    
    n_days = as.numeric(strsplit(mov_avg, " ")[[1]][1])
    data = list()
    for(i in 1:length(countries)){
        df = prepare_dataset(country_chosen = countries[i]) %>%
                 dplyr::select(recovered_cumulative,
                               recovered_per_day)
        mean_recovered_cumulative = NULL
        mean_recovered_per_day = NULL
        k = n_days
        for(j in seq(from = 1, to = nrow(df), by = n_days)){
            mean_recovered_cumulative = c(mean_recovered_cumulative,
                                          mean(df$recovered_cumulative[j:k]))
            mean_recovered_per_day = c(mean_recovered_per_day,
                                       mean(df$recovered_per_day[j:k]))
            k = k + n_days
        }
        df = data.frame("mean_recovered_cumulative" = mean_recovered_cumulative,
                        "mean_recovered_per_day" = mean_recovered_per_day) %>%
                  drop_na() %>%
                 dplyr::filter(mean_recovered_cumulative > 0 &
                               mean_recovered_per_day > 0)
        data[[i]] = df
        
    }
    names(data) = countries
    
    p = plot_ly()
    for(i in 1:length(data)){
        p = p %>%
            add_trace(data = data[[i]],
                      x = ~mean_recovered_cumulative,
                      y = ~mean_recovered_per_day,
                      type = "scatter",
                      mode = "lines",
                      line = list(width = 4),
                      text = names(data)[i],
                      name = names(data)[i],
                      hovertemplate = paste0("<b>%{text}</b><br>",
                                             "Recovered per day: %{x:.1f}<br>",
                                             "Cumulative recovered: %{y:.1f}")
                      
                      )
    }
    p = p %>% 
        layout(xaxis = list(title = "Cumulative recovered",
                            titlefont = list(size = 16),
                            type = type_scale),
               yaxis = list(title = "Recovered per day",
                            titlefont = list(size = 16),
                            type = type_scale))

    p
}









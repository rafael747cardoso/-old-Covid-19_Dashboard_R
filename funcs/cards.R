
### Card of total cases

card_total_cases = function(df){
    valueBox(value = format(df$cases_cumulative[nrow(df)], 
                            big.mark = " "), 
             subtitle = tags$p(style = "font-size: 150%;", 
                               "Total cases"), 
             icon = NULL, 
             color = "orange")
}

### Card of total deaths

card_total_deaths = function(df){
    valueBox(value = format(df$deaths_cumulative[nrow(df)], 
                            big.mark = " "), 
             subtitle = tags$p(style = "font-size: 150%;", 
                               "Total deaths"), 
             icon = NULL, 
             color = "red")
}

### Card of total recovered

card_total_recovered = function(df){
    valueBox(value = format(df$recovered_cumulative[nrow(df)], 
                            big.mark = " "), 
             subtitle = tags$p(style = "font-size: 150%;", 
                               "Total recovered"), 
             icon = NULL, 
             color = "green")
}



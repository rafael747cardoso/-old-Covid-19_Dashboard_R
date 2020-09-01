
### Function to fix the issue with the decimal mark and big mark in addLegend

labelFormat_decimal = function(prefix = "", 
                               suffix = "",
                               between = " &ndash; ",
                               digits = 3,
                               big.mark = ",",
                               transform = identity,
                               decimal.mark = "."){
    
        formatNum = function(x){
            format(round(transform(x), digits), trim=TRUE, scientific=FALSE,
                   big.mark=big.mark, decimal.mark=decimal.mark)
        }
        function(type, ...){
            switch(type, numeric = (function(cuts){
                paste0(prefix, formatNum(cuts), suffix)
            })(...), bin = (function(cuts){
                n = length(cuts)
                paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]),
                       suffix)
            })(...), quantile = (function(cuts, p){
                n = length(cuts)
                p = paste0(round(p * 100), "%")
                cuts = paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
                paste0("<span title=\"", cuts, "\">", prefix, p[-n],
                       between, p[-1], suffix, "</span>")
            })(...), factor = (function(cuts){
                paste0(prefix, as.character(transform(cuts)), suffix)
            })(...))
        }
    }

### Function to create a map for countries

create_map = function(covid_var,
                      countries_pol) {
    
    # Chosen variable:
    df_variables = data.frame("var_name" = c("Cases per Day",
                                             "Deaths per Day",
                                             "Recovered per Day",
                                             "Cases Cumulative",
                                             "Deaths Cumulative",
                                             "Recovered Cumulative",
                                             "Cases Cumulative per Population",
                                             "Deaths Cumulative per Population",
                                             "Recovered Cumulative per Population",
                                             "Population 2020"),
                              "var" = c("cases_per_day",
                                        "deaths_per_day",
                                        "recovered_per_day",
                                        "cases_cumulative",
                                        "deaths_cumulative",
                                        "recovered_cumulative",
                                        "cases_cumulative_pop",
                                        "deaths_cumulative_pop",
                                        "recovered_cumulative_pop",
                                        "population_2020"),
                              stringsAsFactors = FALSE)
    var_chosen = (df_variables %>%
                       dplyr::filter(var_name == covid_var))$var
    
    # Most recent data from each country:
    df = prepare_dataset(country_chosen = "All") %>%
             dplyr::select(country,
                           eval(var_chosen))

    # Fix countries names to match those from the shapefile:
    df = df %>%
             dplyr::mutate(country = dplyr::case_when(
                country == "Bahamas" ~ "The Bahamas",
                country == "Burma" ~ "Myanmar",
                country == "Cabo Verde" ~ "Cape Verde",
                country == "Congo (Brazzaville)" ~ "Republic of Congo",
                country == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
                country == "Cote d'Ivoire" ~ "Ivory Coast",
                country == "Czechia" ~ "Czech Republic",
                country == "Eswatini" ~ "Swaziland",
                country == "Guinea-Bissau" ~ "Guinea Bissau",
                country == "Holy See" ~ "Vatican",
                country == "Korea, South" ~ "South Korea",
                country == "North Macedonia" ~ "Macedonia",
                country == "Serbia" ~ "Republic of Serbia",
                country == "Taiwan*" ~ "Taiwan",
                country == "Tanzania" ~ "United Republic of Tanzania",
                country == "Timor-Leste" ~ "East Timor",
                country == "US" ~ "United States of America",
                TRUE ~ country))
    
    # Remove from the shapefile the countries that aren't in coronavirus dataset:
    countries_pol = countries_pol[countries_pol$ADMIN %in% df$country, ]
    
    # Add the df variable to the shapefile:
    countries_pol@data = countries_pol@data %>%
                             dplyr::inner_join(df,
                                               by = c("ADMIN" = "country"))

    # Bins:
    var_values = (countries_pol@data %>% 
                      dplyr::select(var_chosen))[, 1]
    
    n = 9
    bins_limits = kmeans(x = var_values, centers = n - 2)$centers %>% 
                          as.numeric() %>%
                          sort()
    bins_limits = c(min(var_values), bins_limits, max(var_values))
    if(!(var_chosen %in% c("cases_cumulative_pop",
                           "deaths_cumulative_pop",
                           "recovered_cumulative_pop"))){
        bins_limits = bins_limits %>%
                          round(., 0)
    } else{
        bins_limits = bins_limits %>%
                          round(., 4)
    }
    repeated = bins_limits %>%
                   duplicated() %>%
                   which()
    if(length(repeated) > 0){
        if(length(bins_limits) - length(repeated) > 2){
            bins_limits = bins_limits[-repeated]
        } else{
            bins_limits = seq(from = min(var_values),
                              to = max(var_values),
                              length.out = 10)
        }
    }
    
    # Colors:
    if(tolower(str_split(covid_var, " ")[[1]][1]) == "cases"){
        var_color = "#f49a17"
    }
    if(tolower(str_split(covid_var, " ")[[1]][1]) == "deaths"){
        var_color = "#fe4934"
    }
    if(tolower(str_split(covid_var, " ")[[1]][1]) == "recovered"){
        var_color = "#2a7a16"
    }
    if(tolower(str_split(covid_var, " ")[[1]][1]) == "population"){
        var_color = "#0f1c9b"
    }
    color_func = colorRampPalette(c("white", var_color))
    palette = colorBin(color_func(length(var_values)),
                       domain = var_values,
                       bins = bins_limits,
                       pretty = TRUE)
    
    # Labels:
    labels = sprintf(paste0("<strong>%s: %s</strong>"),
                     countries_pol@data$ADMIN,  
                     format(var_values, 
                            big.mark = " ")
                     ) %>%
                  lapply(htmltools::HTML)
    
    # Map:
    map = leaflet(data = countries_pol,
            options = leafletOptions(minZoom = 1,
                                     maxZoom = 30)
            ) %>% 
        setView(lng = 0, 
                lat = 15,
                zoom = 1.5) %>%
        addTiles() %>%
        addPolygons(fillColor = palette(var_values),
                    weight = 0.5,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(weight = 2,
                                                 color = "#666",
                                                 dashArray = "",
                                                 fillOpacity = 0.7,
                                                 bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(style =list("font-weight" = "normal",
                                                            "padding" = "3px 8px"),
                                                textsize = "15px",
                                                direction = "auto")) %>% 
        addLegend(pal = palette, 
                  values = var_values, 
                  opacity = 0.7,
                  title = ifelse(grepl("per Population", covid_var),
                                 paste0(covid_var, "<br>per 1 000 000"),
                                 covid_var),
                  position = "bottomright",
                  labFormat = labelFormat_decimal(big.mark = " ",
                                                  transform = identity,
                                                  decimal.mark = "."))
    
    map
}


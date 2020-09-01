
### Function to prepere the data

prepare_dataset = function(country_chosen){
    
    # Load the dataset:
    data("coronavirus")
    df_coronavirus = coronavirus
    
    # Cast:
    df_coronavirus = df_coronavirus %>% 
                         tidyr::spread(type, cases)
    names(df_coronavirus)[c(6:8)] = c("cases_per_day",
                                      "deaths_per_day",
                                      "recovered_per_day")

    # Replace NA by 0:
    df_coronavirus = df_coronavirus %>%
                           replace(is.na(.), 0)
    
    # Remove cruise ships and countries without population estimation in countries_pop.csv::
    df_coronavirus = df_coronavirus %>%
                          dplyr::filter(!(country %in% c("Diamond Princess",
                                                         "MS Zaandam",
                                                         "West Bank and Gaza",
                                                         "Kosovo")))

    # Country/world selection:
    if(country_chosen %in% unique(df_coronavirus$country)){
        # Select the country:
        df_country = df_coronavirus %>%
                         dplyr::filter(country == country_chosen)
        
        # Remove the first zeros days:
        ind_out = 0
        for(i in 1:nrow(df_country)){
            if(sum(df_country$cases_per_day[1:i]) == 0 &
               sum(df_country$deaths_per_day[1:i]) == 0 &
               sum(df_country$recovered_per_day[1:i]) == 0){
                ind_out = i
            }
        }
        df_country = df_country[(ind_out + 1):nrow(df_country), ]
        
        # Sum of cases of provinces by country:
        df_country = df_country %>%
                         dplyr::group_by(date) %>% 
                         summarise(country = country[1],
                                   lat = lat[1],
                                   long = long[1],
                                   cases_per_day = sum(cases_per_day),
                                   deaths_per_day = sum(deaths_per_day),
                                   recovered_per_day = sum(recovered_per_day)) %>%
                         as.data.frame(stringsAsFactor = FALSE)
    
        # Cumulative:
        df_country$cases_cumulative = cumsum(df_country$cases_per_day)
        df_country$deaths_cumulative = cumsum(df_country$deaths_per_day)
        df_country$recovered_cumulative = cumsum(df_country$recovered_per_day)
        df = df_country
        
    } else{
        if(country_chosen == "World"){
            # Global numbers:
            df_world = df_coronavirus %>%
                           dplyr::group_by(date) %>%
                           summarise(cases_per_day = sum(cases_per_day),
                                     deaths_per_day = sum(deaths_per_day),
                                     recovered_per_day = sum(recovered_per_day)) %>%
                           as.data.frame(stringsAsFactor = FALSE)
                           
            # Cumulative:
            df_world$cases_cumulative = cumsum(df_world$cases_per_day)
            df_world$deaths_cumulative = cumsum(df_world$deaths_per_day)
            df_world$recovered_cumulative = cumsum(df_world$recovered_per_day)
            df = df_world
        } else{
            # All countries separately with most recent data:
            df_all = NULL
            countries = unique(df_coronavirus$country)
            for(i in 1:length(countries)){
                df_contry_i = df_coronavirus %>%
                                  dplyr::filter(country == countries[i]) %>%
                                  dplyr::group_by(date) %>%
                                  summarise(country = country[1],
                                            cases_per_day = sum(cases_per_day),
                                            deaths_per_day = sum(deaths_per_day),
                                            recovered_per_day = sum(recovered_per_day)) %>%
                                  as.data.frame()
                df_contry_i$cases_cumulative = cumsum(df_contry_i$cases_per_day)
                df_contry_i$deaths_cumulative = cumsum(df_contry_i$deaths_per_day)
                df_contry_i$recovered_cumulative = cumsum(df_contry_i$recovered_per_day)            
                df_all = rbind(df_all, 
                               df_contry_i[nrow(df_contry_i), ])
            }
            
            # Add the population estimated for 2020:
            # Source: https://www.worldometers.info/geography/how-many-countries-are-there-in-the-world/
            path_data =  "data_offline/"
            df_pop = read.csv(paste0(path_data, "countries_pop_2020.csv"),
                              sep = ";",
                              stringsAsFactors = FALSE)
            df_all = df_all %>%
                         dplyr::left_join(df_pop,
                                          by = c("country" = "country"))
            
            # Per million of the population:
            df_all$cases_cumulative_pop = df_all$cases_cumulative/df_all$population_2020*1E6
            df_all$deaths_cumulative_pop = df_all$deaths_cumulative/df_all$population_2020*1E6
            df_all$recovered_cumulative_pop = df_all$recovered_cumulative/df_all$population_2020*1E6
            df = df_all
        }
    }
    
    return(df)
}


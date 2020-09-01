
### Function to define maps UI

ui_world_maps = function(df_data,
                         data_source,
                         countries_names){
    
    fluidRow(style = "margin-left: 5px;
                      margin-right: 5px;",
        
        # General info:
        fluidRow(
            box(background = "aqua",
                solidHeader = FALSE,
                width = 12,
                collapsible = FALSE,
                fluidRow(
                    column(12, 
                        h2(style = "font-weight: 700;",
                            "World map of Covid-19 pandemic"
                        ),
                        h3(paste0("Updated in ", format(df_data$date[nrow(df_data)],
                                                            format = "%d/%m/%Y")))
                    ),
                    column(12,
                        tags$div(style = "font-size: large;",
                            HTML(paste0("Data source: ", 
                                        tags$span(a(href = data_source, data_source))))
                        )
                    )
                )
            )
        ),
        
        # General map:
        fluidRow(
            box(solidHeader = FALSE,
                width = 12,
                collapsible = FALSE,
                fluidRow(
                    column(9,
                        leafletOutput("map_generalized",
                                      height = "600") %>%
                            withSpinner(type = 4,
                                        proxy.height = "200px")
                    ),
                    column(3,
                        radioButtons(inputId = "cdr_cdrpop",
                                     label = NULL,
                                     choices = c("Cases per Day",
                                                 "Deaths per Day",
                                                 "Recovered per Day",
                                                 "Cases Cumulative",
                                                 "Deaths Cumulative",
                                                 "Recovered Cumulative",
                                                 "Cases Cumulative per Population",
                                                 "Deaths Cumulative per Population",
                                                 "Recovered Cumulative per Population",
                                                 "Population 2020"),
                                     selected = "Cases per Day",
                                     width = "100%",
                                     inline = FALSE)
                    )
                )
            )
        )
    )
    
}



